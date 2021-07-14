(in-package #:cl-wasm-runtime)

(cffi:define-foreign-library libwasmer
  (:darwin (:or "shared/libwasmer/lib/libwasmer.dylib")))

(cffi:use-foreign-library libwasmer)

;;; UTIL

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun make-object-type-name-sym (name)
    (alexandria:symbolicate 'wasm- name '-type))

  (defun make-object-type-parser-sym (name)
    (alexandria:symbolicate '%wasm- name '-type))
 
 
  (defun make-cfun-name (name cfunc-name-suffix)
    (let* ((cfunc-name (format nil
			       "wasm_~{~a~^_~}_~a"
			       (split-sequence:split-sequence #\- (string-downcase (string name)))
			       cfunc-name-suffix))
	   (func-name (alexandria:format-symbol *package*
						"%WASM-~a-~{~:@(~a~)~^-~}"
						name
						(split-sequence:split-sequence #\_ cfunc-name-suffix))))
      (list cfunc-name func-name))))

;;; COMMON

(cffi:defctype %size-type :uint)

(cffi:defctype %float-32-type :float)

(cffi:defctype %float-64-type :double)

(defmacro define-wasm-object-type (name &optional slots)
  "Defines a CFFI foreign type named WASM-{name}-TYPE with parser %WASM-{name}-TYPE"
  (let ((object-name-sym (make-object-type-name-sym name))
	 (parser-sym (make-object-type-parser-sym name)))
    `(cffi:define-foreign-type ,object-name-sym (wasm-object-type)
       ,slots
       (:simple-parser ,parser-sym))))

(defmacro define-wasm-vec (name)
  (let* ((vec-name (make-symbol (format nil "~A-VEC" name)))
	 (type-sym (make-object-type-parser-sym vec-name))
	 (struct-type-sym (alexandria:symbolicate '%wasm- vec-name '-struct))
	 (new-empty-name (make-cfun-name vec-name "new_empty"))
	 (new-uninitialized-name (make-cfun-name vec-name "new_uninitialized"))
	 (new-name (make-cfun-name vec-name "new"))
	 (copy-name (make-cfun-name vec-name "copy"))
	 (delete-name (make-cfun-name vec-name "delete")))
    `(progn
       ;; WASM object type
       (define-wasm-object-type ,vec-name)
       ;; Struct
       (cffi:defcstruct ,struct-type-sym
	 (size %size-type)
	 (data :pointer))
       ;; ...-VEC-NEW-EMPTY function
       (cffi:defcfun ,new-empty-name :void
	 (out ,type-sym))
       ;; ...-VEC-NEW-UNINITIALIZED function
       (cffi:defcfun ,new-uninitialized-name :void
	 (out ,type-sym)
	 (size %size-type))
       ;; ...-VEC-NEW function
       (cffi:defcfun ,new-name :void
	 (out ,type-sym)
	 (size %size-type)
	 (init :pointer))
       ;; ...-VEC-COPY function
       (cffi:defcfun ,copy-name :void
	 (out ,type-sym)
	 (src ,type-sym))
       ;; ...-VEC-DELETE function
       (cffi:defcfun ,delete-name :void
	 (vec ,type-sym)))))

(defun wasm-vec-init-empty (wasm-vec vec-struct-type)
  (setf (cffi:foreign-slot-value wasm-vec vec-struct-type 'size)
	0
	(cffi:foreign-slot-value wasm-vec vec-struct-type 'data)
	(cffi:null-pointer)))

(defun wasm-vec-init (wasm-vec vec-struct-type c-array size)
  (setf (cffi:foreign-slot-value wasm-vec vec-struct-type 'size)
	size
	(cffi:foreign-slot-value wasm-vec vec-struct-type 'data)
	c-array))

(defun wasm-vec-size (pointer type)
  (cffi:foreign-slot-value pointer type 'size))

(defun wasm-vec-aref (pointer vec-type offset)
  (cffi:mem-aref (cffi:foreign-slot-value pointer vec-type 'data)
		 :pointer
		 offset))

(defun wasm-vec-aptr (pointer vec-type offset)
  (cffi:mem-aptr (cffi:foreign-slot-value pointer vec-type 'data)
		 :pointer
		 offset))


(defmacro do-wasm-vec (((elm-var elm-type &optional (index-var (gensym))) vec-pointer vec-type) &body body)
  (alexandria:with-gensyms (data size)
    `(let ((,data (cffi:foreign-slot-value ,vec-pointer ,vec-type 'data))
	   (,size (cffi:foreign-slot-value ,vec-pointer ,vec-type 'size)))
       (dotimes (,index-var ,size)
	 (let ((,elm-var (cffi:mem-aref ,data ,elm-type ,index-var)))
	   ,@body)))))

(defmacro define-wasm-own (name)
  (let* ((parser-sym (make-object-type-parser-sym name))
	 (delete-name (make-cfun-name name "delete")))
    `(progn
       (define-wasm-object-type ,name)
       (cffi:defcfun ,delete-name :void
	 (,name ,parser-sym)))))

(defmacro define-wasm-type (name)
  (let ((type-sym (make-object-type-parser-sym name))
	(copy-name (make-cfun-name name "copy")))
    `(progn
       (define-wasm-own ,name)
       (define-wasm-vec ,name)
       (cffi:defcfun ,copy-name ,type-sym
	 (,name ,type-sym)))))

(defmacro define-wasm-object-class (name &optional supers slots &rest options)
  (let ((class-name (alexandria:symbolicate 'wasm- name))
	(delete-name (second (make-cfun-name name "delete"))))
    `(defclass ,class-name
	 (,@supers ,@(unless (find-if (lambda (super) (subtypep super 'wasm-object))
				      supers)
			'(wasm-object)))
       (,@slots
	,@(unless (or (find 'delete-function slots)
		      (assoc 'delete-function slots))
	    `((delete-function :initform (symbol-function ',delete-name)))))
       ,@options)))	 

;;; Wasm vector high level interface

(defun make-wasm-vec-instance (class-name type &key owner)
  (enable-gc (make-instance class-name
			    :pointer (cffi:foreign-alloc type)
			    :owner owner)))

(defun wrap-wasm-vec (class-name pointer &key owner)
  (enable-gc (make-instance class-name
			    :pointer pointer
			    :owner owner)))

(defmacro define-wasm-vec-class (name)
  (let* ((vec-name (make-symbol (format nil "~A-VEC" name)))
	 (struct-type-sym (alexandria:symbolicate '%wasm- vec-name '-struct))
	 (class-name (alexandria:symbolicate 'wasm- vec-name))
	 (make-empty-name (alexandria:symbolicate 'make-wasm- vec-name '-empty))
	 (new-empty-name (second (make-cfun-name vec-name "new_empty")))
	 (make-uninitialized-name (alexandria:symbolicate 'make-wasm- vec-name '-uninitialized))
	 (new-uninitialized-name (second (make-cfun-name vec-name "new_uninitialized")))
	 (make-name (alexandria:symbolicate 'make-wasm- vec-name))
	 (new-name (second (make-cfun-name vec-name "new")))
	 (copy-name (second (make-cfun-name vec-name "copy"))))
    `(progn
       (define-wasm-object-class ,vec-name)
       (defun ,make-empty-name (&key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,new-empty-name vec)
	   vec))
       (defun ,make-uninitialized-name (size &key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,new-uninitialized-name vec size)
	   vec))
       (defun ,make-name (size init-data &key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,new-name vec size init-data)
	   vec))
       (defun ,(alexandria:symbolicate 'wrap-wasm- vec-name) (pointer &key owner)
	 (wrap-wasm-vec ',class-name pointer :owner owner))
       (defun ,(alexandria:symbolicate 'wasm- vec-name '-copy) (src &key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,copy-name vec src)
	   vec))
       (defun ,(alexandria:symbolicate 'wasm- vec-name '-size) (vec)
	 (wasm-vec-size (pointer vec) '(:struct ,struct-type-sym))))))

(defun wasm-vec-to-list (vec vec-type wrap-function &key owner)
  (let ((list nil))
    (do-wasm-vec ((elm-pointer :pointer)
		  (if (cffi:pointerp vec) vec (pointer vec))
		  vec-type)
      (push (funcall wrap-function elm-pointer :owner owner)
	    list))
    (nreverse list)))
			   
;;; Byte vectors

(cffi:defctype %wasm-byte-type :uint8)

(define-wasm-vec byte)

(define-wasm-object-class byte)
(define-wasm-vec-class byte)

(cffi:defctype %wasm-message-struct (:struct %wasm-byte-vec-struct)) ; Null terminated

(cffi:define-foreign-type wasm-message-type (wasm-byte-vec-type)
  ()
  (:simple-parser %wasm-message-type))

(cffi:defctype %wasm-name-struct %wasm-message-struct)

(cffi:define-foreign-type wasm-name-type (wasm-message-type)
  ()
  (:simple-parser %wasm-name-type))

(setf (symbol-function '%wasm-name-new) #'%wasm-byte-vec-new)
(setf (symbol-function '%wasm-name-new-empty) #'%wasm-byte-vec-new-empty)
(setf (symbol-function '%wasm-name-new-uninitialized) #'%wasm-byte-vec-new-uninitialized)
(setf (symbol-function '%wasm-name-copy) #'%wasm-byte-vec-copy)
(setf (symbol-function '%wasm-name-delete) #'%wasm-byte-vec-delete)

(defun octets-to-wasm-byte-vec (octets &key null-terminated owner)
  (let* ((size (length octets))
	 (byte-vec (make-wasm-byte-vec-uninitialized (+ size
							(if null-terminated 1 0)))))
    (cffi:with-foreign-slots ((data) (pointer byte-vec) (:struct %wasm-byte-vec-struct))
      (fast-io:with-fast-input (buffer octets)
	(loop for i below size
	      do (setf (cffi:mem-aref data '%wasm-byte-type i)
		       (fast-io:fast-read-byte buffer))
	      finally (when null-terminated
			(setf (cffi:mem-aref data '%wasm-byte-type (1+ i)) 0))))
      (setf (owner byte-vec) owner)
      byte-vec)))

(defun wasm-byte-vec-to-octets (byte-vec &key null-terminated)
  (cffi:with-foreign-slots ((size data)
			    (if (cffi:pointerp byte-vec) byte-vec (pointer byte-vec))
			    (:struct %wasm-byte-vec-struct))
    (fast-io:with-fast-output (buffer :vector)
      (loop for i below (- size (if null-terminated 1 0))
	    for byte = (cffi:mem-aref data '%wasm-byte-type i)
	    do (fast-io:fast-write-byte byte buffer)))))

(defun string-to-wasm-byte-vec (str &key null-terminated owner)
  (cffi:with-foreign-string (c-str str)
    (make-wasm-byte-vec (+ (babel:string-size-in-octets str)
			   (if null-terminated 1 0))
			c-str
			:owner owner)))


(defun wasm-byte-vec-to-string (byte-vec &key null-terminated)
  (babel:octets-to-string (wasm-byte-vec-to-octets byte-vec :null-terminated null-terminated)))
    
(defmethod cffi:translate-to-foreign ((str string) (type wasm-byte-vec-type))
  (pointer (string-to-wasm-byte-vec str)))

(defmethod cffi:translate-to-foreign ((str string) (type wasm-message-type))
  (pointer (string-to-wasm-byte-vec str :null-terminated t)))

(defmethod cffi:translate-to-foreign ((octets simple-array) (type wasm-byte-vec-type))
  (pointer (octets-to-wasm-byte-vec octets)))

(defmethod cffi:translate-to-foreign ((octets simple-array) (type wasm-message-type))
  (pointer (octets-to-wasm-byte-vec octets :null-terminated t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Environment

;;; Configuration

(define-wasm-own config)

(cffi:defcfun ("wasm_config_new" %wasm-config-new) %wasm-config-type) ;own

(define-wasm-object-class config)

(defun make-wasm-config ()
  (enable-gc (make-instance 'wasm-config :pointer (%wasm-config-new))))

;;; Engine

(define-wasm-own engine)

(cffi:defcfun ("wasm_engine_new" %wasm-engine-new) %wasm-engine-type) ; own

(cffi:defcfun ("wasm_engine_new_with_config" %wasm-engine-new-with-config) %wasm-engine-type ; own
  (config %wasm-config-type)) ; own

(define-wasm-object-class engine)

(defun make-wasm-engine (&optional config)
  (let ((engine (enable-gc (make-instance 'wasm-engine
					  :pointer (if config
						       (%wasm-engine-new-with-config config)
						       (%wasm-engine-new))))))
    (when config
      (setf (owner config) engine))
    engine))

;;; WASM STORE

(define-wasm-object-type store)
(define-wasm-own store)

(cffi:defcfun ("wasm_store_new" %wasm-store-new) %wasm-store-type ;own
  (engine %wasm-engine-type))

(define-wasm-object-class store)

(defun make-wasm-store (engine)
  (let ((store-ptr (%wasm-store-new engine)))
    (enable-gc (make-instance 'wasm-store
			      :pointer store-ptr
			      :parent engine))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Representation

;;; Type attributes

(cffi:defctype %wasm-mutability-type :uint8)

(cffi:defcenum %wasm-mutability-enum
  :wasm-const
  :wasm-var)

(cffi:defcstruct %wasm-limits-type
  (min :uint32)
  (max :uint32))

;;; Value Types

(define-wasm-type valtype)

(cffi:defctype %wasm-val-kind-type :uint8)

(cffi:defcenum %wasm-val-kind-enum
  :wasm-i32
  :wasm-i64
  :wasm-f32
  :wasm-f64
  (:wasm-any-ref 128)
  :wasm-func-ref)

(defun key-to-wasm-valkind (key)
  (cffi:foreign-enum-value '%wasm-val-kind-enum key))

(defun wasm-valkind-to-key (valkind)
  (cffi:foreign-enum-keyword '%wasm-val-kind-enum valkind))

(cffi:defcfun ("wasm_valtype_new" %wasm-valtype-new) %wasm-valtype-type ; own
  (valkind %wasm-val-kind-type))

(cffi:defcfun ("wasm_valtype_kind" %wasm-valtype-kind) %wasm-val-kind-type
  (valtype %wasm-valtype-type))

(define-wasm-object-class valtype ()
  ((kind :initarg kind
	 :reader kind)))

(defun make-wasm-valtype (kind-key)
  (enable-gc (make-instance 'wasm-valtype
			    :pointer (%wasm-valtype-new (key-to-wasm-valkind kind-key))
			    :owner owner
			    :kind kind-key)))

(defun wrap-wasm-valtype (pointer &key owner)
  (enable-gc (make-instance 'wasm-valtype
			    :pointer pointer
			    :owner owner
			    :kind (wasm-valkind-to-key (%wasm-valtype-kind pointer)))))

(define-wasm-vec-class valtype)

;;; Function Types

(define-wasm-type functype)

(cffi:defcfun ("wasm_functype_new" %wasm-functype-new) %wasm-functype-type ; own
  (params %wasm-valtype-vec-type) ; own
  (result %wasm-valtype-vec-type)) ; own

(cffi:defcfun ("wasm_functype_params" %wasm-functype-params) %wasm-valtype-vec-type ; const
  (functype %wasm-functype-type))

(cffi:defcfun ("wasm_functype_results" %wasm-functype-results) %wasm-valtype-vec-type ; const
  (functype %wasm-functype-type))

(define-wasm-object-class functype)

(defun make-wasm-functype (params results)
  (let ((functype (enable-gc (make-instance 'wasm-functype
					    :pointer (%wasm-functype-new params results)))))
    (setf (owner params) functype
	  (owner results) functype)
    functype))

(defun wrap-wasm-functype (pointer &key owner)
  (enable-gc (make-instance 'wasm-functype
			    :pointer pointer
			    :owner owner)))

(defun wasm-functype-params (functype)
  (wrap-wasm-valtype-vec (%wasm-functype-params functype) :owner (owner functype)))

(defun wasm-functype-results (functype)
  (wrap-wasm-valtype-vec (%wasm-functype-results functype) :owner (owner functype)))

;;; Global Types

(define-wasm-type globaltype)

(cffi:defcfun ("wasm_globaltype_new" %wasm-globaltype-new) %wasm-globaltype-type ; own
  (valtype %wasm-valtype-type) ; own
  (mutability %wasm-mutability-type))

(cffi:defcfun ("wasm_globaltype_content" %wasm-globaltype-content) %wasm-valtype-type ; const
  (globaltype %wasm-globaltype-type))

(cffi:defcfun ("wasm_global_type_mutability" %wasm-globaltype-mutability) %wasm-mutability-type ; const
  (globaltype %wasm-globaltype-type))

(define-wasm-object-class globaltype)

(defun make-wasm-globaltype (valtype mutability)
  (let ((globaltype (enable-gc (make-instance 'wasm-globaltype
					      :pointer (%wasm-globaltype-new valtype mutability)))))
    (setf (owner valtype) globaltype)
    globaltype))

;;; Table Types

(define-wasm-type tabletype)

(cffi:defcfun ("wasm_tabletype_new" %wasm-tabletype-new) %wasm-tabletype-type ; own
  (elements %wasm-valtype-type) ; own
  (limits (:pointer (:struct %wasm-limits-type))))

(cffi:defcfun ("wasm_tabletype_element" %wasm-tabletype-element) %wasm-valtype-type ; const
  (tabletype %wasm-tabletype-type))

(cffi:defcfun ("wasm_tabletype_limits" %wasm-tabletype-limits) (:pointer (:struct %wasm-limits-type))
  (tabletype %wasm-tabletype-type))

(define-wasm-object-class globaltype)

(defun make-wasm-tabletype (elements limits)
  (let ((tabletype (enable-gc (make-instance 'wasm-globaltype
					     :pointer (%wasm-tabletype-new valtype limits)))))
    (setf (owner elements) tabletype)
    tabletype))	  

;;; Memory Types

(define-wasm-type memorytype)

(cffi:defcfun ("wasm_memorytype_new" %wasm-memorytype-new) %wasm-memorytype-type ; own
  (limits (:pointer (:struct %wasm-limits-type))))

(cffi:defcfun ("wasm_memorytype_limits" %wasm-memorytype-limits) (:pointer (:struct %wasm-limits-type)) ; const
  (memorytype %wasm-memorytype-type))

(define-wasm-object-class memorytype)

(defun make-wasm-memorytype (limits)
  (enable-gc (make-instance 'wasm-memorytype
			    :pointer (%wasm-memorytype-new limits))))
     
;;; Extern Types

(cffi:defctype %wasm-extern-kind-type :uint8)

(cffi:defcenum %wasm-extern-kind-enum
  :wasm-extern-func
  :wasm-extern-global
  :wasm-extern-table
  :wasm-extern-memory)

(defun key-to-wasm-externkind (key)
  (cffi:foreign-enum-value '%wasm-extern-kind-enum key))

(defun wasm-externkind-to-key (externkind)
  (cffi:foreign-enum-keyword '%wasm-extern-kind-enum externkind))

(define-wasm-type externtype)

(cffi:defcfun ("wasm_externtype_kind" %wasm-externtype-kind) %wasm-extern-kind-type
  (externtype %wasm-externtype-type))

(defmacro define-wasm-externtype-conversion (type-name)
  (let* ((type-sym (make-object-type-parser-sym type-name))
	 (to-externtype-name (make-cfun-name type-name "as_externtype"))
	 (to-externtype-const-name (make-cfun-name type-name "as_externtype_const"))
	 (from-externtype-name (list (format nil "wasm_externtype_as_~a" (string-downcase (string type-name)))
				     (alexandria:symbolicate '%wasm-externtype-as- type-name)))
	 (from-externtype-const-name (list (format nil "wasm_externtype_as_~a_const" (string-downcase (string type-name)))
					   (alexandria:symbolicate '%wasm-externtype-as- type-name '-const))))
    `(progn
       (cffi:defcfun ,to-externtype-name %wasm-externtype-type
	 (,type-name ,type-sym))
       (cffi:defcfun ,to-externtype-const-name %wasm-externtype-type
	 (,type-name ,type-sym))
       (cffi:defcfun ,from-externtype-name ,type-sym
	 (externtype %wasm-externtype-type))
       (cffi:defcfun ,from-externtype-const-name ,type-sym
	 (externtype %wasm-externtype-type)))))

(define-wasm-externtype-conversion functype)
(define-wasm-externtype-conversion globaltype)
(define-wasm-externtype-conversion tabletype)
(define-wasm-externtype-conversion memorytype)

(define-wasm-object-class externtype ()
  ((kind :initarg :kind
	 :reader wasm-externtype-kind)))

(defun wrap-wasm-externtype (pointer &key owner)
  (enable-gc (make-instance 'wasm-externtype
			    :pointer pointer
			    :owner owner
			    :kind (wasm-externkind-to-key (%wasm-externtype-kind pointer)))))

;;; Import Types

(define-wasm-type importtype)

(cffi:defcfun ("wasm_importtype_new" %wasm-importtype-new) %wasm-importtype-type ; own
  (module-name %wasm-name-type) ; own
  (name %wasm-name-type) ; own
  (externtype %wasm-externtype-type)) ; own

(cffi:defcfun ("wasm_importtype_module" %wasm-importtype-module) %wasm-name-type ; const
  (importtype %wasm-importtype-type))

(cffi:defcfun ("wasm_importtype_type" %wasm-importtype-type) %wasm-externtype-type ; const
  (importtype %wasm-importtype-type))

(define-wasm-object-class importtype)

(defun make-wasm-importtype (module-name name externtype)
  (let ((importtype (enable-gc (make-instance 'wasm-importtype
					      :pointer (%wasm-importtype-new module name externtype)
					      :parent module))))
    (setf (owner module-name) importtype
	  (owner name) importtype
	  (owner externtype) importtype)
    importtype))

;;; Export Types

(define-wasm-type exporttype)

(cffi:defcfun ("wasm_exporttype_new" %wasm-exporttype-new) %wasm-exporttype-type ; own
  (name %wasm-name-type) ; own
  (externtype %wasm-externtype-type)) ; own

(cffi:defcfun ("wasm_exporttype_name" %wasm-exporttype-name) %wasm-name-type ; const
  (exporttype %wasm-exporttype-type))

(cffi:defcfun ("wasm_exporttype_type" %wasm-exporttype-type) %wasm-externtype-type ; const
  (exporttype %wasm-exporttype-type))

(define-wasm-object-class exporttype ()
  ((name :reader wasm-exporttype-name)
   (externtype :reader wasm-exporttype-externtype)))

(defun make-wasm-exporttype (name externtype)
  (let ((exporttype (enable-gc (make-instance 'wasm-exporttype
					      :pointer (%wasm-exporttype-new name externtype)
					      :name name
					      :externtype externtype))))
    (setf (owner name) exporttype
	  (owner externtype) exporttype)))
					     
    
    

(defun wrap-wasm-exporttype (pointer &key owner)
  (let ((exporttype (make-instance 'wasm-exporttype
				    :pointer pointer
				    :owner owner)))
    (enable-gc exporttype)
    (setf (slot-value exporttype 'name)
	  (wasm-byte-vec-to-string (%wasm-exporttype-name pointer))
	  (slot-value exporttype 'externtype)
	  (wrap-wasm-externtype (%wasm-exporttype-type pointer)
				:owner exporttype))
    exporttype))

(define-wasm-vec-class exporttype)

(defun wasm-exporttype-vec-to-list (exporttype-vec)
  (wasm-vec-to-list exporttype-vec
		    '(:struct %wasm-exporttype-vec-struct)
		    #'wrap-wasm-exporttype
		    :owner exporttype-vec))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Objects

;;; References

(cffi:defcstruct %wasm-ref-struct)

(defmacro define-wasm-ref-base (name)
  (let ((type-sym (make-object-type-parser-sym name))
	(copy-name (make-cfun-name name "copy"))
	(same-name (make-cfun-name name "same"))
	(get-host-info-name (make-cfun-name name "get_host_info"))
	(set-host-info-name (make-cfun-name name "set_host_info"))
	(set-host-info-with-finalizer-name (make-cfun-name name "set_host_info_with_finalizer")))
    `(progn
       (define-wasm-object-type ,name)
       (define-wasm-own ,name)
       (cffi:defcfun ,copy-name %wasm-name-type ; own
	 (,name ,type-sym))
       (cffi:defcfun ,same-name :boolean
	 (x ,type-sym)
	 (y ,type-sym))
       (cffi:defcfun ,get-host-info-name :pointer
	 (,name ,type-sym))
       (cffi:defcfun ,set-host-info-name :void
	 (,name ,type-sym)
	 (info :pointer))
       (cffi:defcfun ,set-host-info-with-finalizer-name :void
	 (,name ,type-sym)
	 (finalizer :pointer)))))
	   
(defmacro define-wasm-ref (name)
  (let ((type-sym (make-object-type-parser-sym name))
	(as-ref-name (make-cfun-name name "as_ref"))
	(ref-as-name (list (format nil "wasm_ref_as_~a" (string-downcase name))
			   (intern (format nil "%WASM-REF-AS-~A" name))))
	(as-ref-const-name (make-cfun-name name "as_ref_const"))
	(ref-as-const-name  (list (format nil "wasm_ref_as_~a_const" (string-downcase name))
				  (intern (format nil "%WASM-REF-AS-~A-CONST" name)))))
    `(progn
       (define-wasm-ref-base ,name)
       (cffi:defcfun ,as-ref-name (:pointer (:struct %wasm-ref-struct))
	 (,name ,type-sym))
       (cffi:defcfun ,ref-as-name ,type-sym
	 (ref %wasm-ref-type))
       (cffi:defcfun ,as-ref-const-name (:pointer (:struct %wasm-ref-struct)) ; const
	 (,name ,type-sym))
       (cffi:defcfun ,ref-as-const-name ,type-sym ; const
	 (ref %wasm-ref-type)))))

(defmacro define-wasm-sharable-ref (name)
  (let* ((shared-name (make-symbol (format nil "SHARED-~A" name)))
	 (type-sym (make-object-type-parser-sym name))
	 (shared-type-sym (make-object-type-parser-sym shared-name))
	 (share-name (make-cfun-name name "share"))
	 (obtain-name (make-cfun-name name "obtain")))
    `(progn
       (define-wasm-ref ,name)
       (define-wasm-own ,shared-name)
       (define-wasm-object-type ,shared-name)
       (cffi:defcfun ,share-name ,shared-type-sym ; own
	 (,name ,type-sym))
       (cffi:defcfun ,obtain-name ,type-sym ; own
	 (store %wasm-store-type)
	 (,name ,shared-type-sym)))))

(define-wasm-ref-base ref)

(define-wasm-object-class ref)

;;; Values

(cffi:defcunion %wasm-val-union
  (i32 :int32)
  (i64 :int64)
  (f32 %float-32-type)
  (f64 %float-64-type)
  (ref (:pointer (:struct %wasm-ref-struct))))

(cffi:defcstruct %wasm-val-struct
  (kind %wasm-val-kind-type)
  (of (:union %wasm-val-union)))

(define-wasm-object-type val)
(define-wasm-vec val)

(cffi:defcfun ("wasm_val_delete" %wasm-val-delete) :void
  (v %wasm-val-type))

(cffi:defcfun ("wasm_val_copy" %wasm-val-copy) :void
  (out %wasm-val-type)
  (in %wasm-val-type))

(defun wasm-val-type-kind-of (kind-key)
  (ecase kind-key
    (:wasm-i32 'i32)
    (:wasm-i64 'i64)
    (:wasm-f32 'f32)
    (:wasm-f64 'f64)
    (:wasm-any-ref 'ref)
    (:wasm-func-ref 'ref)))

(defun wasm-val-type-value (pointer)
  (let* ((kind (cffi:foreign-slot-value pointer '(:struct %wasm-val-struct) 'kind))
	 (kind-key (cffi:foreign-enum-keyword '%wasm-val-kind-enum kind)))
    (cffi:foreign-slot-value (cffi:foreign-slot-value pointer
						      '(:struct %wasm-val-struct)
						      'of)
			     '(:union %wasm-val-union)
			     (wasm-val-type-kind-of kind-key))))

(defun wasm-val-init (wasm-val wasm-val-kind-key val)
  (setf (cffi:foreign-slot-value wasm-val '(:struct %wasm-val-struct) 'kind)
	(cffi:foreign-enum-value '%wasm-val-kind-enum wasm-val-kind-key)
	(cffi:foreign-slot-value (cffi:foreign-slot-value wasm-val
							  '(:struct %wasm-val-struct)
							  'of)
				 '(:union %wasm-val-union)
				 (wasm-val-type-kind-of wasm-val-kind-key))
	val))

(define-wasm-object-class val)

(defun make-wasm-val (kind-key lisp-val &key owner)
  (let* ((pointer (cffi:foreign-alloc '(:struct %wasm-val-struct)))
	 (wasm-val (enable-gc (make-instance 'wasm-val
					     :pointer pointer
					     :owner owner))))
    (wasm-val-init pointer kind-key lisp-val)
    wasm-val))

(defun wasm-val-kind (wasm-val)
  (unless (null? (pointer wasm-val))
    (wasm-valkind-to-key (cffi:foreign-slot-value (pointer wasm-val)
						  '(:struct %wasm-val-struct)
						  'kind))))

(defun wasm-val-value (wasm-val)
  (unless (null? (pointer wasm-val))
    (let ((kind-key (wasm-val-kind wasm-val)))
      (cffi:foreign-slot-value (cffi:foreign-slot-value (pointer wasm-val)
							'(:struct %wasm-val-struct)
							'of)
			       '(:union %wasm-val-union)
			       (wasm-val-type-kind-of kind-key)))))

(defun wrap-wasm-val (pointer &key owner)
  (enable-gc (make-instance 'wasm-val
			    :pointer pointer
			    :owner owner)))
			      

;; TODO: Translate more types
(defun lisp-to-wasm-valkind (lisp-val)
  (etypecase lisp-val
    ;; TODO: Translate CL floats properly
    (single-float :wasm-f32)
    (double-float :wasm-f64)
    ((unsigned-byte 32) :wasm-i32)
    ((unsigned-byte 64) :wasm-i64)))

(defun lisp-to-wasm-val (lisp-val)
  (make-wasm-val (lisp-to-wasm-valkind lisp-val)
		 lisp-val))
  

(define-wasm-vec-class val)

(defun wasm-val-vec-to-list (val-vec)
  (wasm-vec-to-list val-vec
		    '(:struct %wasm-val-vec-struct)
		    #'wrap-wasm-val
		    :owner (owner val-vec)))

(defun list-to-wasm-val-vec (list &key owner)
  (let* ((size (length list)))
    (cffi:with-foreign-pointer (vals size)
      (let ((owned (loop for val in list
			 for i from 0
			 if (cffi:pointerp val)
			   do (setf (cffi:mem-aref vals :pointer i) val)
			 else
			   collect (typecase val
				     (wasm-object
				      (setf (cffi:mem-aref vals :pointer i) (pointer val))
				      val)
				     (t
				      (let ((wasm-obj (lisp-to-wasm-val val)))
					(setf (cffi:mem-aref vals :pointer i) (pointer wasm-obj))
					wasm-obj)))
			 end))
	    (val-vec (enable-gc (make-wasm-val-vec size vals :owner owner))))
	(loop for obj in owned
	      do (setf (owner obj) val-vec))
	val-vec))))
						  
;;; Frames

(define-wasm-own frame)
(define-wasm-vec frame)

(cffi:defcfun ("wasm_frame_copy" %wasm-frame-copy) %wasm-frame-type ; own
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_instance" %wasm-frame-instance) :pointer ; %wasm-instance-type
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_func_index" %wasm-frame-func-instance) :uint32
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_func_offset" %wasm-frame-func-offset) %size-type
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_module_offset" %wasm-frame-module-offset) %size-type
  (frame %wasm-frame-type))

(define-wasm-object-class frame)
(define-wasm-vec-class frame)

;;; Traps

(define-wasm-ref trap)

(cffi:defcfun ("wasm_trap_new" %wasm-trap-new) %wasm-trap-type ; own
  (store %wasm-store-type)
  (message (:pointer %wasm-message-type)))

(cffi:defcfun ("wasm_trap_message" %wasm-trap-message) :void
  (trap %wasm-trap-type)
  (out (:pointer %wasm-message-type)))

(cffi:defcfun ("wasm_trap_origin" %wasm-trap-origin) %wasm-frame-type ; own
  (trap %wasm-trap-type))

(cffi:defcfun ("wasm_trap_trace" %wasm-trap-trace) :void
  (trap %wasm-trap-type)
  (out %wasm-frame-vec-type))

(define-wasm-object-class trap)

;;; Foreign Objects

(define-wasm-ref foreign)

(cffi:defcfun ("wasm_foreign_new" %wasm-foreign-new) %wasm-foreign-type ; own
  (store %wasm-store-type))

(define-wasm-object-class foreign)

;;; Modules

(define-wasm-sharable-ref module)

(cffi:defcfun ("wasm_module_new" %wasm-module-new) %wasm-module-type ; own
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(cffi:defcfun ("wasm_module_validate" %wasm-module-validate) :boolean
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(cffi:defcfun ("wasm_module_imports" %wasm-module-imports) :void
  (module %wasm-module-type)
  (out %wasm-importtype-vec-type))

(cffi:defcfun ("wasm_module_exports" %wasm-module-exports) :void
  (module %wasm-module-type)
  (out %wasm-exporttype-type))

(cffi:defcfun ("wasm_module_serialize" %wasm-module-serialize) :void
  (module %wasm-module-type)
  (out %wasm-byte-vec-type))

(cffi:defcfun ("wasm_module_deserialize" %wasm-module-deserialize) %wasm-module-type
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(defclass wasm-module-exports (wasm-exporttype-vec)
  ((exports-list)))

(defun make-wasm-module-exports (module)
  (let* ((pointer (cffi:foreign-alloc '(:struct %wasm-exporttype-vec-struct)))
	 (exports (make-instance 'wasm-module-exports
				 :pointer pointer
				 :parent module)))
    (%wasm-module-exports module exports)
    (enable-gc exports)
    (setf (slot-value exports 'exports-list)
	  (wasm-exporttype-vec-to-list exports))
    exports))

(define-wasm-object-class module ()
  ((exports)))

(defun make-wasm-module (store binary)
  (let* ((pointer (%wasm-module-new store binary))
	 (module (make-instance 'wasm-module
				:pointer pointer
				:parent store)))
    (enable-gc module)
    (setf (slot-value module 'exports)
	  (make-wasm-module-exports module))
    module))

(defun wasm-module-exports (module)
  (slot-value (slot-value module 'exports)
	      'exports-list))
   
;;; Function Instances

(define-wasm-ref func)

(cffi:defcfun ("wasm_func_new" %wasm-func-new) %wasm-func-type ; own
  (store %wasm-store-type)
  (functype %wasm-functype-type)
  (callback :pointer))

(cffi:defcfun ("wasm_func_new_with_env" %wasm-func-new-with-env) %wasm-func-type ; own
  (store %wasm-store-type)
  (functype %wasm-functype-type)
  (callback :pointer)
  (finalizer :pointer))

(cffi:defcfun ("wasm_func_type" %wasm-func-type) %wasm-functype-type ; own
  (func %wasm-func-type))

(cffi:defcfun ("wasm_func_param_arity" %wasm-func-param-arity) %size-type
  (func %wasm-func-type))

(cffi:defcfun ("wasm_func_result_arity" %wasm-func-result-arity) %size-type
  (func %wasm-func-type))

(cffi:defcfun ("wasm_func_call" %wasm-func-call) %wasm-trap-type
  (func %wasm-func-type)
  (args %wasm-val-vec-type)
  (results %wasm-val-vec-type))

(define-wasm-object-class func)

(defun make-wasm-func (store functype callback &optional finalizer)
  (enable-gc (make-instance 'wasm-func
			    :pointer (if finalizer
					 (%wasm-func-new-with-env store functype callback finalizer)
					 (%wasm-func-new store functype callback))
			    :parent store)))

(defun wrap-wasm-func (pointer &key owner)
  (enable-gc (make-instance 'wasm-func
			    :pointer pointer
			    :owner owner)))

(defun wasm-func-type (func &key owner)
  (wrap-wasm-functype (%wasm-func-type func) :owner owner))

(defun wasm-funcall (func &rest received-args)
  (let* ((functype (wasm-func-type func))
	 (num-received-args (length received-args))
	 (params (wasm-functype-params functype))
	 (num-params (wasm-valtype-vec-size params))
	 (num-results (wasm-valtype-vec-size (wasm-functype-results functype))))
    (when (not (= num-received-args num-params))
      (error (format nil "WASM function called with ~a arguments, but wants exactly ~a" num-received-args num-params)))
    ;; Try to keep as many ephemeral objects in dynamic extent as possible
    (cffi:with-foreign-objects ((arg-arr '(:struct %wasm-val-struct) num-params)
				(args '(:struct %wasm-val-vec-struct))
				(results '(:struct %wasm-val-vec-struct)))
      
      (loop for arg in received-args
	    for i from 0
	    do (wasm-val-init (cffi:mem-aptr arg-arr '(:struct %wasm-val-struct))
			      (cffi:foreign-enum-keyword
			       '%wasm-val-kind-enum
			       (%wasm-valtype-kind (wasm-vec-aref (pointer params)
								  '(:struct %wasm-valtype-vec-struct)
								  i)))
			      arg))
      (unwind-protect
	   (progn
	     (%wasm-val-vec-new args num-params arg-arr)
	     (%wasm-val-vec-new-uninitialized results num-results)
	     (%wasm-func-call func args results)
	     (loop for i below num-results
		   collect (wasm-val-type-value (wasm-vec-aptr results '(:struct %wasm-val-vec-struct) i))
		     into result-values
		   finally (return (values-list result-values))))
	(%wasm-val-vec-delete args)
	(%wasm-val-vec-delete results)))))
      
;;; Global Instances

(define-wasm-ref global)

(cffi:defcfun ("wasm_global_new" %wasm-global-new) %wasm-global-type ; own
  (store %wasm-store-type)
  (globaltype %wasm-globaltype-type)
  (val %wasm-val-type))

(cffi:defcfun ("wasm_global_type" %wasm-global-type) %wasm-globaltype-type ; own
  (global %wasm-global-type))

(cffi:defcfun ("wasm_global_get" wasm-global-get) :void
  (global %wasm-global-type)
  (out %wasm-val-type))

(cffi:defcfun ("wasm_global_set" wasm-global-set) :void
  (global %wasm-global-type)
  (value %wasm-val-type))
	 
(define-wasm-object-class global)

(defun make-wasm-global (store globaltype val)
  (enable-gc (make-instance 'wasm-global
			    :pointer (%wasm-global-new store globaltype val)
			    :parent store)))

;;; Table Instances

(define-wasm-ref table)

(define-wasm-object-class table)

;;; Memory Instances

(define-wasm-ref memory)

(define-wasm-object-class memory)

;;; Externals

(define-wasm-ref extern)
(define-wasm-vec extern)

(cffi:defcfun ("wasm_extern_kind" %wasm-extern-kind) %wasm-extern-kind-type
  (extern %wasm-extern-type))

(cffi:defcfun ("wasm_extern_type" %wasm-extern-type) %wasm-externtype-type ; own
  (extern %wasm-extern-type))

(defmacro define-wasm-extern-conversion (type)
  (let ((type-name (make-object-type-parser-sym type))
	(as-extern-name (make-cfun-name type "_as_extern"))
	(as-type-name (list (format nil "wasm_extern_as_~a" (string-downcase type))
			    (alexandria:symbolicate '%wasm-extern-as- type))))
    `(progn
       (cffi:defcfun ,as-extern-name %wasm-extern-type
	 (,type ,type-name))
       (cffi:defcfun ,as-type-name ,type-name
	 (extern %wasm-extern-type)))))

(define-wasm-extern-conversion func)
(define-wasm-extern-conversion global)
(define-wasm-extern-conversion table)
(define-wasm-extern-conversion memory)

(define-wasm-object-class extern ()
  ((kind :reader wasm-extern-kind)
   (externtype :reader wasm-extern-type)))

(defun wrap-wasm-extern (pointer &key owner)
  (let ((extern (enable-gc (make-instance 'wasm-extern :pointer pointer :owner owner))))
    (setf (slot-value extern 'kind)
	  (wasm-externkind-to-key (%wasm-extern-kind pointer))
	  (slot-value extern 'externtype)
	  (wrap-wasm-externtype (%wasm-extern-type pointer) :owner (owner extern)))
    extern))

(defun wasm-extern-as-func (extern)
  (let ((func-pointer (%wasm-extern-as-func extern)))
    (unless (null? func-pointer)
      (wrap-wasm-func func-pointer :owner (owner extern)))))

(define-wasm-vec-class extern)

(defun wasm-extern-vec-to-list (extern-vec &key owner)
  (wasm-vec-to-list extern-vec
		    '(:struct %wasm-extern-vec-struct)
		    #'wrap-wasm-extern
		    :owner (owner extern-vec)))

;;; Module Instances

(define-wasm-ref instance)

(cffi:defcfun ("wasm_instance_new" %wasm-instance-new) %wasm-instance-type ; own
  (store %wasm-store-type)
  (module %wasm-module-type)
  (imports %wasm-extern-vec-type)
  (traps %wasm-trap-type)) ; own

(cffi:defcfun ("wasm_instance_exports" %wasm-instance-exports) :void
  (instance %wasm-instance-type)
  (out %wasm-extern-vec-type))

(defclass wasm-imports (wasm-extern-vec)
  ())

(defun make-wasm-imports ()
  (let ((extern-vec (make-wasm-vec-instance 'wasm-instance-exports '(:struct %wasm-extern-vec-struct))))
    (%wasm-extern-vec-new-empty extern-vec)
    extern-vec))

(defclass wasm-instance-exports (wasm-extern-vec)
  ((export-alist :reader wasm-instance-exports-to-alist)))

(defun make-wasm-instance-exports (instance)
  (let* ((module (wasm-instance-module instance))
	 (pointer (cffi:foreign-alloc '(:struct %wasm-extern-vec-struct)))
	 (exports (make-instance 'wasm-instance-exports
				 :pointer pointer
				 :parent instance)))
    (%wasm-instance-exports instance exports)
    (enable-gc exports)
    (setf (slot-value exports 'export-alist)
	  (loop for extern in (wasm-extern-vec-to-list exports)
		for module-export in (wasm-module-exports module)
		collect (cons (wasm-exporttype-name module-export)
			      extern)))
    
    exports))

(defun wasm-instance-exports-func (name exports)
  (let* ((extern (cdr (assoc name (wasm-instance-exports-to-alist exports) :test #'string=))))
    (unless (null? extern)
      (wasm-extern-as-func extern))))

    
(define-wasm-object-class instance ()
  ((module :initarg :module
	   :reader wasm-instance-module)
   (exports :reader wasm-instance-exports)))

(defun make-wasm-instance (store module imports &optional traps)
  (let ((instance (enable-gc
		   (make-instance 'wasm-instance
				  :pointer (%wasm-instance-new store
							       module
							       imports
							       (if (null? traps) (cffi:null-pointer) traps))
				  :parent store
				  :module module))))
    (when traps
      (setf (owner traps) instance))
    (setf (slot-value instance 'exports)
	  (make-wasm-instance-exports instance))
    instance))
	
;;; WAT

(cffi:defcfun ("wat2wasm" %wat-to-wasm) :void
  (wat %wasm-byte-vec-type)
  (out %wasm-byte-vec-type))

(defun wat-to-wasm (store wat-str)
  (cffi:with-foreign-object (wasm-bytes '%wasm-byte-vec-type)
    (unwind-protect (progn
		      (%wat-to-wasm wat-str wasm-bytes)
		      (make-wasm-module store wasm-bytes))
      (%wasm-byte-vec-delete wasm-bytes))))

;;; High-level interface

(defun read-wasm-module (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bin (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bin in)
      bin)))

(defun binary-to-wasm-module (store bin-array)
  (let ((size (length bin-array)))
    (cffi:with-foreign-objects ((wasm-byte-arr '%wasm-byte-type size)
				(wasm-bytes '(:struct %wasm-byte-vec-struct)))
      (unwind-protect
	   (progn
	     (loop for i below size
		   do (setf (cffi:mem-aref wasm-byte-arr '%wasm-byte-type i)
			    (aref bin-array i)))
	     (%wasm-byte-vec-new wasm-bytes size wasm-byte-arr)
	     (make-wasm-module store wasm-bytes))
	(%wasm-byte-vec-delete wasm-bytes)))))
