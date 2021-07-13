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

(defmacro do-wasm-vec (((elm-var elm-type &optional (index-var (gensym))) vec-pointer vec-type) &body body)
  (alexandria:with-gensyms (data size)
    `(let ((,data (cffi:foreign-slot-value ,vec-pointer ,vec-type 'data))
	   (,size (cffi:foreign-slot-value ,vec-pointer ,vec-type 'size)))
       (dotimes (,index-var ,size)
	 (let ((,elm-var (cffi:mem-aref ,data ,elm-type ,index-var)))
	   ,@body)))))

(defmacro define-wasm-own (name)
  (let* ((type-sym (make-object-type-parser-sym name))
	 (delete-name (make-cfun-name name "delete")))
    `(progn
       (define-wasm-object-type ,name)
       (cffi:defcfun ,delete-name :void
	 (,name ,type-sym)))))

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

(defun make-wasm-vec-instance (class-name type)
  (enable-gc (make-instance class-name :pointer (cffi:foreign-alloc type))))

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
       (defun ,make-empty-name ()
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym))))
	   (,new-empty-name vec)
	   vec))
       (defun ,make-uninitialized-name (size)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym))))
	   (,new-uninitialized-name vec size)
	   vec))
       (defun ,make-name (size init-data)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym))))
	   (,new-name vec size init-data)
	   vec))
       (defun ,(alexandria:symbolicate 'wasm- vec-name '-copy) (src)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym))))
	   (,copy-name vec src)
	   vec))
       (defun ,(alexandria:symbolicate 'wasm- vec-name '-size) (vec)
	 (wasm-vec-size (pointer vec) '(:struct ,struct-type-sym))))))
			   
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

(defun octets-to-wasm-byte-vec (octets &key null-terminated)
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
	
      byte-vec)))

(defun wasm-byte-vec-to-octets (byte-vec &key null-terminated)
  (cffi:with-foreign-slots ((size data)
			    (if (cffi:pointerp byte-vec) byte-vec (pointer byte-vec))
			    (:struct %wasm-byte-vec-struct))
    (fast-io:with-fast-output (buffer :vector)
      (loop for i below (- size (if null-terminated 1 0))
	    for byte = (cffi:mem-aref data '%wasm-byte-type i)
	    do (fast-io:fast-write-byte byte buffer)))))

(defun string-to-wasm-byte-vec (str &key null-terminated)
  (cffi:with-foreign-string (c-str str)
    (make-wasm-byte-vec (+ (babel:string-size-in-octets str)
			   (if null-terminated 1 0))
			c-str)))


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

(cffi:defcfun ("wasm_config_new" %wasm-config-new) :pointer)

(define-wasm-object-class config)

(defun make-wasm-config ()
  (enable-gc (make-instance 'wasm-config :pointer (%wasm-config-new))))

;;; Engine

(define-wasm-own engine)

(cffi:defcfun ("wasm_engine_new" %wasm-engine-new) :pointer)

(cffi:defcfun ("wasm_engine_new_with_config" %wasm-engine-new-with-config) :pointer
  (config %wasm-config-type))

(define-wasm-object-class engine)

(defun make-wasm-engine (&optional config)
  ;; Should config be :parent ?
  (enable-gc (make-instance 'wasm-engine
			    :pointer (if config
			      (%wasm-engine-new-with-config config)
			      (%wasm-engine-new)))))

;;; WASM STORE

(define-wasm-object-type store)
(define-wasm-own store)

(cffi:defcfun ("wasm_store_new" %wasm-store-new) :pointer
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

(cffi:defcfun ("wasm_valtype_new" %wasm-valtype-new) :pointer
  (valkind %wasm-val-kind-type))

(cffi:defcfun ("wasm_valtype_kind" %wasm-valtype-kind) %wasm-val-kind-type
  (valtype %wasm-valtype-type))

(define-wasm-object-class valtype ()
  ((kind :initarg kind
	 :reader kind)))

(defun make-wasm-valtype (kind-key &key owned-by)
  (enable-gc (make-instance 'wasm-valtype
			    :pointer (%wasm-valtype-new (key-to-wasm-valkind kind-key))
			    :parent owned-by
			    :kind kind-key)))

(defun wrap-wasm-valtype (pointer &key owned-by)
  (enable-gc (make-instance 'wasm-valtype
			    :pointer pointer
			    :parent owned-by
			    :kind (wasm-valkind-to-key (%wasm-valtype-kind pointer)))))

(define-wasm-vec-class valtype)

;;; Function Types

(define-wasm-type functype)

(cffi:defcfun ("wasm_functype_new" %wasm-functype-new) :pointer
  (params %wasm-valtype-vec-type)
  (result %wasm-valtype-vec-type))

(cffi:defcfun ("wasm_functype_params" %wasm-functype-params) :pointer
  (functype %wasm-functype-type))

(cffi:defcfun ("wasm_functype_results" %wasm-functype-results) :pointer
  (functype %wasm-functype-type))

(define-wasm-object-class functype)

(defun make-wasm-functype (&optional param-list result-list)
  (cffi:with-foreign-objects ((params '(:struct %wasm-valtype-vec-struct))
			      (results '(:struct %wasm-valtype-vec-struct)))
    (flet ((valtype-vec-new (val-vec val-list)
	     (let ((num-vals (length val-list)))
	       (if (zerop num-vals)
		   (%wasm-valtype-vec-new-empty val-vec)
		   (cffi:with-foreign-object (vals :pointer num-vals)
		     (loop for val in val-list
			   for i from 0
			   do (setf (cffi:mem-aref vals :pointer i) val))
		     (%wasm-valtype-vec-new val-vec num-vals vals))))))
      (valtype-vec-new params param-list)
      (valtype-vec-new results result-list)
      (enable-gc (make-instance 'wasm-functype
				:pointer (%wasm-functype-new params results))))))
  
;;; Global Types

(define-wasm-type globaltype)

(cffi:defcfun ("wasm_globaltype_new" %wasm-globaltype-new) :pointer
  (valtype %wasm-valtype-type)
  (mutability %wasm-mutability-type))

(cffi:defcfun ("wasm_globaltype_content" %wasm-globaltype-content) :pointer
  (globaltype %wasm-globaltype-type))

(cffi:defcfun ("wasm_global_type_mutability" %wasm-globaltype-mutability) %wasm-mutability-type
  (globaltype %wasm-globaltype-type))

(define-wasm-object-class globaltype)

(defun make-wasm-globaltype (valtype mutability)
  (enable-gc (make-instance 'wasm-globaltype :pointer (%wasm-globaltype-new valtype mutability))))

;;; Table Types

(define-wasm-type tabletype)

(cffi:defcfun ("wasm_tabletype_new" %wasm-tabletype-new) :pointer
  (valtype %wasm-valtype-type)
  (limits (:pointer (:struct %wasm-limits-type))))

(cffi:defcfun ("wasm_tabletype_element" %wasm-tabletype-element) :pointer
  (tabletype %wasm-tabletype-type))

(cffi:defcfun ("wasm_tabletype_limits" %wasm-tabletype-limits) (:pointer (:struct %wasm-limits-type))
  (tabletype %wasm-tabletype-type))

(define-wasm-object-class globaltype)

(defun make-wasm-tabletype (valtype limits)
  (enable-gc (make-instance 'wasm-globaltype :pointer (%wasm-tabletype-new valtype limits))))

;;; Memory Types
(define-wasm-type memorytype)

(cffi:defcfun ("wasm_memorytype_new" %wasm-memorytype-new) :pointer
  (limits (:pointer (:struct %wasm-limits-type))))

(cffi:defcfun ("wasm_memorytype_limits" %wasm-memorytype-limits) (:pointer (:struct %wasm-limits-type))
  (memorytype %wasm-memorytype-type))

(define-wasm-object-class memorytype)

(defun make-wasm-memorytype (limits)
  (enable-gc (make-instance 'wasm-memorytype :pointer (%wasm-memorytype-new limits))))

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
       (cffi:defcfun ,to-externtype-name :pointer
	 (,type-name ,type-sym))
       (cffi:defcfun ,to-externtype-const-name :pointer
	 (,type-name ,type-sym))
       (cffi:defcfun ,from-externtype-name :pointer
	 (externtype %wasm-externtype-type))
       (cffi:defcfun ,from-externtype-const-name :pointer
	 (externtype %wasm-externtype-type)))))

(define-wasm-externtype-conversion functype)
(define-wasm-externtype-conversion globaltype)
(define-wasm-externtype-conversion tabletype)
(define-wasm-externtype-conversion memorytype)

(define-wasm-object-class externtype ()
  ((kind :initarg :kind
	 :reader wasm-externtype-kind)))

(defun wrap-wasm-externtype (pointer &key owned-by)
  (enable-gc (make-instance 'wasm-externtype
			    :pointer pointer
			    :parent owned-by
			    :kind (wasm-externkind-to-key (%wasm-externtype-kind pointer)))))

;;; Import Types

(define-wasm-type importtype)

(cffi:defcfun ("wasm_importtype_new" %wasm-importtype-new) :pointer
  (module (:pointer %wasm-name-type))
  (name (:pointer %wasm-name-type))
  (externtype %wasm-externtype-type))

(cffi:defcfun ("wasm_importtype_module" %wasm-importtype-module) (:pointer %wasm-name-type)
  (importtype %wasm-importtype-type))

(cffi:defcfun ("wasm_importtype_type" %wasm-importtype-type) :pointer
  (importtype %wasm-importtype-type))

(define-wasm-object-class importtype)

(defun make-wasm-importtype (module name externtype)
  (enable-gc (make-instance 'wasm-importtype
			    :pointer (%wasm-importtype-new module name externtype)
			    :parent module)))

;;; Export Types

(define-wasm-type exporttype)

(cffi:defcfun ("wasm_exporttype_new" %wasm-exporttype-new) :pointer
  (name %wasm-name-type)
  (externtype %wasm-externtype-type))

(cffi:defcfun ("wasm_exporttype_name" %wasm-exporttype-name) :pointer
  (exporttype %wasm-exporttype-type))

(cffi:defcfun ("wasm_exporttype_type" %wasm-exporttype-type) :pointer
  (exporttype %wasm-exporttype-type))

(define-wasm-object-class exporttype ()
  ((name :reader wasm-exporttype-name)
   (externtype :reader wasm-exporttype-externtype)))

(defun make-wasm-exporttype (name externtype &key owned-by)
  (enable-gc (make-instance 'wasm-exporttype
			    :pointer (%wasm-exporttype-new name externtype)
			    :name name
			    :externtype externtype
			    :parent owned-by)))

(defun wrap-wasm-exporttype (pointer &key owned-by)
  (let ((exporttype (make-instance 'wasm-exporttype
				    :pointer pointer
				    :parent owned-by)))
    (enable-gc exporttype)
    (setf (slot-value exporttype 'name)
	  (wasm-byte-vec-to-string (%wasm-exporttype-name pointer))
	  (slot-value exporttype 'externtype)
	  (wrap-wasm-externtype (%wasm-exporttype-type pointer)
				:owned-by exporttype))
    exporttype))

(define-wasm-vec-class exporttype)

(defun wasm-exporttype-vec-to-list (exporttype-vec &key owned-by)
  (let ((exporttype-list nil))
    (do-wasm-vec ((exporttype-pointer :pointer) (pointer exporttype-vec) '(:struct %wasm-exporttype-vec-struct))
      (push (wrap-wasm-exporttype exporttype-pointer
				  :owned-by (or owned-by exporttype-vec))
	    exporttype-list))
    exporttype-list))
    
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
       (cffi:defcfun ,copy-name :pointer
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
       (cffi:defcfun ,ref-as-name :pointer
	 (ref %wasm-ref-type))
       (cffi:defcfun ,as-ref-const-name (:pointer (:struct %wasm-ref-struct))
	 (,name ,type-sym))
       (cffi:defcfun ,ref-as-const-name :pointer
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
       (cffi:defcfun ,share-name :pointer
	 (,name ,type-sym))
       (cffi:defcfun ,obtain-name :pointer
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

(cffi:defcfun ("wasm_val_delete" %wasm-val-delete) :void
  (v %wasm-val-type))

(cffi:defcfun ("wasm_val_copy" %wasm-val-copy) :void
  (out %wasm-val-type)
  (in %wasm-val-type))

(defclass wasm-val (wasm-object)
  ((delete-function :initform #'%wasm-val-delete)
   (kind :initarg :kind
	 :accessor kind
	 :initform nil)
   (val :initarg :val
	:accessor val
	:initform nil)))

(defun wasm-val-type-kind-of (kind-key)
  (ecase kind-key
    (:wasm-i32 'i32)
    (:wasm-i64 'i64)
    (:wasm-f32 'f32)
    (:wasm-f64 'f64)
    (:wasm-any-ref 'ref)
    (:wasm-func-ref 'ref)))

(defmethod cffi:translate-from-foreign (pointer (type wasm-val-type))
  (let ((kind-key (cffi:foreign-enum-keyword '%wasm-val-kind-enum
					     (cffi:foreign-slot-value pointer
								      '(:struct %wasm-val-struct)
								      'kind))))
    (enable-gc (make-instance 'wasm-val
			      :pointer pointer
			      :kind kind-key
			      :val (cffi:foreign-slot-value (cffi:foreign-slot-value pointer
										     '(:struct %wasm-val-struct)
										     'of)
							    '(:union %wasm-val-union)
							    (wasm-val-type-kind-of kind-key))))))

(defun wasm-val-init (wasm-val wasm-val-kind-key val)
  (setf (cffi:foreign-slot-value wasm-val '(:struct %wasm-val-struct) 'kind)
	(cffi:foreign-enum-value '%wasm-val-kind-enum wasm-val-kind-key)
	(cffi:foreign-slot-value (cffi:foreign-slot-value wasm-val
							  '(:struct %wasm-val-struct)
							  'of)
				 '(:union %wasm-val-union)
				 (wasm-val-type-kind-of wasm-val-kind-key))
	val))

(define-wasm-vec val)

(define-wasm-object-class val)
(define-wasm-vec-class val)


;;; Frames

(define-wasm-own frame)
(define-wasm-vec frame)

(cffi:defcfun ("wasm_frame_copy" %wasm-frame-copy) :pointer
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_instance" %wasm-frame-instance) :pointer
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

(cffi:defcfun ("wasm_trap_new" %wasm-trap-new) :pointer
  (store %wasm-store-type)
  (message (:pointer %wasm-message-type)))

(cffi:defcfun ("wasm_trap_message" %wasm-trap-message) :void
  (trap %wasm-trap-type)
  (out (:pointer %wasm-message-type)))

(cffi:defcfun ("wasm_trap_origin" %wasm-trap-origin) :pointer
  (trap %wasm-trap-type))

(cffi:defcfun ("wasm_trap_trace" %wasm-trap-trace) :void
  (trap %wasm-trap-type)
  (out %wasm-frame-vec-type))

(define-wasm-object-class trap)

;;; Foreign Objects

(define-wasm-ref foreign)

(cffi:defcfun ("wasm_foreign_new" %wasm-foreign-new) :pointer
  (store %wasm-store-type))

(define-wasm-object-class foreign)

;;; Modules

(define-wasm-sharable-ref module)

(cffi:defcfun ("wasm_module_new" %wasm-module-new) :pointer
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

(cffi:defcfun ("wasm_module_deserialize" %wasm-module-deserialize) :pointer
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(defclass wasm-module-exports (wasm-exporttype-vec)
  ((exports-list)))

(defun make-wasm-module-exports (module)
  (let* ((pointer (cffi:foreign-alloc '(:struct %wasm-exporttype-vec-struct)))
	 (exports (make-instance 'wasm-module-exports
				 :pointer pointer
				 :parent module)))
    (enable-gc exports)
    (%wasm-module-exports module exports)
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

(cffi:defcfun ("wasm_func_new" %wasm-func-new) :pointer
  (store %wasm-store-type)
  (functype %wasm-functype-type)
  (callback :pointer))

(cffi:defcfun ("wasm_func_new_with_env" %wasm-func-new-with-env) %wasm-func-type
  (store %wasm-store-type)
  (functype %wasm-functype-type)
  (callback :pointer)
  (finalizer :pointer))

(cffi:defcfun ("wasm_func_type" %wasm-func-type) :pointer
  (func %wasm-func-type))

(cffi:defcfun ("wasm_func_param_arity" %wasm-func-param-arity) %size-type
  (func %wasm-func-type))

(cffi:defcfun ("wasm_func_result_arity" %wasm-func-result-arity) %size-type
  (func %wasm-func-type))

(cffi:defcfun ("wasm_func_call" %wasm-func-call) :pointer
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

;;; Global Instances

(define-wasm-ref global)

(cffi:defcfun ("wasm_global_new" %wasm-global-new) :pointer
  (store %wasm-store-type)
  (globaltype %wasm-globaltype-type)
  (val %wasm-val-type))

(cffi:defcfun ("wasm_global_type" %wasm-global-type) :pointer
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

(cffi:defcfun ("wasm_extern_type" %wasm-extern-type) :pointer
  (extern %wasm-extern-type))

(defmacro define-wasm-extern-conversion (type)
  (let ((type-name (make-object-type-parser-sym type))
	(as-extern-name (make-cfun-name type "_as_extern"))
	(as-type-name (list (format nil "wasm_extern_as_~a" (string-downcase type))
			    (alexandria:symbolicate '%wasm-extern-as- type))))
    `(progn
       (cffi:defcfun ,as-extern-name :pointer
	 (,type ,type-name))
       (cffi:defcfun ,as-type-name :pointer
	 (extern %wasm-extern-type)))))

(define-wasm-extern-conversion func)
(define-wasm-extern-conversion global)
(define-wasm-extern-conversion table)
(define-wasm-extern-conversion memory)

(define-wasm-object-class extern)
(define-wasm-vec-class extern)

;;; Module Instances

(define-wasm-ref instance)

(cffi:defcfun ("wasm_instance_new" %wasm-instance-new) :pointer
  (store %wasm-store-type)
  (module %wasm-module-type)
  (imports %wasm-extern-vec-type)
  (traps %wasm-trap-type))

(cffi:defcfun ("wasm_instance_exports" %wasm-instance-exports) :void
  (instance %wasm-instance-type)
  (out %wasm-extern-vec-type))

(define-wasm-object-class instance)

(defun make-wasm-instance (store module imports &optional traps)
  (enable-gc (make-instance 'wasm-instance
			    :pointer (%wasm-instance-new store
							 module
							 imports
							 (if (null? traps) (cffi:null-pointer) traps))
			    :parent store)))

(defclass wasm-instance-exports (wasm-extern-vec)
  ((exports :initarg :exports
	    :accessor exports)))

#|(defun make-wasm-instance-exports (instance)
  (let ((pointer (cffi:foreign-alloc '(:struct wasm-extern-vec-struct)))
	(exports (make-instance 'wasm-instance-exports
				:pointer pointer)
				:parent instance)))
    (%wasm-instance-exports instance exports)
    (enable-gc exports)
    (let ((num-exports (wasm-extern-vec-size exports)))
      (loop for i below num-exports
	    for export = (cffi:mem-aref pointer :pointer)
	    collect |#
      

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
