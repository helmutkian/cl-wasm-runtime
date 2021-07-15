(in-package #:cl-wasm-runtime)

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
  ((kind :initarg :kind
	 :reader kind)))

(defun make-wasm-valtype (kind-key)
  (enable-gc (make-instance 'wasm-valtype
			    :pointer (%wasm-valtype-new (key-to-wasm-valkind kind-key))
			    :kind kind-key)))

(defun wrap-wasm-valtype (pointer &key owner)
  (enable-gc (make-instance 'wasm-valtype
			    :pointer pointer
			    :owner owner
			    :kind (wasm-valkind-to-key (%wasm-valtype-kind pointer)))))

(define-wasm-vec-class valtype)

(defun list-to-wasm-valtype-vec (list &key owner)
  (let ((size (length list)))
    (if (zerop size)
	(make-wasm-extern-vec-empty :owner owner)
	(cffi:with-foreign-pointer (arr size)
	  (loop for elm in list
		for i from 0
		do (setf (cffi:mem-aref arr :pointer i) (pointer elm)))
	  (make-wasm-valtype-vec size arr :owner owner)))))


;;; Function Types

(define-wasm-type functype)

(cffi:defcfun ("wasm_functype_new" %wasm-functype-new) %wasm-functype-type ; own
  (params %wasm-valtype-vec-type) ; own
  (results %wasm-valtype-vec-type)) ; own

(cffi:defcfun ("wasm_functype_params" %wasm-functype-params) %wasm-valtype-vec-type ; const
  (functype %wasm-functype-type))

(cffi:defcfun ("wasm_functype_results" %wasm-functype-results) %wasm-valtype-vec-type ; const
  (functype %wasm-functype-type))

(define-wasm-object-class functype)

(defun make-wasm-functype (params-list results-list)
  (let* ((params (list-to-wasm-valtype-vec params-list))
	 (results (list-to-wasm-valtype-vec results-list))
	 (functype (enable-gc (make-instance 'wasm-functype
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

(defun make-wasm-globaltype (valtype mutability-key)
  (let* ((mutability (cffi:foreign-enum-value '%wasm-mutability-enum mutability-key))
	 (globaltype (enable-gc (make-instance 'wasm-globaltype
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
  (namespace %wasm-name-type) ; own
  (name %wasm-name-type) ; own
  (externtype %wasm-externtype-type)) ; own

(cffi:defcfun ("wasm_importtype_module" %wasm-importtype-namespace) %wasm-name-type ; const
  (importtype %wasm-importtype-type))

(cffi:defcfun ("wasm_importtype_name" %wasm-importtype-name) %wasm-name-type ; const
  (importtype %wasm-importtype-type))

(cffi:defcfun ("wasm_importtype_type" %wasm-importtype-type) %wasm-externtype-type ; const
  (importtype %wasm-importtype-type))

(define-wasm-object-class importtype ()
  ((namespace :initarg :namespace
	      :reader namespace)
   (name :initarg :name
	 :reader name)
   (externtype :initarg :externtype
	       :reader externtype)))

(defun make-wasm-importtype (namespace name externtype)
  (let ((importtype (enable-gc (make-instance 'wasm-importtype
					      :pointer (%wasm-importtype-new module name externtype)
					      :parent module))))
    (setf (owner namespace) importtype
	  (owner name) importtype
	  (owner externtype) importtype)
    importtype))

(defun wrap-wasm-importtype (pointer &key owner)
  (let ((importtype (make-instance 'wasm-importtype
				   :pointer pointer
				   :owner owner)))
    (setf (slot-value importtype 'namespace)
	  (wasm-byte-vec-to-string (%wasm-importtype-namespace importtype))
	  (slot-value importtype 'name)
	  (wasm-byte-vec-to-string (%wasm-importtype-name importtype))
	  (slot-value importtype 'externtype)
	  (wrap-wasm-externtype (%wasm-importtype-type importtype)
				:owner (owner importtype)))
    importtype))
      
(define-wasm-vec-class importtype)

(defun wasm-importtype-vec-to-list (importtype-vec)
  (wasm-vec-to-list importtype-vec
		    '(:struct %wasm-importtype-vec-struct)
		    #'wrap-wasm-importtype
		    :owner importtype-vec))

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
  ((name :initarg :name
	 :reader wasm-exporttype-name)
   (externtype :initarg :externtype
	       :reader wasm-exporttype-externtype)))

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
				:owner (owner exporttype)))
    exporttype))

(define-wasm-vec-class exporttype)

(defun wasm-exporttype-vec-to-list (exporttype-vec)
  (wasm-vec-to-list exporttype-vec
		    '(:struct %wasm-exporttype-vec-struct)
		    #'wrap-wasm-exporttype
		    :owner exporttype-vec))

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