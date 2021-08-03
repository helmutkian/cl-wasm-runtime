(in-package #:cl-wasm-runtime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Representation

;;; Type attributes

(cffi:defctype %wasm-mutability-type :uint8)

(cffi:defcenum %wasm-mutability-enum
  :wasm-const
  :wasm-var)

(define-wasm-object-type limits)

(cffi:defcstruct %wasm-limits-struct
  (min :uint32)
  (max :uint32))

(defclass wasm-limits (wasm-object)
  ((delete-function :initform #'cffi:foreign-free)))

(defun make-wasm-limits (min max)
  (let ((pointer (cffi:foreign-alloc '(:struct %wasm-limits-struct))))
    (setf (cffi:foreign-slot-value pointer '(:struct %wasm-limits-struct) 'min)
	  min
	  (cffi:foreign-slot-value pointer '(:struct %wasm-limits-struct) 'max)
	  max)
    (enable-gc (make-instance 'wasm-limits :pointer pointer))))

(defun wrap-wasm-limits (pointer &key owner)
  (enable-gc (make-instance 'wasm-limits :pointer pointer :owner owner)))

(defun minimum (limits)
  (cffi:foreign-slot-value (pointer limits) '(:struct %wasm-limits-struct) 'min))

(defun maximum (limits)
  (cffi:foreign-slot-value (pointer limits) '(:struct %wasm-limits-struct) 'max))

;;; Generic Function

(defgeneric value-type (wasm-val-type))

(defgeneric extern-type (wasm-extern-type))

(defgeneric limits (limitable))

(defmethod limits :around ((limitable wasm-object))
  (enable-gc (wrap-wasm-limits (call-next-method) :owner (owner limitable))))

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

(cffi:defcfun "wasm_valtype_new" %wasm-valtype-type ; own
  (valkind %wasm-val-kind-type))

(cffi:defcfun "wasm_valtype_kind" %wasm-val-kind-type
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

(defun ensure-wasm-valtype (valtype-or-key)
  (etypecase valtype-or-key
    (wasm-valtype valtype-or-key)
    (keyword (make-wasm-valtype valtype-or-key)))) 

(defmethod value-type :around ((object wasm-object))
  (enable-gc (wrap-wasm-valtype (call-next-method) :owner (owner object))))

(defun reference? (val-type-or-key)
  (>= (cffi:foreign-enum-value '%wasm-val-kind-enum
			       (etypecase val-type-or-key
				 (keyword val-type-or-key)
				 (wasm-valtype (kind val-type-or-key))))
      (cffi:foreign-enum-value '%wasm-val-kind-enum :wasm-any-ref)))

(defun number? (val-type-or-key)
  (not (reference? val-type-or-key)))

(define-wasm-vec-class valtype ()
  ((wrap-data-function :allocation :class
		       :initform #'wrap-wasm-valtype)))

;;; Function Types

(define-wasm-type functype)

(cffi:defcfun "wasm_functype_new" %wasm-functype-type ; own
  (params %wasm-valtype-vec-type) ; own
  (results %wasm-valtype-vec-type)) ; own

(cffi:defcfun "wasm_functype_params" %wasm-valtype-vec-type ; const
  (functype %wasm-functype-type))

(cffi:defcfun "wasm_functype_results" %wasm-valtype-vec-type ; const
  (functype %wasm-functype-type))

(define-wasm-object-class functype)

(defun make-wasm-functype (params-list results-list)
  (let* ((params (from-list (mapcar #'ensure-wasm-valtype params-list) 'wasm-valtype-vec))
	 (results (from-list (mapcar #'ensure-wasm-valtype results-list) 'wasm-valtype-vec))
	 (functype (enable-gc (make-instance 'wasm-functype
					    :pointer (%wasm-functype-new params results)))))
    (setf (owner params) functype
	  (owner results) functype)
    functype))

(defun wrap-wasm-functype (pointer &key owner)
  (enable-gc (make-instance 'wasm-functype
			    :pointer pointer
			    :owner owner)))

(defun params (functype)
  (wrap-wasm-valtype-vec (%wasm-functype-params functype) :owner (owner functype)))

(defun results (functype)
  (wrap-wasm-valtype-vec (%wasm-functype-results functype) :owner (owner functype)))

;;; Global Types

(define-wasm-type globaltype)

(cffi:defcfun "wasm_globaltype_new" %wasm-globaltype-type ; own
  (valtype %wasm-valtype-type) ; own
  (mutability %wasm-mutability-type))

(cffi:defcfun "wasm_globaltype_content" %wasm-valtype-type ; const
  (globaltype %wasm-globaltype-type))

(cffi:defcfun "wasm_globaltype_mutability" %wasm-mutability-type ; const
  (globaltype %wasm-globaltype-type))

(define-wasm-object-class globaltype)

(defun make-wasm-globaltype (valtype &key mutable)
  (let* ((vtype (ensure-wasm-valtype valtype))
	 (mutability-key (if mutable :wasm-var :wasm-const))
	 (mutability (cffi:foreign-enum-value '%wasm-mutability-enum mutability-key))
	 (globaltype (enable-gc (make-instance 'wasm-globaltype
					      :pointer (%wasm-globaltype-new vtype mutability)))))
    (setf (owner vtype) globaltype)
    globaltype))

(defun wrap-wasm-globaltype (pointer &key owner)
  (enable-gc (make-instance 'wasm-globaltype
			    :pointer pointer
			    :owner owner)))

(defmethod value-type ((globaltype wasm-globaltype))
  (%wasm-globaltype-content globaltype))

(defun mutable? (global-type)
  (eql (%wasm-globaltype-mutability global-type)
       (cffi:foreign-enum-value '%wasm-mutability-enum :wasm-var)))

;;; Table Types

(define-wasm-type tabletype)

(cffi:defcfun "wasm_tabletype_new" %wasm-tabletype-type ; own
  (elements %wasm-valtype-type) ; own
  (limits %wasm-limits-type))

(cffi:defcfun "wasm_tabletype_element" %wasm-valtype-type ; const
  (tabletype %wasm-tabletype-type))

(cffi:defcfun "wasm_tabletype_limits" %wasm-limits-type
  (tabletype %wasm-tabletype-type))

(define-wasm-object-class tabletype)

(defun make-wasm-tabletype (elements limits)
  (let* ((valtype (ensure-wasm-valtype elements))
	 (tabletype (enable-gc (make-instance 'wasm-tabletype
					      :pointer (%wasm-tabletype-new valtype limits)))))
    (setf (owner valtype) tabletype)
    tabletype))

(defun wrap-wasm-tabletype (pointer &key owner)
  (enable-gc (make-instance 'wasm-tabletype
			    :pointer pointer
			    :owner owner)))

(defmethod value-type ((tabletype wasm-tabletype))
  (%wasm-tabletype-element tabletype))

(defmethod limits ((table-type wasm-tabletype))
  (%wasm-tabletype-limits table-type))


;;; Memory Types

(define-wasm-type memorytype)

(cffi:defcfun "wasm_memorytype_new" %wasm-memorytype-type ; own
  (limits %wasm-limits-type))

(cffi:defcfun "wasm_memorytype_limits" %wasm-limits-type ; const
  (memorytype %wasm-memorytype-type))

(define-wasm-object-class memorytype)

(defun make-wasm-memorytype (limits)
  (enable-gc (make-instance 'wasm-memorytype
			    :pointer (%wasm-memorytype-new limits))))

(defun wrap-wasm-memorytype (pointer &key owner)
  (enable-gc (make-instance 'wasm-memorytype
			    :pointer pointer
			    :owner owner)))

(defmethod limits ((memorytype wasm-memorytype))
  (%wasm-memorytype-limits memorytype))
     
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

(cffi:defcfun "wasm_externtype_kind" %wasm-extern-kind-type
  (externtype %wasm-externtype-type))

(defmacro define-wasm-externtype-conversion (type-name)
  (let* ((type-sym (make-object-type-parser-sym type-name))
	 (to-externtype-name (format-wasm-cfun-name type-name "as_externtype"))
	 (to-externtype-const-name (format-wasm-cfun-name type-name "as_externtype_const"))
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
	 :reader kind)))

(defun wrap-wasm-externtype (pointer &key owner)
  (enable-gc (make-instance 'wasm-externtype
			    :pointer pointer
			    :owner owner
			    :kind (wasm-externkind-to-key (%wasm-externtype-kind pointer)))))

(defmethod extern-type :around ((object wasm-object))
  (enable-gc (wrap-wasm-externtype (call-next-method) :owner (owner object))))

(defgeneric to-wasm-extern-type (wasm-type))

(defmethod to-wasm-extern-type :around ((wasm-type wasm-object))
  (typecase wasm-type
    (wasm-externtype wasm-type)
    (t (enable-gc (wrap-wasm-externtype (call-next-method) :owner (owner wasm-type))))))

(defmethod to-wasm-extern-type ((extern-type wasm-externtype))
  extern-type)

(defmethod to-wasm-extern-type ((functype wasm-functype))
  (%wasm-functype-as-externtype functype))

(defmethod to-wasm-extern-type ((globaltype wasm-globaltype))
  (%wasm-globaltype-as-externtype globaltype))

(defmethod to-wasm-extern-type ((memorytype wasm-memorytype))
  (%wasm-memorytype-as-externtype memorytype))

(defmethod to-wasm-extern-type ((tabletype wasm-tabletype))
  (%wasm-tabletype-as-externtype tabletype))

(defun to-wasm-func-type (extern-type)
  (wrap-wasm-functype (%wasm-externtype-as-functype extern-type)
		      :owner (owner extern-type)))

(defun to-wasm-global-type (extern-type)
  (wrap-wasm-globaltype (%wasm-externtype-as-globaltype extern-type)
			:owner (owner extern-type)))

(defun to-wasm-memory-type (extern-type)
  (wrap-wasm-memorytype (%wasm-externtype-as-memorytype extern-type)
			:owner (owner extern-type)))

(defun to-wasm-table-type (extern-type)
  (wrap-wasm-tabletype (%wasm-externtype-as-tabletype extern-type)
		       :owner (owner extern-type)))


;;; Import Types

(define-wasm-type importtype)

(cffi:defcfun "wasm_importtype_new" %wasm-importtype-type ; own
  (namespace %wasm-name-type) ; own
  (name %wasm-name-type) ; own
  (externtype %wasm-externtype-type)) ; own

(cffi:defcfun ("wasm_importtype_module" %wasm-importtype-namespace) %wasm-name-type ; const
  (importtype %wasm-importtype-type))

(cffi:defcfun "wasm_importtype_name" %wasm-name-type ; const
  (importtype %wasm-importtype-type))

(cffi:defcfun "wasm_importtype_type" %wasm-externtype-type ; const
  (importtype %wasm-importtype-type))

(define-wasm-object-class importtype)

(defun make-wasm-importtype (namespace name extern-type-able)
  (with-wasm-byte-vecs ((namespace-bytes namespace)
			(name-bytes name))
    (let* ((extern-type (%wasm-externtype-copy (to-wasm-extern-type extern-type-able)))
	   (pointer (%wasm-importtype-new namespace-bytes name-bytes extern-type)))
      (enable-gc (make-instance 'wasm-importtype :pointer pointer)))))


(defun wrap-wasm-importtype (pointer &key owner)
  (make-instance 'wasm-importtype
		 :pointer pointer
		 :owner owner))

(defmethod name ((import-type wasm-importtype))
  (wasm-byte-vec-to-string (%wasm-importtype-name import-type)))

(defun namespace (import-type)
  (wasm-byte-vec-to-string (%wasm-importtype-namespace import-type)))

(defmethod extern-type ((importtype wasm-importtype))
  (%wasm-importtype-type importtype))
      
(define-wasm-vec-class importtype ()
  ((wrap-data-function :allocation :class
		       :initform #'wrap-wasm-importtype)))

;;; Export Types

(define-wasm-type exporttype)

(cffi:defcfun "wasm_exporttype_new" %wasm-exporttype-type ; own
  (name %wasm-name-type) ; own
  (externtype %wasm-externtype-type)) ; own

(cffi:defcfun "wasm_exporttype_name" %wasm-name-type ; const
  (exporttype %wasm-exporttype-type))

(cffi:defcfun "wasm_exporttype_type" %wasm-externtype-type ; const
  (exporttype %wasm-exporttype-type))

(define-wasm-object-class exporttype)

(defun make-wasm-exporttype (name externtype-able)
  (with-wasm-byte-vecs ((name-bytes name))
    (let* ((externtype (to-wasm-extern-type externtype-able))
	   (pointer (%wasm-exporttype-new name-bytes (%wasm-externtype-copy externtype))))
      (enable-gc (make-instance 'wasm-exporttype :pointer pointer)))))
					     
(defun wrap-wasm-exporttype (pointer &key owner)
  (let ((exporttype (make-instance 'wasm-exporttype
				    :pointer pointer
				    :owner owner)))
    (enable-gc exporttype)))

(defmethod name ((export-type wasm-exporttype))
  (wasm-byte-vec-to-string (%wasm-exporttype-name export-type)))

(defmethod extern-type ((exporttype wasm-exporttype))
  (%wasm-exporttype-type exporttype))

(define-wasm-vec-class exporttype ()
  ((wrap-data-function :allocation :class
		       :initform #'wrap-wasm-exporttype)))

;;; References

(cffi:defcstruct %wasm-ref-struct)

(defmacro define-wasm-ref-base (name)
  (let ((type-sym (make-object-type-parser-sym name))
	(copy-name (format-wasm-cfun-name name "copy"))
	(same-name (format-wasm-cfun-name name "same"))
	(get-host-info-name (format-wasm-cfun-name name "get_host_info"))
	(set-host-info-name (format-wasm-cfun-name name "set_host_info"))
	(set-host-info-with-finalizer-name (format-wasm-cfun-name name "set_host_info_with_finalizer")))
    `(progn
       (define-wasm-object-type ,name)
       (define-wasm-own ,name)
       (cffi:defcfun ,copy-name ,type-sym ; own
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
	(as-ref-name (format-wasm-cfun-name name "as_ref"))
	(ref-as-name (list (format nil "wasm_ref_as_~a" (string-downcase name))
			   (intern (format nil "%WASM-REF-AS-~A" name))))
	(as-ref-const-name (format-wasm-cfun-name name "as_ref_const"))
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
	 (share-name (format-wasm-cfun-name name "share"))
	 (obtain-name (format-wasm-cfun-name name "obtain")))
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
