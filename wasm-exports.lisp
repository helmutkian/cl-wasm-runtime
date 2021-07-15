(in-package #:cl-wasm-runtime)

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
  ((namespace-table :initform nil)))

(defun ensure-namespace-table (imports)
  (unless (slot-value imports 'namespace-table)
    (setf (slot-value imports 'namespace-table)
	  (make-hash-table :test 'equal)))
  (slot-value imports 'namespace-table))

(defun make-wasm-imports ()
  (make-instance 'wasm-imports)) 

(defun register (imports namespace extern-alist)
  (unless (null? (pointer imports))
    (error "Cannot register new imports once WASM-INSTANCE is created"))
  (let ((namespace-table (ensure-namespace-table imports)))
    (setf (gethash namespace namespace-table) extern-alist)))

(defun import-from (module imports)
  (let* ((module-imports (wasm-module-imports module))
	 (namespace-table (ensure-namespace-table imports))
	 (extern-vec (loop for module-import in module-imports
			   for namespace = (namespace module-import)
			   for name = (name module-import)
			   for import = (assoc name (gethash namespace namespace-table))
			   unless import
			     do (error (format nil "Missing import: \"~a\".\"~\a" namespace name))
			   collect (to-wasm-extern import) into externs-list
			   finally (return (list-to-wasm-extern-vec externs-list :owner (owner imports))))))
    (setf (slot-value imports 'pointer)
	  (pointer extern-vec))
    (enable-gc imports)))
    
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
							       (import-from module imports)
							       (if (null? traps) (cffi:null-pointer) traps))
				  :parent store
				  :module module))))
    (when traps
      (setf (owner traps) instance))
    (setf (slot-value instance 'exports)
	  (make-wasm-instance-exports instance))
    instance))
