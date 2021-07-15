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

(defclass wasm-import ()
  ((name :initarg :name
	 :reader name)
   (extern :initarg :extern
	   :reader extern)))

(defun make-wasm-import (name externable)
  (make-instance 'wasm-import
		 :name name
		 :extern (to-wasm-extern externable)))

(defclass wasm-import-namespace ()
  ((name :initarg :name
	 :reader namespace)
   (externs-alist :initarg :imports
		  :reader imports
		  :initform nil)))

(defun make-wasm-namespace (namespace-name imports-list)
  (make-instance 'wasm-import-namespace
		 :name namespace-name
		 :imports (loop for import in imports-list
				collect (cons (name import)
					      (extern import)))))
(defclass wasm-imports (wasm-extern-vec)
  ((namespace-alist :initarg :namespaces
		    :reader :namespaces)))

(defun make-wasm-imports (module &optional namespaces-list)
  (let* ((namespace-alist (loop for namespace in namespaces-list
				for name = (name namespace)
				for externs-alist = (imports namespace)
				collect (cons name externs-alist)))
	 (extern-vec (loop for module-import in (wasm-module-imports module)
			   for namespace = (namespace module-import)
			   for name = (name module-import)
			   for import = (assoc name (cdr (assoc namespace
								namespace-alist)))
			   unless import
			     do (error (format nil "Missing import: \"~a\".\"~a\"." namespace name))
			   end
			   collect (to-wasm-extern import) into externs-list
			   finally (return (list-to-wasm-extern-vec externs-list))))
	 (imports (enable-gc (make-instance 'wasm-imports
					    :pointer (pointer extern-vec)
					    :namespaces namespace-alist))))
    (setf (owner extern-vec) imports)
    imports))
			      
     
(defclass wasm-instance-exports (wasm-extern-vec)
  ((export-alist :reader exports-alist)))

(defun make-wasm-instance-exports (instance)
  (let* ((module (module instance))
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

(defun get-export (exports name type)
  (let* ((extern (cdr (assoc name (exports-alist exports) :test #'string=))))
    (unless (null? extern)
      (from-wasm-extern extern type))))
  
(define-wasm-object-class instance ()
  ((module :initarg :module
	   :reader module)
   (exports :reader exports)))

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
