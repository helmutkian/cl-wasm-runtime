(in-package #:cl-wasm-runtime)

(define-wasm-ref instance)

(cffi:defcfun "wasm_instance_new" %wasm-instance-type ; own
  (store %wasm-store-type)
  (module %wasm-module-type)
  (imports %wasm-extern-vec-type)
  (traps %wasm-trap-type)) ; own

(cffi:defcfun "wasm_instance_exports" :void
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
	 :reader name)
   (externs-alist :initarg :imports
		  :reader imports
		  :initform nil)))

(defun make-wasm-namespace (namespace-name &rest imports-list)
  (make-instance 'wasm-import-namespace
		 :name namespace-name
		 :imports (loop for import in imports-list
				collect (cons (name import)
					      (extern import)))))
(defclass wasm-imports (wasm-extern-vec)
  ((namespace-alist :initarg :namespaces
		    :reader namespaces)))
	       

(defun make-wasm-imports (module &rest namespaces-list)
  (let* ((namespace-alist (loop for namespace in namespaces-list
				for name = (name namespace)
				for externs-alist = (imports namespace)
				collect (cons name externs-alist)))
	 (extern-vec (loop for module-import in (imports module)
			   for namespace = (namespace module-import)
			   for name = (name module-import)
			   for import = (cdr (assoc name
						    (cdr (assoc namespace
								namespace-alist
								:test #'string=))
						    :test #'string=))
			   unless import
			     do (error (format nil "Missing import: \"~a\".\"~a\"." namespace name))
			   end
			   collect import into externs-list
			   finally (return (from-list externs-list 'wasm-extern-vec)))))
    (change-class extern-vec
		  'wasm-imports
		  :namespaces namespace-alist)))

(defmacro import-modules (module &rest namespace-specs)
  `(make-wasm-imports
    ,module
    ,@(loop for (namespace . import-spec) in namespace-specs
	    collect `(make-wasm-namespace
		      ,namespace
		      ,@(loop for (name externable) in import-spec
			      collect `(make-wasm-import ,name ,externable))))))

     
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
	  (loop for extern in (to-list exports)
		for module-export in (exports module)
		collect (cons (name module-export)
			      extern)))
    
    exports))

(defun get-export (exports name type)
  (let* ((extern (cdr (assoc name (exports-alist exports) :test #'string=))))
    (unless (null? extern)
      (from-wasm-extern extern type))))
  
(define-wasm-object-class instance ()
  ((module :initarg :module
	   :reader module)
   (exports)))

(defmethod exports ((instance wasm-instance))
  (slot-value instance 'exports))

(defun make-wasm-instance (store module  &optional imports traps)
  (let ((instance (enable-gc
		   (make-instance 'wasm-instance
				  :pointer (%wasm-instance-new store
							       module
							       (or imports (make-wasm-imports module))
							       (if (null? traps) (cffi:null-pointer) traps))
				  :parent store
				  :module module))))
    (when traps
      (setf (owner traps) instance))
    (setf (slot-value instance 'exports)
	  (make-wasm-instance-exports instance))
    instance))
