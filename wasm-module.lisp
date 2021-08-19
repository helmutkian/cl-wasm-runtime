(in-package #:cl-wasm-runtime)

(define-wasm-sharable-ref module)

(cffi:defcfun "wasm_module_new" %wasm-module-type ; own
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(cffi:defcfun "wasm_module_validate" :boolean
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(cffi:defcfun "wasm_module_imports" :void
  (module %wasm-module-type)
  (out %wasm-importtype-vec-type))

(cffi:defcfun "wasm_module_exports" :void
  (module %wasm-module-type)
  (out %wasm-exporttype-type))

(cffi:defcfun "wasm_module_serialize" :void
  (module %wasm-module-type)
  (out %wasm-byte-vec-type))

(cffi:defcfun "wasm_module_deserialize" %wasm-module-type
  (store %wasm-store-type)
  (binary %wasm-byte-vec-type))

(defclass wasm-module-imports (wasm-importtype-vec)
  ((imports-list :reader imports)))

(defun make-wasm-module-imports (module)
  (let* ((pointer (cffi:foreign-alloc '(:struct %wasm-importtype-vec-struct)))
	 (imports (make-instance 'wasm-module-imports
				 :pointer pointer
				 :parent module
				 :delete-function (then-free #'%wasm-importtype-vec-delete))))
    (%wasm-module-imports module imports)
    (enable-gc imports)
    (setf (slot-value imports 'imports-list)
	  (to-list imports))
    imports))
    

(defclass wasm-module-exports (wasm-exporttype-vec)
  ((exports-list)))

(defun make-wasm-module-exports (module)
  (let* ((pointer (cffi:foreign-alloc '(:struct %wasm-exporttype-vec-struct)))
	 (exports (make-instance 'wasm-module-exports
				 :pointer pointer
				 :parent module
				 :delete-function (then-free #'%wasm-exporttype-vec-delete))))
    (%wasm-module-exports module exports)
    (enable-gc exports)
    (setf (slot-value exports 'exports-list)
	  (to-list exports))
    exports))

(define-wasm-object-class module ()
  ((exports)
   (imports)))

(defun wrap-wasm-module (store pointer)
  (let ((module (make-instance 'wasm-module
			       :pointer pointer
			       :parent store)))
    (enable-gc module)
    (setf (slot-value module 'exports)
	  (make-wasm-module-exports module)
	  (slot-value module 'imports)
	  (make-wasm-module-imports module))
    module))

(defun make-wasm-module (store binary)
  (wrap-wasm-module store (%wasm-module-new store binary)))

(defmethod exports ((module wasm-module))
  (slot-value (slot-value module 'exports)
	      'exports-list))

(defmethod imports ((module wasm-module))
  (slot-value (slot-value module 'imports)
	      'imports-list))

(defun serialize (module)
  (cffi:with-foreign-object (byte-vec '(:struct %wasm-byte-vec-struct))
    (%wasm-module-serialize module byte-vec)
    (wasm-byte-vec-copy byte-vec)))

(defun deserialize (store byte-vec)
  (wrap-wasm-module store (%wasm-module-deserialize store byte-vec)))

(defun load-wasm (path)
  (with-open-file (in path :element-type 'fast-io:octet)
    (fast-io:with-fast-input (buf nil in)
      (let ((bin (fast-io:make-octet-vector (file-length in))))
	(fast-io:fast-read-sequence bin buf)
	(octets-to-wasm-byte-vec bin)))))

(defun load-wasm-module (store path)
  (make-wasm-module store (load-wasm path)))
