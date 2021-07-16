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
				 :parent module)))
    (%wasm-module-imports module imports)
    (enable-gc imports)
    (setf (slot-value imports 'imports-list)
	  (wasm-importtype-vec-to-list imports))
    imports))
    

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
  ((exports)
   (imports)))

(defun make-wasm-module (store binary)
  (let* ((pointer (%wasm-module-new store binary))
	 (module (make-instance 'wasm-module
				:pointer pointer
				:parent store)))
    (enable-gc module)
    (setf (slot-value module 'exports)
	  (make-wasm-module-exports module)
	  (slot-value module 'imports)
	  (make-wasm-module-imports module))
    module))

(defun wasm-module-exports (module)
  (slot-value (slot-value module 'exports)
	      'exports-list))

(defun wasm-module-imports (module)
  (slot-value (slot-value module 'imports)
	      'imports-list))
