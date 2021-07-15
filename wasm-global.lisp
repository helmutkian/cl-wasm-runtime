(in-package #:cl-wasm-runtime)

(define-wasm-ref global)

(cffi:defcfun ("wasm_global_new" %wasm-global-new) %wasm-global-type ; own
  (store %wasm-store-type)
  (globaltype %wasm-globaltype-type)
  (val %wasm-val-type))

(cffi:defcfun ("wasm_global_type" %wasm-global-type) %wasm-globaltype-type ; own
  (global %wasm-global-type))

(cffi:defcfun ("wasm_global_get" %wasm-global-get) :void
  (global %wasm-global-type)
  (out %wasm-val-type))

(cffi:defcfun ("wasm_global_set" %wasm-global-set) :void
  (global %wasm-global-type)
  (value %wasm-val-type))
	 
(define-wasm-object-class global)

(defun make-wasm-global (store globaltype val)
  (enable-gc (make-instance 'wasm-global
				 :pointer (%wasm-global-new store globaltype val))))

(defun wrap-wasm-global (pointer &key owner)
  (enable-gc (make-instance 'wasm-global
			    :pointer pointer
			    :owner owner)))

(defun wasm-global-get (global)
  (let ((val-pointer (cffi:foreign-alloc '(:struct %wasm-val-struct))))
    (%wasm-global-get global val-pointer)
    (wrap-wasm-val val-pointer)))

(defun wasm-global-set (global val)
  (%wasm-global-set global val))

(defun wasm-global-value (global)
  (wasm-val-value (wasm-global-get global)))
