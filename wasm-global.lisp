(in-package #:cl-wasm-runtime)

(define-wasm-ref global)

(cffi:defcfun "wasm_global_new" %wasm-global-type ; own
  (store %wasm-store-type)
  (globaltype %wasm-globaltype-type)
  (val %wasm-val-type))

(cffi:defcfun "wasm_global_type" %wasm-globaltype-type ; own
  (global %wasm-global-type))

(cffi:defcfun "wasm_global_get" :void
  (global %wasm-global-type)
  (out %wasm-val-type))

(cffi:defcfun "wasm_global_set" :void
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

(defun global-type (global)
  (wrap-wasm-globaltype (%wasm-global-type global) :owner (owner global)))

(defmethod value ((global wasm-global))
  (let ((val-pointer (cffi:foreign-alloc '(:struct %wasm-val-struct))))
    (%wasm-global-get global val-pointer)
    (wasm-val-value val-pointer)))

(defmethod (setf value) :before (val (global wasm-global))
  (unless (mutable? (global-type global))
    (error "Global value is not mutable, cannot set new value")))

(defmethod (setf value) ((val wasm-val) (global wasm-global))
  (%wasm-global-set global val))

(defmethod (setf value) (val (global wasm-global))
  (%wasm-global-set global (lisp-to-wasm-val val)))
