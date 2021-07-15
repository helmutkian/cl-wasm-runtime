(in-package #:cl-wasm-runtime)

;;; Configuration

(define-wasm-own config)

(cffi:defcfun ("wasm_config_new" %wasm-config-new) %wasm-config-type) ;own

(define-wasm-object-class config)

(defun make-wasm-config ()
  (enable-gc (make-instance 'wasm-config :pointer (%wasm-config-new))))

;;; Engine

(define-wasm-own engine)

(cffi:defcfun ("wasm_engine_new" %wasm-engine-new) %wasm-engine-type) ; own

(cffi:defcfun ("wasm_engine_new_with_config" %wasm-engine-new-with-config) %wasm-engine-type ; own
  (config %wasm-config-type)) ; own

(define-wasm-object-class engine)

(defun make-wasm-engine (&optional config)
  (let ((engine (enable-gc (make-instance 'wasm-engine
					  :pointer (if config
						       (%wasm-engine-new-with-config config)
						       (%wasm-engine-new))))))
    (when config
      (setf (owner config) engine))
    engine))

;;; WASM STORE

(define-wasm-object-type store)
(define-wasm-own store)

(cffi:defcfun ("wasm_store_new" %wasm-store-new) %wasm-store-type ;own
  (engine %wasm-engine-type))

(define-wasm-object-class store)

(defun make-wasm-store (engine)
  (let ((store-ptr (%wasm-store-new engine)))
    (enable-gc (make-instance 'wasm-store
			      :pointer store-ptr
			      :parent engine))))
