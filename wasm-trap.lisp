(in-package #:cl-wasm-runtime)

(define-wasm-ref trap)

(cffi:defcfun "wasm_trap_new" %wasm-trap-type ; own
  (store %wasm-store-type)
  (message %wasm-message-type))

(cffi:defcfun "wasm_trap_message" :void
  (trap %wasm-trap-type)
  (out %wasm-message-type))

(cffi:defcfun "wasm_trap_origin" %wasm-frame-type ; own
  (trap %wasm-trap-type))

(cffi:defcfun "wasm_trap_trace" :void
  (trap %wasm-trap-type)
  (out %wasm-frame-vec-type))

(define-wasm-object-class trap)

(defun make-wasm-trap (store message)
  (enable-gc (make-instance 'wasm-trap
			    :pointer (%wasm-trap-new store message)
			    :parent store)))
