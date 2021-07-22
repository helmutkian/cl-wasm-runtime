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

(defun wrap-wasm-trap (pointer &key owner)
  (enable-gc (make-instance 'wasm-trap
			    :pointer pointer
			    :owner owner)))

(defun message (trap)
  (cffi:with-foreign-object (bytes '(:struct %wasm-byte-vec-struct))
    (%wasm-trap-message trap bytes)
    (wasm-byte-vec-to-string bytes :null-terminated t)))

(defun origin (trap)
  "ORIGIN returns the top frame of the WebAssembly stack responsible for this trap."
  (%wasm-trap-origin trap))

(defun trap-trace (trap)
  "TRAP-TRACE returns the trace of the WebAssembly frames for this trap."
  (let ((pointer (cffi:foreign-alloc '(:struct %wasm-frame-vec-struct))))
    (%wasm-trap-trace trap pointer)
    (wrap-wasm-frame-vec pointer)))

(define-condition wasm-trap-error (error)
  ((message :initarg :message
	    :reader wasm-trap-error-message)
   (origin :initarg :origin
	   :reader wasm-trap-error-origin)
   (trap-trace :initarg :trace
	       :reader wasm-trap-error-trace))
  (:report (lambda (c s)
	     (print (wasm-trap-error-message c) s))))
