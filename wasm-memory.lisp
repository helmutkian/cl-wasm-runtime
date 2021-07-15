(in-package #:cl-wasm-runtime)

(define-wasm-ref memory)

(define-wasm-object-class memory)

(defun wrap-wasm-memory (pointer &key owner)
  (enable-gc (make-instance 'wasm-memory
			    :pointer pointer
			    :owner owner)))
