(in-package #:cl-wasm-runtime)

(define-wasm-ref table)

(define-wasm-object-class table)

(defun wrap-wasm-table (pointer &key owner)
  (enable-gc (make-instance 'wasm-global
			    :pointer pointer
			    :owner owner)))
