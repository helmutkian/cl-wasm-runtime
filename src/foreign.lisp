(defpackage #:cl-wasm-runtime.internal/foreign
  (:nicknames #:wasm-rt/foreign)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-foreign
	   #:make-wasm-foreign))

(in-package #:cl-wasm-runtime.internal/foreign)

(define-wasm-object wasm-foreign)

(defun make-wasm-foreign (store)
  (check-type store wasm-store)
  (with-new-wasm-object wasm-foreign
    (wasm-ffi:wasm-foreign-new (parent* store))))
