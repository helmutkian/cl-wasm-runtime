(defpackage #:cl-wasm-runtime.internal/config
  (:nicknames #:wasm-rt/config)
  (:use #:cl
	#:cl-wasm-runtime.internal/object)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-config
	   #:make-wasm-config))

(in-package #:cl-wasm-runtime.internal/config)

(define-wasm-object wasm-config)

(defun make-wasm-config ()
  (make-instance 'config :pointer (wasm-ffi:wasm-config-new)))
