(defpackage #:cl-wasm-runtime.internal/frame
  (:nicknames #:wasm-rt/frame)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	; #:cl-wasm-runtime.internal/instance
	)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-frame
	   #:wasm-frame-vec
	   ; #:instance
	   #:frame-func-index
	   #:frame-func-offset
	   #:frame-module-offset))

(in-package #:cl-wasm-runtime.internal/frame)

(define-wasm-object wasm-frame)
(define-wasm-vec wasm-frame)

#| TODO
(defun instance (frame)
  (make-instance 'wasm-instance
		 :pointer (wasm-ffi:wasm-frame-instance frame)))
|#

(defun frame-func-index (frame)
  (check-type frame (pointer wasm-frame))
  (wasm-ffi:wasm-frame-func-index frame))

(defun frame-func-offset (frame)
  (check-type frame (pointer wasm-frame))
  (wasm-ffi:wasm-frame-func-offset frame))

(defun frame-module-offset (frame)
  (check-type frame (pointer wasm-frame))
  (wasm-ffi:wasm-frame-module-offset frame))
