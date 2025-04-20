(defpackage #:cl-wasm-runtime.internal/trap
  (:nicknames #:wasm-rt/trap)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/byte-vec
	#:cl-wasm-runtime.internal/frame)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind
		#:with-auto-foreign-object)
  (:export #:wasm-trap
	   #:make-wasm-trap
	   #:wasm-trap-error
	   #:trap-message
	   #:trap-origin
	   #:trap-trace))

(in-package #:cl-wasm-runtime.internal/trap)

(define-wasm-object wasm-trap ())

(defun make-wasm-trap (store message)
  (check-type store wasm-store)
  (check-type message string)
  (with-new-wasm-object wasm-trap
    (wasm-ffi:wasm-trap-new (parent* store) (make-wasm-message message))))

(defun wasm-trap-error (trap)
  (error 'wasm-rt/error:wasm-trap-error
	 :message (trap-message trap)
	 :origin (trap-origin trap)
	 :trace (trap-trace trap)))

(defun trap-message (trap)
  (with-auto-foreign-object ((bytes '(:struct wasm-ffi:wasm-byte-vec-t))
			     #'wasm-ffi:wasm-byte-vec-delete)
    (wasm-ffi:wasm-trap-message trap bytes)
    (wasm-byte-vec-to-string bytes :null-terminated t)))

(defun trap-origin (trap)
  "ORIGIN returns the top frame of the WebAssembly stack responsible for this trap."
  (with-new-wasm-object wasm-frame
    (wasm-ffi:wasm-trap-origin trap)))

(defun trap-trace (trap)
  "TRAP-TRACE returns a LIST of the WebAssembly frames for this trap."
  (wasm-vec-to-list
   (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-frame-vec-t))
			#'cffi:foreign-free))
     (with-new-wasm-object (wasm-frame-vec :dynamic? t)
       (wasm-ffi:wasm-trap-trace trap (out pointer))))))
