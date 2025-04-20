(defpackage #:cl-wasm-runtime.wasmer/wat2wasm
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/byte-vec)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.wasmer-ffi))

(in-package #:cl-wasm-runtime.wasmer/wat2wasm)

(defun wat2wasm (wat-str)
  (safe-bind ((byte-vec (cffi:foreign-alloc '(:struct wasm-ffi:wasm-byte-vec-t))
			#'cffi:foreign-free)) 
    (with-new-wasm-object (wasm-byte-vec :dynamic? t)
      (cl-wasm-runtime.wasmer-ffi:wat2wasm wat-str (out byte-vec)))))
