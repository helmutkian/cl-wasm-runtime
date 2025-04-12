(defpackage #:cl-wasm-runtime.wasmer+
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/byte-vec)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:import-from #:cl-wasm-runtime.wasmer))

(in-package #:cl-wasm-runtime.wasmer+)

(defun wat2wasm (wat-str)
  (safe-bind ((byte-vec (cffi:foreign-alloc '(:struct cl-wasm-runtime.wasmer:wasm-byte-vec-t))
			#'cffi:foreign-free)) 
    (with-new-wasm-object (wasm-byte-vec :dynamic? t)
      (cl-wasm-runtime.wasmer:wat2wasm wat-str (out byte-vec)))))

