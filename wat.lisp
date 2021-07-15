(in-package #:cl-wasm-runtime)

(cffi:defcfun ("wat2wasm" %wat-to-wasm) :void
  (wat %wasm-byte-vec-type)
  (out %wasm-byte-vec-type))

(defun wat-to-wasm (store wat-str)
  (cffi:with-foreign-object (wasm-bytes '%wasm-byte-vec-type)
    (unwind-protect (progn
		      (%wat-to-wasm wat-str wasm-bytes)
		      (make-wasm-module store wasm-bytes))
      (%wasm-byte-vec-delete wasm-bytes))))
