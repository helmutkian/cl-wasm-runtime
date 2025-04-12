(defpackage #:cl-wasm-runtime.internal/module
  (:nicknames #:wasm-rt/module)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/byte-vec
	#:cl-wasm-runtime.internal/importtype
	#:cl-wasm-runtime.internal/exporttype)
  (:import-from #:cffi)
  (:import-from #:fast-io)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.internal/runtime
		#:with-signal-runtime-error)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:auto-bind
		#:safe-bind
		#:with-memoized-slot)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:export #:wasm-module
	   #:make-wasm-module
	   #:module-validate
	   #:module-imports
	   #:module-exports
	   #:module-serialize
	   #:module-deserialize
	   #:load-wasm
	   #:load-wasm-module))

(in-package #:cl-wasm-runtime.internal/module)

(defclass module-imports (wasm-importtype-vec)
  (%list))

(defun %make-module-imports (module)
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-importtype-vec-t))
		       #'cffi:foreign-free))
    (with-new-wasm-object ((module-imports wasm-importtype-vec) :dynamic? t) 
      (wasm-ffi:wasm-module-imports module (out pointer)))))

(defclass module-exports (wasm-exporttype-vec)
  (%list))

(defun %make-module-exports (module)
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-exporttype-vec-t))
		       #'cffi:foreign-free))
    (with-new-wasm-object ((module-exports wasm-exporttype-vec) :dynamic? t)
      (wasm-ffi:wasm-module-exports module (out pointer)))))

(define-wasm-object wasm-module ()
  ;; TODO: Do we need to track store here?
  ((%imports)
   (%exports)))

(defun make-wasm-module (store binary)
  (check-type store wasm-store)
  (check-type binary (or (pointer wasm-byte-vec) (vector (unsigned-byte 8) *)))
  (with-new-wasm-object wasm-module
    (with-signal-runtime-error
      (wasm-ffi:wasm-module-new (parent* store) binary))))

(defun module-validate (store binary)
  (check-type store wasm-store)
  (check-type binary (or (pointer wasm-byte-vec) (vector (unsigned-byte 8) *)))
  (with-signal-runtime-error
    (wasm-ffi:wasm-module-validate store binary)))

(defun module-imports (module)
  "Returns a LIST of WASM-IMPORTTYPEs"
  (let ((imports (with-memoized-slot (%imports module) (%make-module-imports module))))
    (with-memoized-slot (%list imports)
      (wasm-vec-to-list imports))))

(defun module-exports (module)
  "Returns a LIST of WASM-EXPORTTYPEs"
  (let ((exports (with-memoized-slot (%exports module) (%make-module-exports module))))
    (with-memoized-slot (%list exports)
      (wasm-vec-to-list exports))))

(defun module-serialize (module)
  (check-type module wasm-module)
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-byte-vec-t))
		       #'cffi:foreign-free))
    (with-new-wasm-object wasm-byte-vec
      (with-signal-runtime-error
	(wasm-ffi:wasm-module-serialize (parent* module) (out pointer))))))

(defun module-deserialize (store binary)
  (check-type store wasm-store)
  (check-type binary (or (pointer wasm-byte-vec) (vector (unsigned-byte 8) *)))
  (with-new-wasm-object wasm-module
    (with-signal-runtime-error
      (wasm-ffi:wasm-module-deserialize (parent* store) binary))))

(defun load-wasm (path)
  (with-open-file (in path :element-type 'fast-io:octet)
    (fast-io:with-fast-input (buf nil in)
      (let ((bin (fast-io:make-octet-vector (file-length in))))
	(fast-io:fast-read-sequence bin buf)
	(octets-to-wasm-byte-vec bin)))))

(defun load-wasm-module (store path)
  (make-wasm-module store (load-wasm path)))
