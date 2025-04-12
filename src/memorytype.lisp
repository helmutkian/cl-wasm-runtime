(defpackage #:cl-wasm-runtime.internal/memorytype
  (:nicknames #:wasm-rt/memorytype)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/limits)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-memorytype
	   #:make-wasm-memorytype
	   #:memorytype-limits))

(in-package #:cl-wasm-runtime.internal/memorytype)

(define-wasm-object wasm-memorytype)

;; TODO: Should this accept a limits object?
(defun make-wasm-memorytype (min max)
  (cffi:with-foreign-object (limits '(:struct wasm-ffi:wasm-limits-t))
    (with-new-wasm-object wasm-memorytype
      (wasm-ffi:wasm-memorytype-new (init-wasm-limits limits min max)))))

;; TODO: Should this return a limits object?
(defun memorytype-limits (memorytype)
  "Returns (VALUES MIN MAX) of MEMORYTYPE's WASM-LIMITS"
  (check-type memorytype (pointer wasm-memorytype))
  (let ((limits (wasm-ffi:wasm-memorytype-limits memorytype)))
    (values (limits-min limits)
	    (limits-max limits))))

(defmethod wasm-object-eq? ((memorytype-a wasm-memorytype) (memorytype-b wasm-memorytype))
  (multiple-value-bind (min-a max-a) (memorytype-limits memorytype-a)
    (multiple-value-bind (min-b max-b) (memorytype-limits memorytype-b)
      (and (= min-a min-b)
	   (= max-a max-b)))))
