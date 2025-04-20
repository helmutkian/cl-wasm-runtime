(defpackage #:cl-wasm-runtime.internal/tabletype
  (:nicknames #:wasm-rt/tabletype)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/valtype
	#:cl-wasm-runtime.internal/limits)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-tabletype
	   #:wasm-tabletype-vec	  
	   #:make-wasm-tabletype
	   #:tabletype-type
	   #:tabletype-limits))

(in-package #:cl-wasm-runtime.internal/tabletype)

(define-wasm-object wasm-tabletype)
(define-wasm-vec wasm-tabletype)

(defun make-wasm-tabletype (elements min max)
  (cffi:with-foreign-object (limits '(:struct wasm-ffi:wasm-limits-t))
    (with-new-wasm-object wasm-tabletype
      (wasm-ffi:wasm-tabletype-new (own (copy-wasm-valtype elements))
				   (init-wasm-limits limits min max)))))

(defun tabletype-type (tabletype)
  (wasm-valkind-to-key (wasm-ffi:wasm-valtype-kind (wasm-ffi:wasm-tabletype-element tabletype))))

(defun tabletype-limits (tabletype)
  "Returns (VALUES MIN MAX) of TABLETYPE's WASM-LIMITS"
  (check-type tabletype (pointer wasm-tabletype))
  (let ((limits (wasm-ffi:wasm-tabletype-limits tabletype)))
    (values (limits-min limits)
	    (limits-max limits))))

(defmethod wasm-object-eq? ((tabletype-a wasm-tabletype) (tabletype-b wasm-tabletype))
  (and (wasm-object-eq? (tabletype-type tabletype-a) (tabletype-type tabletype-b))
       (multiple-value-bind (min-a max-a) (tabletype-limits tabletype-a)
	 (multiple-value-bind (min-b max-b) (tabletype-limits tabletype-b)
	   (and (= min-a min-b)
		(= max-a max-b))))))


