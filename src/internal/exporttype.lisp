(defpackage #:cl-wasm-runtime.internal/exporttype
  (:nicknames #:wasm-rt/exporttype)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/byte-vec
	#:cl-wasm-runtime.internal/externtype)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:auto-bind
		#:safe-bind)
  (:export #:wasm-exporttype
	   #:wasm-exporttype-vec
	   #:make-wasm-exporttype
	   #:copy-wasm-exporttype
	   #:exporttype-name
	   #:exporttype-type))

(in-package #:cl-wasm-runtime.internal/exporttype)

(define-wasm-object wasm-exporttype)

(define-wasm-vec wasm-exporttype)

(defun make-wasm-exporttype (name type)
  (check-type name (or string (pointer wasm-byte-vec)))
  (check-type type (pointer externtype-able))
  (let ((name-bytes (etypecase name
		      (string (make-wasm-name name))
		      ((pointer wasm-byte-vec) (copy-wasm-byte-vec name))))) 
    (safe-bind ((externtype (wasm-ffi:wasm-externtype-copy (to-externtype type))
			    #'wasm-ffi:wasm-externtype-delete))
      (with-new-wasm-object wasm-exporttype
	(wasm-ffi:wasm-exporttype-new (own (pointer name-bytes))
				      (own externtype))))))


(defun copy-wasm-exporttype (exporttype &key parent owner)
  (check-type parent (or null wasm-object))
  (check-type owner (or null wasm-object))
  (with-new-wasm-object (wasm-exporttype :parent parent :owner owner)
    (wasm-ffi:wasm-exporttype-copy exporttype)))

(defun exporttype-name (exporttype)
  (wasm-byte-vec-to-string (wasm-ffi:wasm-exporttype-name exporttype)))

(defun exporttype-type (exporttype)
  (check-type exporttype wasm-exporttype)
  (with-new-wasm-object (wasm-externtype)
    (wasm-ffi:wasm-exporttype-type (owner* exporttype))))

(defmethod wasm-object-eq? ((exporttype-a wasm-exporttype) (exporttype-b wasm-exporttype))
  (and (string= (exporttype-name exporttype-a) (exporttype-name exporttype-b))
       (wasm-object-eq? (exporttype-type exporttype-a) (exporttype-type exporttype-b))))
