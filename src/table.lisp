(defpackage #:cl-wasm-runtime.internal/table
  (:nicknames #:wasm-rt/table)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/tabletype)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind
		#:with-memoized-slot)
  (:export #:wasm-table
	   #:table-size
	   #:ref
	   #:grow-table)
  #-wasmer
  (:export #:table-type))

(in-package #:cl-wasm-runtime.internal/table)

(define-wasm-object wasm-table ()
  ((%tabletype)))

(defun make-wasm-table (store tabletype ref)
  (check-type store wasm-store)
  (let ((tabletype* (etypecase tabletype
		      ((pointer wasm-tabletype) tabletype)
		      ((and cons list) (apply #'make-wasm-tabletype tabletype))))) 
    (with-new-wasm-object wasm-table
      (wasm-ffi:wasm-table-new (parent* store) tabletype* ref))))

;; Wasmer does not provide WASM-TABLE-TYPE and due to ownership book-keeping it cannot
;; be mocked in the WASMER-specific WASM-FFI package. Wasmer-specific implementation
;; is located in WASM-RT/EXTERN package since it depends on EXTERN interface
#-wasmer
(defun table-type (table)
  (check-type table wasm-table)
  (with-memoized-slot (%tabletype table)
    (with-new-wasm-object wasm-tabletype
      (wasm-ffi:wasm-table-type table))))

(defun table-size (table)
  (wasm-ffi:wasm-table-size table))

(defun grow-table (table delta init-ref)
  (check-type delta (unsigned-byte 32))
  (not (zerop (wasm-ffi:wasm-table-grow table delta init-ref))))



