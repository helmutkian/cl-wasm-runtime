(defpackage #:cl-wasm-runtime.internal/extern
  (:nicknames #:wasm-rt/extern)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/externtype
	#:cl-wasm-runtime.internal/func
	#:cl-wasm-runtime.internal/global
	#:cl-wasm-runtime.internal/table
	#:cl-wasm-runtime.internal/memory)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  #+wasmer
  (:import-from #:cl-wasm-runtime.prelude/util
		#:with-memoized-slot)
  #+wasmer
  (:export #:table-type)
  (:export #:wasm-extern
	   #:wasm-extern-vec
	   #:extern-able
	   #:make-wasm-extern-vec
	   #:extern-type
	   #:to-extern
	   #:extern-to-func
	   #:extern-to-global
	   #:extern-to-table
	   #:extern-to-memory
	   #:from-extern))

(in-package #:cl-wasm-runtime.internal/extern)

(define-wasm-object wasm-extern)
(define-wasm-vec wasm-extern)

(deftype extern-able ()
  `(or wasm-extern wasm-global wasm-func wasm-table wasm-memory))

(defun extern-type (extern)
  (with-new-wasm-object wasm-externtype
    ;; TODO: Should EXTERN be PARENT?
    (wasm-ffi:wasm-extern-type extern)))

(defun to-extern (object)
  (check-type object extern-able)
  (if (typep object 'wasm-extern)
      object
      (let ((as-wasm-extern-func
	      (etypecase object
		(wasm-func #'wasm-ffi:wasm-func-as-extern-const)
		(wasm-global #'wasm-ffi:wasm-global-as-extern-const)
		(wasm-table #'wasm-ffi:wasm-table-as-extern-const)
		(wasm-memory #'wasm-ffi:wasm-table-as-extern-const))))
	(with-new-wasm-object wasm-extern
	  (funcall as-wasm-extern-func (owner* object))))))


(defun extern-to-func (extern)
  (check-type extern wasm-extern)
  (with-new-wasm-object wasm-func
    (wasm-ffi:wasm-extern-as-func-const (owner* extern))))

(defun extern-to-global (extern)
  (check-type extern wasm-extern)
  (with-new-wasm-object wasm-global
    (wasm-ffi:wasm-extern-as-global-const (owner* extern))))

(defun extern-to-table (extern)
  (check-type extern wasm-extern)
  (with-new-wasm-object wasm-table
    (wasm-ffi:wasm-extern-as-table-const (owner* extern))))

(defun extern-to-memory (extern)
  (check-type extern wasm-extern)
  (with-new-wasm-object wasm-memory
    (wasm-ffi:wasm-extern-as-memory-const (owner* extern))))

(defun from-extern (extern)
  (let ((externkind (wasm-ffi:wasm-extern-kind extern)))
    (ecase (wasm-externkind-to-key externkind)
      (:wasm-extern-func (extern-to-func extern))
      (:wasm-extern-global (extern-to-global extern))
      (:wasm-extern-table (extern-to-table extern))
      (:wasm-extern-memory (extern-to-memory extern)))))

;; Wasmer does not provide WASM-TABLE-TYPE and due to ownership book-keeping it cannot
;; be mocked in the WASMER-specific WASM-FFI package. Non-wasmer-specific implementation
;; is located in WASM-RT/TABLE package
#+wasmer
(defun table-type (table)
  (check-type table wasm-table)
  (with-memoized-slot (wasm-rt/table::%tabletype table) ; need the internal symbol
    (externtype-to-tabletype (extern-type (to-extern table)))))
