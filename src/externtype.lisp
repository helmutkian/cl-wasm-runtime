(defpackage #:cl-wasm-runtime.internal/externtype
  (:nicknames #:wasm-rt/externtype)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/functype
	#:cl-wasm-runtime.internal/globaltype
	#:cl-wasm-runtime.internal/tabletype
	#:cl-wasm-runtime.internal/memorytype)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:with-memoized-slot)
  (:export #:wasm-externkind-to-key
	   #:wasm-externtype
	   #:externtype-kind
	   #:externtype-able
	   #:to-externtype
	   #:externtype-to-functype
	   #:externtype-to-globaltype
	   #:externtype-to-memorytype
	   #:externtype-to-tabletype
	   #:from-externtype))

(in-package #:cl-wasm-runtime.internal/externtype)

(defun key-to-wasm-externkind (key)
  (cffi:foreign-enum-value 'wasm-ffi:wasm-externkind-enum key))

(defun wasm-externkind-to-key (externkind)
  (cffi:foreign-enum-keyword 'wasm-ffi:wasm-externkind-enum externkind))

(define-wasm-object wasm-externtype ()
  ((%kind :initarg :kind)))

(defun externtype-kind (externtype)
  (check-type externtype wasm-externtype)
  (with-memoized-slot (%kind externtype)
    (wasm-externkind-to-key (wasm-ffi:wasm-externtype-kind externtype))))

(deftype externtype-able ()
  `(or wasm-externtype wasm-functype wasm-globaltype wasm-memorytype wasm-tabletype))

(defun to-externtype (type)
  (if (typep type 'wasm-externtype)
      type
      (let ((as-wasm-externtype-func
	      (etypecase type
		(wasm-functype #'wasm-ffi:wasm-functype-as-externtype-const)
		(wasm-globaltype #'wasm-ffi:wasm-globaltype-as-externtype-const)
		(wasm-memorytype #'wasm-ffi:wasm-memorytype-as-externtype-const)
		(wasm-tabletype #'wasm-ffi:wasm-tabletype-as-externtype-const))))
	(with-new-wasm-object wasm-externtype
	  (funcall as-wasm-externtype-func (owner* type))))))

(defun externtype-to-functype (externtype)
  (with-new-wasm-object wasm-functype
    (wasm-ffi:wasm-externtype-as-functype-const (owner* externtype))))

(defun externtype-to-globaltype (externtype)
  (with-new-wasm-object wasm-globaltype
    (wasm-ffi:wasm-externtype-as-globaltype-const (owner* externtype))))

(defun externtype-to-memorytype (externtype)
  (with-new-wasm-object wasm-memorytype
    (wasm-ffi:wasm-externtype-as-memorytype-const (owner* externtype))))

(defun externtype-to-tabletype (externtype)
  (with-new-wasm-object wasm-tabletype
    (wasm-ffi:wasm-externtype-as-tabletype-const (owner* externtype))))

(defun from-externtype (externtype)
  (ecase (externtype-kind externtype)
    (:wasm-extern-func (externtype-to-functype externtype))
    (:wasm-extern-global (externtype-to-globaltype externtype))
    (:wasm-extern-table (externtype-to-tabletype externtype))
    (:wasm-extern-memory (externtype-to-memorytype externtype))))

(defmethod wasm-object-eq? ((externtype-a wasm-externtype) (externtype-b wasm-externtype))
  (and (eql (externtype-kind externtype-a) (externtype-kind externtype-b))
       (wasm-object-eq? (from-externtype externtype-a)
			(from-externtype externtype-b))))
