(defpackage #:cl-wasm-runtime.internal/importtype
  (:nicknames #:wasm-rt/importtype)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/byte-vec
	#:cl-wasm-runtime.internal/externtype)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-importtype
	   #:wasm-importtype-vec
	   #:make-wasm-importtype-vec
	   #:make-wasm-importtype
	   #:copy-wasm-importtype
	   #:importtype-name
	   #:importtype-module
	   #:importtype-type))

(in-package #:cl-wasm-runtime.internal/importtype)

(define-wasm-object wasm-importtype)

(define-wasm-vec wasm-importtype)

(defun make-wasm-importtype (module name type)
  (check-type module string)
  (check-type name string)
  (check-type type (pointer externtype-able))
  (safe-bind ((module-bytes (make-wasm-name module))
	      (name-bytes (make-wasm-name name))
	      (externtype (wasm-ffi:wasm-externtype-copy (to-externtype type))
			  #'wasm-ffi:wasm-externtype-delete))
    (with-new-wasm-object wasm-importtype
      (wasm-ffi:wasm-importtype-new (own module-bytes)
				    (own name-bytes)
				    (own externtype)))))

(defun copy-wasm-importtype (importtype &key parent owner)
  (check-type parent (or null wasm-object))
  (check-type owner (or null wasm-object))
  (with-new-wasm-object (wasm-importtype :parent parent :owner owner)
    (wasm-ffi:wasm-importtype-copy importtype)))

(defun importtype-name (importtype)
  (wasm-byte-vec-to-string (wasm-ffi:wasm-importtype-name importtype)))

(defun importtype-module (importtype)
  (wasm-byte-vec-to-string (wasm-ffi:wasm-importtype-module importtype)))

(defun importtype-type (importtype)
  (check-type importtype wasm-importtype)
  (with-new-wasm-object wasm-externtype 
    (wasm-ffi:wasm-importtype-type (owner* importtype))))

(defmethod wasm-object-eq? ((importtype-a wasm-importtype) (importtype-b wasm-importtype))
  (and (string= (importtype-module importtype-a) (importtype-module importtype-b))
       (string= (importtype-name importtype-a) (importtype-name importtype-b))
       (wasm-object-eq? (importtype-type importtype-a) (importtype-type importtype-b))))
