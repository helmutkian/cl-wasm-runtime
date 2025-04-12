(defpackage #:cl-wasm-runtime.internal/globaltype
  (:nicknames #:wasm-rt/globaltype)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/valtype
	#:cl-wasm-runtime.internal/vector)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-globaltype
	   #:wasm-globaltype-vec
	   #:make-wasm-globaltype
	   #:globaltype-type
	   #:globaltype-mutable?))

(in-package #:cl-wasm-runtime.internal/globaltype)

(define-wasm-object wasm-globaltype)
(define-wasm-vec wasm-globaltype)

(defun make-wasm-globaltype (valtype &key mutable?)
  (let ((mutability (cffi:foreign-enum-value 'wasm-ffi:wasm-mutability-enum
					     (if mutable? :wasm-var :wasm-const)))) 
    (with-new-wasm-object wasm-globaltype
      (wasm-ffi:wasm-globaltype-new (own (copy-wasm-valtype valtype))
				    mutability))))

(defun globaltype-type (globaltype)
  (check-type globaltype (pointer wasm-globaltype))
  (wasm-valkind-to-key
   (wasm-ffi:wasm-valtype-kind (wasm-ffi:wasm-globaltype-content globaltype))))

(defun globaltype-mutable? (globaltype)
  (check-type globaltype (pointer wasm-globaltype))
  (eql (wasm-ffi:wasm-globaltype-mutability globaltype)
       (cffi:foreign-enum-value 'wasm-ffi:wasm-mutability-enum :wasm-var)))

(defmethod wasm-object-eq? ((globaltype-a wasm-globaltype) (globaltype-b wasm-globaltype))
  (and (eql (globaltype-mutable? globaltype-a) (globaltype-mutable? globaltype-b))
       (wasm-object-eq? (globaltype-type globaltype-a) (globaltype-type globaltype-b))))

