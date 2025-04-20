(defpackage #:cl-wasm-runtime.internal/valtype
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector)
  (:nicknames #:wasm-rt/valtype)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-valkind
	   #:wasm-valkind-key
	   #:key-to-wasm-valkind
	   #:wasm-valkind-to-key
	   #:wasm-valtype
	   #:valtype-kind
	   #:make-wasm-valtype
	   #:copy-wasm-valtype
	   #:valtype-reference?
	   #:valtype-number?
	   #:wasm-valtype-vec
	   #:make-wasm-valtype-vec))

(in-package #:cl-wasm-runtime.internal/valtype)

(deftype wasm-valkind ()
  `(unsigned-byte 8))

(deftype wasm-valkind-key ()
  `(member :wasm-i32 :wasm-i64 :wasm-f32 :wasm-f64 :wasm-anyref :wasm-funcref))

(defun key-to-wasm-valkind (key)
  "Converts KEYWORD to foreign enum value"
  (check-type key keyword)
  (cffi:foreign-enum-value 'wasm-ffi:wasm-valkind-enum key))

(defun wasm-valkind-to-key (valkind)
  "Converts foreign enum value to KEYWORD"
  (check-type valkind wasm-valkind)
  (cffi:foreign-enum-keyword 'wasm-ffi:wasm-valkind-enum valkind))

(define-wasm-object wasm-valtype ()
  ()
  (:documentation "Represents the type of a wasm value."))

(define-wasm-vec wasm-valtype)

(defun make-wasm-valtype (kind-key)
  "Creates new WASM-VALTYPE instance.

Syntax:

(MAKE-WASM-VALTYPE kind-key) => valtype

kind-key - WASM-VALKIND-KEY
valtype - WASM-VALTYPE instance"
  (check-type kind-key wasm-valkind-key)
  (with-new-wasm-object wasm-valtype
    (wasm-ffi:wasm-valtype-new (key-to-wasm-valkind kind-key))))

(defun valtype-kind (valtype)
  "Get kind of WASM-VALTYPE instance.

Syntax:

(VALTYPE-KIND valtype) => kind

valtype - WASM-VALTYPE instance
kind - WASM-VALKIND-KEY"
  (etypecase valtype
    (wasm-valkind-key valtype)
    ((pointer wasm-valtype) (wasm-valkind-to-key (wasm-ffi:wasm-valtype-kind valtype)))))

(defun copy-wasm-valtype (valtype-or-key)
  (with-new-wasm-object wasm-valtype
    (wasm-ffi:wasm-valtype-new (etypecase valtype-or-key
				 ((or wasm-valtype cffi:foreign-pointer)
				  (wasm-ffi:wasm-valtype-kind valtype-or-key))
				 (wasm-valkind-key
				  (key-to-wasm-valkind valtype-or-key))
				 (wasm-valkind
				  valtype-or-key)))))

(defun valtype-reference? (val-type-or-key)
  "Is WASM-VALTYPE instance or WASM-VALKIND-KEY a reference type?

Syntax:

(VALTYPE-REFERENCE? val-type-or-key) => reference?

val-type-or-key - WASM-VALTYPE instance or WASM-VALKIND-KEY
reference? - BOOLEAN"
  (wasm-ffi:wasm-valkind-is-ref (etypecase val-type-or-key
				  (wasm-valkind-key
				   (cffi:foreign-enum-value 'wasm-ffi:wasm-valkind-enum
							    val-type-or-key))
				  ((or wasm-valkind wasm-valtype cffi:foreign-pointer)
				   (wasm-ffi:wasm-valtype-kind val-type-or-key)))))

(defun valtype-number? (val-type-or-key)
  "Is WASM-VALTYPE instance or WASM-VALKIND-KEY a numeric? type?

Syntax:

(VALTYPE-REFERENCE? val-type-or-key) => number?

val-type-or-key - WASM-VALTYPE instance or WASM-VALKIND-KEY
number? - BOOLEAN"
  (wasm-ffi:wasm-valkind-is-num (etypecase val-type-or-key
				  (wasm-valkind-key
				   (cffi:foreign-enum-value 'wasm-ffi:wasm-valkind-enum
							    val-type-or-key))
				  ((or wasm-valkind wasm-valtype cffi:foreign-pointer)
				   (wasm-ffi:wasm-valtype-kind val-type-or-key)))))

(defmethod wasm-object-eq? ((valtype-a wasm-valtype) (valtype-b wasm-valtype))
  (eql (valtype-kind valtype-a) (valtype-kind valtype-b)))

(defmethod wasm-object-eq? ((valtype-a symbol) (valtype-b wasm-valtype))
  (eql valtype-a (valtype-kind valtype-b)))

(defmethod wasm-object-eq? ((valtype-a wasm-valtype) (valtype-b symbol))
  (eql (valtype-kind valtype-a) valtype-b))


