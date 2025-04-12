(uiop:define-package #:cl-wasm-runtime.internal/wasm-ffi
  (:nicknames #:cl-wasm-runtime/wasm-ffi #:wasm-rt/wasm-ffi #:wasm-ffi)
  (:import-from #:cffi)
  (:import-from #:alexandria)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:process-slots
		#:process-supers)
  (:use-reexport #:cl-wasm-runtime.prelude/ffi-type)
  #+wasmer(:use-reexport #:cl-wasm-runtime.wasmer)
  (:export #:wasm-val-of
	   #:wasm-val-t
	   #:wasm-name-t
	   #:wasm-name-new
	   #:wasm-new-empty
	   #:wasm-new-uninitialized
	   #:wasm-name-copy
	   #:wasm-name-delete
	   #:wasm-name-new-from-string
	   #:wasm-name-new-from-string-nt
	   #:+wasm-limits-max-default+
	   #:wasm-valkind-is-num
	   #:wasm-valkind-is-ref
	   #:wasm-valtype-is-num
	   #:wasm-valtype-is-ref
	   #:wasm-ptr-type
	   #:wasm-vec-ptr-type))

(cl:in-package #:cl-wasm-runtime.internal/wasm-ffi)

;;; Necessary redefinition to handle anonymous enum as struct field in WASM-VAL-T

(cffi:defcunion wasm-val-of
  (i32 int32-t)
  (i64 int64-t)
  (f32 float32-t)
  (f64 float64-t)
  (ref (:pointer (:struct wasm-ref-t))))

(cffi:defcstruct wasm-val-t
  (kind wasm-valkind-t)
  (of (:union wasm-val-of)))

(cffi:defctype wasm-val-t (:struct wasm-val-t))

;;; wasm_name ==================================================================

(cl:setf (cl:fdefinition 'wasm-name-new) #'wasm-byte-vec-new
	 (cl:fdefinition 'wasm-name-new-empty) #'wasm-byte-vec-new-empty
	 (cl:fdefinition 'wasm-name-new-uninitialized) #'wasm-byte-vec-new-uninitialized
	 (cl:fdefinition 'wasm-name-copy) #'wasm-byte-vec-copy
	 (cl:fdefinition 'wasm-name-delete) #'wasm-byte-vec-delete)

(cl:declaim (cl:inline wasm-name-new-from-string))
(cl:defun wasm-name-new-from-string (out s)
  (wasm-name-new out (cffi:foreign-funcall "strlen" :string s :int) s))

(cl:declaim (cl:inline wasm-name-new-from-string-nt))
(cl:defun wasm-name-new-from-string-nt (out s)
  (wasm-name-new out (cl:1+ (cffi:foreign-funcall "strlen" :string s :int)) s))

;;; wasm_valkind ===============================================================

(cl:declaim (cl:inline wasm-valkind-is-num))
(cl:defun wasm-valkind-is-num (k)
  (cl:< k (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-anyref)))

(cl:declaim (cl:inline wasm-valkind-is-ref))
(cl:defun wasm-valkind-is-ref (k)
  (cl:>= k (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-anyref)))

;;; wasm_limits ================================================================

(cl:defconstant +wasm-limits-max-default+ #xffffffff)

;;; wasm_valtype ===============================================================

(cl:declaim (cl:inline wasm-valtype-copy))
(cl:defun wasm-valtype-copy (valtype)
  (wasm-valtype-new (wasm-valtype-kind valtype)))

(cl:declaim (cl:inline wasm-valtype-is-num))
(cl:defun wasm-valtype-is-num (t)
  (wasm-valkind-is-num (wasm-valtype-kind t)))

(cl:declaim (cl:inline wasm-valtype-is-ref))
(cl:defun wasm-valtype-is-ref (t)
  (wasm-valkind-is-ref (wasm-valtype-kind t)))

(cl:declaim (cl:inline wasm-valtype-new-i32))
(cl:defun wasm-valtype-new-i32 ()
  (wasm-valtype-new (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-i32)))

(cl:declaim (cl:inline wasm-valtype-new-i64))
(cl:defun wasm-valtype-new-i64 ()
  (wasm-valtype-new (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-i64)))

(cl:declaim (cl:inline wasm-valtype-new-f32))
(cl:defun wasm-valtype-new-f32 ()
  (wasm-valtype-new (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-f32)))

(cl:declaim (cl:inline wasm-valtype-new-f64))
(cl:defun wasm-valtype-new-f64 ()
  (wasm-valtype-new (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-f64)))

(cl:declaim (cl:inline wasm-valtype-new-anyref))
(cl:defun wasm-valtype-new-anyref ()
  (wasm-valtype-new (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-anyref)))

(cl:declaim (cl:inline wasm-valtype-new-funcref))
(cl:defun wasm-valtype-new-funcref ()
  (wasm-valtype-new (cffi:foreign-enum-value 'wasm-valkind-enum :wasm-funcref)))

;;; wasm_val ===================================================================

;; TODO: Should probably be a reader macro to avoid unreachable code
(cl:defmacro %if-uint32-ptr (then else)
  `(cl:progn ,(cl:if (cl:= uintptr-max uint32-max) then else)))

(cl:declaim (cl:inline wasm-val-init-ptr))
(cl:defun wasm-val-init-ptr (out p)
  (cffi:with-foreign-slots ((kind of) out (:struct wasm-val-t))
    (cl:setf kind
	     (cffi:foreign-enum-value 'wasm-valkind-enum (%if-uint32-ptr :wasm-i32 :wasm-i64))
	     (cffi:foreign-slot-value of 'wasm-val-of (%if-uint32-ptr 'i32 'i64))
	     (cffi:pointer-address p))))

(cl:declaim (cl:inline wasm-val-ptr))
(cl:defun wasm-val-ptr (val)
  (cffi:with-foreign-slots ((of) val (:struct wasm-val-t))
    (cffi:foreign-slot-value of 'wasm-val-of (%if-uint32-ptr 'i32 'i64))))

;;; DEFINE-PARSE-METHOD for all :STRUCT types ==================================

(cl:mapcar (cl:lambda (type)
	     (cl:destructuring-bind (c-type-name ptr-parser-sym foreign-type) type
	       (cl:declare (cl:ignorable ptr-parser-sym))
	       (cl:eval `(cffi:define-parse-method ,(cl:find-symbol c-type-name) (cl:&rest args)
			   (cl:apply #'cl:make-instance ',foreign-type args)))))
	   *wasm-ptr-types*)
