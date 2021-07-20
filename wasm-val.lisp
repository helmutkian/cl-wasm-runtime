(in-package #:cl-wasm-runtime)

(cffi:defcunion %wasm-val-union
  (i32 :int32)
  (i64 :int64)
  (f32 %float-32-type)
  (f64 %float-64-type)
  (ref (:pointer (:struct %wasm-ref-struct))))

(cffi:defcstruct %wasm-val-struct
  (kind %wasm-val-kind-type)
  (of (:union %wasm-val-union)))

(define-wasm-object-type val)
(define-wasm-vec val)

(cffi:defcfun "wasm_val_delete" :void
  (v %wasm-val-type))

(cffi:defcfun "wasm_val_copy" :void
  (out %wasm-val-type)
  (in %wasm-val-type))

(defun wasm-val-type-kind-of (kind-key)
  (ecase kind-key
    (:wasm-i32 'i32)
    (:wasm-i64 'i64)
    (:wasm-f32 'f32)
    (:wasm-f64 'f64)
    (:wasm-any-ref 'ref)
    (:wasm-func-ref 'ref)))

(defun wasm-val-type-value (pointer)
  (let* ((kind (cffi:foreign-slot-value pointer '(:struct %wasm-val-struct) 'kind))
	 (kind-key (cffi:foreign-enum-keyword '%wasm-val-kind-enum kind)))
    (cffi:foreign-slot-value (cffi:foreign-slot-value pointer
						      '(:struct %wasm-val-struct)
						      'of)
			     '(:union %wasm-val-union)
			     (wasm-val-type-kind-of kind-key))))

(defun wasm-val-init (wasm-val kind val)
  (let ((kind-key (cffi:foreign-enum-keyword '%wasm-val-kind-enum kind))
	(kind-of (cffi:foreign-slot-value wasm-val '(:struct %wasm-val-struct) 'of)))
    (setf (cffi:foreign-slot-value wasm-val '(:struct %wasm-val-struct) 'kind)
	  kind
	  (cffi:foreign-slot-value kind-of '(:union %wasm-val-union) (wasm-val-type-kind-of kind-key))
	  val)))

(define-wasm-object-class val)

(defun make-wasm-val (lisp-val kind-or-key &key owner)
  (let* ((pointer (cffi:foreign-alloc '(:struct %wasm-val-struct)))
	 (wasm-val (enable-gc (make-instance 'wasm-val
					     :pointer pointer
					     :owner owner))))
    (wasm-val-init pointer
		   (typecase kind-or-key
		     (keyword
		      (cffi:foreign-enum-value '%wasm-val-kind-enum kind-or-key))
		     (t
		      kind-or-key))
		   lisp-val)
    wasm-val))

(defun wasm-val-kind (wasm-val)
  (unless (null? (pointer wasm-val))
    (wasm-valkind-to-key (cffi:foreign-slot-value (pointer wasm-val)
						  '(:struct %wasm-val-struct)
						  'kind))))

(defun wasm-val-value (wasm-val)
  (unless (null? (pointer wasm-val))
    (let ((kind-key (wasm-val-kind wasm-val)))
      (cffi:foreign-slot-value (cffi:foreign-slot-value (pointer wasm-val)
							'(:struct %wasm-val-struct)
							'of)
			       '(:union %wasm-val-union)
			       (wasm-val-type-kind-of kind-key)))))

(defun wrap-wasm-val (pointer &key owner)
  (enable-gc (make-instance 'wasm-val
			    :pointer pointer
			    :owner owner)))
			      

;; TODO: Translate more types
(defun lisp-to-wasm-valkind (lisp-val)
  (etypecase lisp-val
    ;; TODO: Translate CL floats properly
    (single-float :wasm-f32)
    (double-float :wasm-f64)
    ((unsigned-byte 32) :wasm-i32)
    ((unsigned-byte 64) :wasm-i64)))

(defun lisp-to-wasm-val (lisp-val)
  (make-wasm-val lisp-val
		 (lisp-to-wasm-valkind lisp-val)))

;; TODO: Properly translate types
(defun wasm-val-to-lisp (wasm-val)
  (wasm-val-value wasm-val))
  
(define-wasm-vec-class val ()
  ((wrap-data-function :allocation :class
		       :initform #'wrap-wasm-val)))
