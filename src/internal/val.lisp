(defpackage #:cl-wasm-runtime.internal/val
  (:nicknames #:wasm-rt/val)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/valtype)
  (:import-from #:cffi)
  (:import-from #:ieee-floats)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:unsigned-to-signed
		#:safe-bind)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:export #:wasm-val
	   #:wasm-val-vec
	   #:val-kind
	   #:val-value
	   #:infer-wasm-valkind
	   #:translate-to-wasm
	   #:init-wasm-val
	   #:make-wasm-val
	   #:copy-wasm-val))

(in-package #:cl-wasm-runtime.internal/val)

(defun wasm-val-kind-of (kind-key)
  (ecase kind-key
    (:wasm-i32 'wasm-ffi:i32)
    (:wasm-i64 'wasm-ffi:i64)
    (:wasm-f32 'wasm-ffi:f32)
    (:wasm-f64 'wasm-ffi:f64)
    (:wasm-anyref 'wasm-ffi:ref)
    (:wasm-funcref 'wasm-ffi:ref)))

(defun %wasm-val-kind (pointer)
  (cffi:foreign-slot-value pointer '(:struct wasm-ffi:wasm-val-t) 'wasm-ffi:kind))

(defun (setf %wasm-val-kind) (new-value pointer)
  (setf (cffi:foreign-slot-value pointer '(:struct wasm-ffi:wasm-val-t) 'wasm-ffi:kind)
	new-value))

(defun %wasm-val-of (pointer)
  (let ((kind-key (wasm-valkind-to-key (%wasm-val-kind pointer))))
    (cffi:foreign-slot-value (cffi:foreign-slot-value pointer
						      '(:struct wasm-ffi:wasm-val-t)
						      'wasm-ffi:of)
			     '(:union wasm-ffi:wasm-val-of)
			     (wasm-val-kind-of kind-key))))

(defun (setf %wasm-val-of) (new-value pointer)
  (let ((kind-key (wasm-valkind-to-key (%wasm-val-kind pointer))))
    (setf (cffi:foreign-slot-value (cffi:foreign-slot-value pointer
							     '(:struct wasm-ffi:wasm-val-t)
							     'wasm-ffi:of)
				    '(:union wasm-ffi:wasm-val-of)
				    (wasm-val-kind-of kind-key))
	  new-value)))

(define-wasm-object wasm-val)
(define-wasm-vec wasm-val ()
  ((data-type :initform '(:struct wasm-ffi:wasm-val-t))
   (element-as-pointer? :initform nil)))

(defun val-kind (val)
  (wasm-valkind-to-key (%wasm-val-kind (etypecase val
					 (cffi:foreign-pointer val)
					 (wasm-val (pointer val))))))

(defun val-value (val)
  "Values of WASM-VALKIND :WASM-I32 and :WASM-I64 are returned as (SIGNED-BYTE 32) and (SIGNED-BYTE 64) respectively"
  (let ((kind-key (val-kind val))
	(value (%wasm-val-of (etypecase val
			       (cffi:foreign-pointer val)
			       (wasm-val (pointer val))))))
    ;; TODO: Handle ref types. Maybe this should be a generic function?
    (ecase kind-key
      ((or :wasm-i32 :wasm-i64 :wasm-f32 :wasm-f64) value))))

;; TODO: Find a better way of deducing the bit-width of CL FLOATs
(defun lisp-float-to-float32 (float)
  (handler-case (ieee-floats:decode-float32 (ieee-floats:encode-float32 float))
    (simple-error (c)
      (declare (ignorable c))
      nil)))

(defun lisp-float-to-float64 (float)
  (handler-case (ieee-floats:decode-float64 (ieee-floats:encode-float64 float))
    (simple-error (c)
      (declare (ignorable c))
      nil)))

(defun infer-wasm-valkind (lisp-val)
  "Tries to guess the best WASM representation for a Lisp value. This is dangerous, you should probably explicitly provide the WASM-VALKIND!"
  (etypecase lisp-val
    ((or (signed-byte 32) (unsigned-byte 32)) :wasm-i32)
    ((or (signed-byte 64) (unsigned-byte 64)) :wasm-i64)
    (float
     (cond
       ((lisp-float-to-float32 lisp-val) :wasm-f32)
       ((lisp-float-to-float64 lisp-val) :wasm-f64)
       (t (error 'wasm-rt/error:wasm-translate-float-overflow
			  :datum lisp-val
			  :kind '(or :wasm-f32 :wasm-f64)))))))

;; TODO: Generic function?
(defun translate-to-wasm (object kind)
  (let ((kind-key
	  (etypecase kind
	    (keyword kind)
	    (wasm-valkind (wasm-valkind-to-key kind))))) 
    (handler-case 
	(ecase kind-key
	  (:wasm-i32
	   (etypecase object
	     ((signed-byte 32) object)
	     ((unsigned-byte 32) (unsigned-to-signed object 32))))
	  (:wasm-i64
	   (etypecase object
	     ((signed-byte 64) object)
	     ((unsigned-byte 64) (unsigned-to-signed object 64))))
	  (:wasm-f32
	    (etypecase object
	      (float
	       (or (lisp-float-to-float32 object)
		   (error 'wasm-rt/error:wasm-translate-float-overflow
			  :datum object
			  :kind kind-key)))))
	  (:wasm-f64
	   (etypecase object
	     (float
	      (or (lisp-float-to-float64 object)
		  (error 'wasm-rt/error:wasm-translate-float-overflow
			 :datum object
			 :kind kind-key))))))
      (type-error (c)
	(error 'wasm-rt/error:wasm-translate-type-error
	       :kind kind-key
	       :datum object
	       :expected-type (type-error-expected-type c))))))

(defun init-wasm-val (pointer lisp-val key-or-kind)
  (multiple-value-bind (kind kind-key)
      (etypecase key-or-kind
	(keyword (values (key-to-wasm-valkind key-or-kind) key-or-kind))
	(wasm-valkind (values key-or-kind (wasm-valkind-to-key key-or-kind))))
    (prog1 pointer
      (setf (%wasm-val-kind pointer) kind
	    (%wasm-val-of pointer) (translate-to-wasm lisp-val kind-key)))))

(defun make-wasm-val (lisp-val key-or-kind &key owner)
  ;; wasmer currently has a bug in wasm_val_delete that causes double freeing
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-val-t))
		       (or #-wasmer(then-free #'wasm-val-delete)
			   #+wasmer(or #'wasm-ffi:wasm-val-delete))))
    (make-instance 'wasm-val
		   :pointer (init-wasm-val pointer lisp-val key-or-kind)
		   :dynamic? (or #-wasmer(or t))
		   :owner owner)))

(defun copy-wasm-val (wasm-val &key owner)
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-val-t))
		       (or #-wasmer(then-free #'wasm-val-delete)
			   #+wasmer(or #'wasm-ffi:wasm-val-delete))))
    (wasm-ffi:wasm-val-copy pointer wasm-val)
    (make-instance 'wasm-val
		   :pointer pointer
		   :dynamic? (or #-wasmer(or t))
		   :owner owner)))

;; TODO: Should allow for comparisons against unboxed integers and floats?
(defmethod wasm-object-eq? ((val-a wasm-val) (val-b wasm-val))
  (and (eql (val-kind val-a) (val-kind val-b))
       (= (val-value val-a) (val-value val-b))))
