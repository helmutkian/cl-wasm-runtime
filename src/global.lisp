(uiop:define-package #:cl-wasm-runtime.internal/global
  (:nicknames #:wasm-rt/global)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/valtype
	#:cl-wasm-runtime.internal/globaltype
	#:cl-wasm-runtime.internal/val)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:with-auto-foreign-object
		#:auto-bind
		#:safe-bind
		#:with-memoized-slot)
  (:export #:wasm-global
	   #:make-wasm-global
	   #:global-type
	   #:global-value))

(in-package #:cl-wasm-runtime.internal/global)

(define-wasm-object wasm-global ()
  ((%type)))

(defun make-wasm-global (store globaltype lisp-value)
  (check-type store wasm-store)
  (let ((globaltype* (etypecase globaltype
		       ((pointer wasm-globaltype)
			globaltype)
		       ((or keyword (pointer wasm-valtype))
			(make-wasm-globaltype globaltype))
		       ((and cons list) (apply #'make-wasm-globaltype globaltype))))) 
    (with-new-wasm-object wasm-global
      (wasm-ffi:wasm-global-new (parent* store)
				globaltype*
				(make-wasm-val lisp-value (globaltype-type globaltype*))))))

(defun global-type (global)
  (check-type global wasm-global)
  (with-memoized-slot (%type global)
    (with-new-wasm-object wasm-globaltype
      (wasm-ffi:wasm-global-type global))))

(defun global-value (global)
  (with-auto-foreign-object ((val-pointer '(:struct wasm-ffi:wasm-val-t))
			     ;; wasmer's WASM-VAL-DELETE is not safe to call stack allocated data
			     #-wasmer(or #'wasm-ffi:wasm-val-delete))
    (wasm-ffi:wasm-global-get global val-pointer)
    (val-value val-pointer)))

(defun (setf global-value) (lisp-val global)
  (unless (globaltype-mutable? (wasm-ffi:wasm-global-type global))
    ;; TODO: Better error
    (wasm-rt/error:wasm-simple-error "Cannot set immutable global ~S" global))
  (with-auto-foreign-object ((val-pointer '(:struct wasm-ffi:wasm-val-t))
			     ;; wasmer's WASM-VAL-DELETE is not safe to call stack allocated data
			     #-wasmer(or #'wasm-ffi:wasm-val-delete))
    (let ((kind (globaltype-type (global-type global))))
      (prog1 lisp-val
	(init-wasm-val val-pointer lisp-val kind)
	;; TODO: Check bool result
	(wasm-ffi:wasm-global-set global val-pointer)))))

