(defpackage #:cl-wasm-runtime.internal/func
  (:nicknames #:wasm-rt/func)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/valtype
	#:cl-wasm-runtime.internal/functype
	#:cl-wasm-runtime.internal/val
	#:cl-wasm-runtime.internal/trap)
  (:import-from #:alexandria)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:prog1-let
		#:safe-bind
		#:auto-bind
		#:with-auto-foreign-objects
		#:with-memoized-slot)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:export #:wasm-func
	   #:make-wasm-func
	   #:wasm-funcall
	   #:func-type
	   #:func-param-arity
	   #:func-result-arity))

(in-package #:cl-wasm-runtime.internal/func)

;; TODO: Should WASM-FUNC be FUNCALLABLE?
(define-wasm-object wasm-func ()
  ((%type)))

;;; HOST-FUNCTION  =============================================================

;; TODO: Make thread-safe
(defstruct (%host-function-registry (:constructor %make-host-function-registry))
  "TABLE - HASH-TABLE of INTEGER indices to WASM-HOST-FUNCTION"
  (table (make-hash-table) :type hash-table))

(defstruct (%host-function (:constructor %make-host-function))
  (store)
  (callback)
  (user-env))

(defclass wasm-host-func (wasm-func)
  ((%host-function :initarg :host-function :reader host-function)))

(defvar *host-function-registry* (%make-host-function-registry))

(defun %host-function-load (index &optional (registry *host-function-registry*))
  (or (gethash index (%host-function-registry-table registry))
      (wasm-rt/error:wasm-simple-error "Host function ~a does not exist." index)))

(defun %host-function-save (host-function &optional (registry *host-function-registry*))
  (let* ((table (%host-function-registry-table registry)))
    (prog1-let (index (or (loop for index being the hash-keys in table using (hash-value hf)
				when (null hf) do (return index))
			  (hash-table-count table)))
      (setf (gethash index table) host-function))))

(defun %host-function-remove (index &optional (registry *host-function-registry*))
  (setf (gethash index (%host-function-registry-table registry)) nil))

(cffi:defcstruct %func-env
  (store wasm-ffi:wasm-store-t)
  (index :uint))

(defclass func-env (wasm-object)
  ((delete-function :initform (lambda (pointer) (declare (ignorable pointer)))))) ; noop

(defun make-func-env (store index)
  (check-type store wasm-store)
  (check-type index integer)
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct %func-env)) #'cffi:foreign-free))
    (setf (cffi:foreign-slot-value pointer '(:struct %func-env) 'store) (pointer store)
	  (cffi:foreign-slot-value pointer '(:struct %func-env) 'index) index)
    (make-instance 'func-env :pointer pointer :parent store :dynamic? t)))

(defun host-funcall-with-trampoline(env args results &key with-environment?)
  (cffi:with-foreign-slots ((index store) env (:struct %func-env))
    (handler-case
	(let* ((host-func
		 (%host-function-load index))
	       (num-args
		 (wasm-vec-size args '(:struct wasm-ffi:wasm-val-vec-t)))
	       (args-list
		 (append (when with-environment? (list (%host-function-user-env host-func)))
			 (loop for i below num-args
			       collect (wasm-vec-aptr args
						      '(:struct wasm-ffi:wasm-val-vec-t)
						      i
						      '(:struct wasm-ffi:wasm-val-t)))))
	       (callback
		 (%host-function-callback host-func)))
	  (handler-case
	      (let* ((results-list (apply callback args-list))
		     (num-results (length results-list))
		     (size (wasm-vec-size results '(:struct wasm-ffi:wasm-val-vec-t)))
		     (data (wasm-vec-data results '(:struct wasm-ffi:wasm-val-vec-t))))
		(unless (= num-results size)
		  (wasm-rt/error:wasm-simple-error
		 "Expected ~a results, but host function returned ~a"
		 size
		 num-results))
		(loop for result in results-list
		      for i from 0
		      for result-out = (cffi:mem-aptr data '(:struct wasm-ffi:wasm-val-t) i)
		      do (wasm-ffi:wasm-val-copy result-out result))
		(cffi:null-pointer))
	    (condition (c)
	      (wasm-ffi:wasm-trap-new (%host-function-store host-func)
				      (trivial-backtrace:print-condition c nil)))))
      (condition (c)
	(wasm-ffi:wasm-trap-new store (trivial-backtrace:print-condition c nil))))))

(cffi:defcallback function-trampoline wasm-ffi:wasm-trap-t-ptr
    ((env :pointer)
     (args wasm-ffi:wasm-val-vec-t-ptr)
     (results wasm-ffi:wasm-val-vec-t-ptr))
  (let ((ret-val (host-funcall-with-trampoline env args results)))
    ret-val))

(cffi:defcallback function-with-environment-trampoline wasm-ffi:wasm-trap-t-ptr
    ((env :pointer)
     (args wasm-ffi:wasm-val-vec-t-ptr)
     (results wasm-ffi:wasm-val-vec-t-ptr))
  (host-funcall-with-trampoline env args results :with-environment? t))

(cffi:defcallback func-env-finalizer :void
    ((env :pointer))
  ;; noop
  (declare (ignorable env)))

(defun make-wasm-callback (functype function &key with-args-as-wasm-vals? with-environment?)
  (declare (optimize debug))
  (lambda (env-or-val &rest args-val-list)
    ;; If ENVIRONMENT is not NIL, then the first arg is an env otherwise
    ;; its just an arg val
    (let* ((env (when with-environment? (list env-or-val)))
	   (vals (if env args-val-list (cons env-or-val args-val-list)))
	   (args (if with-args-as-wasm-vals?
		     (mapcar #'copy-wasm-val vals)
		     (mapcar #'val-value vals)))
	   (result-types (functype-results functype))
	   (results (multiple-value-list (apply function (append env args)))))
      (loop for result-type in result-types
	    for result in results
	    collect (typecase result
		      (wasm-val 
		       (unless (eql (val-kind result) (valtype-kind result-type))
			 ;; TODO: Better error
			 (error "Expected result of WASM-VALKIND ~a, but got ~a ~a"
				(valtype-kind result-type)
				(val-kind result)
				(val-value result)))
		       result)
		      (t (make-wasm-val result (valtype-kind result-type))))))))

(defun make-wasm-func (store functype function &key environment with-args-as-wasm-vals?)
  (check-type store wasm-store)
  (check-type functype wasm-functype)
  (check-type function function)
  (let* ((callback
	   (make-wasm-callback functype
			       function
			       :with-environment? (and environment t) ; convert to boolean
			       :with-args-as-wasm-vals? with-args-as-wasm-vals?))
	 (host-func
	   (%make-host-function :store store :callback callback :user-env environment))
	 (index
	   (%host-function-save host-func))
	 (func-env
	   (make-func-env store index))
	 (delete-function
	   (lambda (pointer)
	     (%host-function-remove index)
	     (wasm-ffi:wasm-func-delete pointer)))
	 (foreign-callback
	   (if environment
	       (cffi:callback function-with-environment-trampoline)
	       (cffi:callback function-trampoline))))
    (safe-bind ((pointer
		 (wasm-ffi:wasm-func-new-with-env store
						  functype
						  foreign-callback
						  (pointer func-env)
						  (cffi:callback func-env-finalizer))
		 delete-function))
      (make-instance 'wasm-host-func
		     :host-function host-func
		     :pointer pointer
		     :parent func-env
		     :delete-function delete-function))))

(defun func-type (func)
  "Get WASM-FUNCTYPE of FUNC"
  (check-type func wasm-func)
  (with-memoized-slot (%type func)
    (with-new-wasm-object wasm-functype
      (wasm-ffi:wasm-func-type func))))

(defun func-param-arity (func)
  "Get number of params expected by FUNC"
  (check-type func (pointer wasm-func))
  (wasm-ffi:wasm-func-param-arity func))

(defun func-result-arity (func)
  "Get number of results returned by FUNC"
  (check-type func (pointer wasm-func))
  (wasm-ffi:wasm-func-result-arity func))

(defun wasm-funcall (func &rest received-args)
  "Call WASM-FUNC"
  (declare (optimize debug))
  (check-type func wasm-func)
  ;; Wasmer does not expose its internal trampoline to the C API.
  ;; Therefore calling host functions as WASM functions is not supported and will result
  ;; in a panic. We could just call the host function callback instead of signalling a
  ;; condition...
  (when (typep func 'wasm-host-func)
    (wasm-rt/error:wasm-simple-error "Calling host functions is not supported."))
  (let* ((functype (func-type func))
	 (num-received-args (length received-args))
	 (params (functype-params functype))
	 (num-params (wasm-ffi:wasm-func-param-arity func))
	 (num-results (wasm-ffi:wasm-func-result-arity func)))
    (unless (= num-received-args num-params)
      (wasm-rt/error:wasm-simple-error
       "WASM-FUNC called with ~a arguments, but wants exactly ~a."
       num-received-args
       num-params))
    (with-auto-foreign-objects (((args '(:struct wasm-ffi:wasm-val-vec-t))
				 #'wasm-ffi:wasm-val-vec-delete)
				((results '(:struct wasm-ffi:wasm-val-vec-t))
				 #'wasm-ffi:wasm-val-vec-delete))
      (wasm-ffi:wasm-val-vec-new-uninitialized args num-params)
      (wasm-ffi:wasm-val-vec-new-uninitialized results num-results)
      (loop for received-arg in received-args
	    for param in params
	    for kind = (valtype-kind param)
	    for i from 0
	    for arg-ptr = (wasm-vec-aptr args
					 '(:struct wasm-ffi:wasm-val-vec-t)
					 i
					 '(:struct wasm-ffi:wasm-val-t))
	    do (init-wasm-val arg-ptr received-arg kind))
      (auto-bind ((trap (wasm-ffi:wasm-func-call func args results)
			#'wasm-ffi:wasm-trap-delete))
	(unless (null? trap)
	  (wasm-trap-error trap))
	(loop for i below num-results
	      for result = (wasm-vec-aptr results
					'(:struct wasm-ffi:wasm-val-vec-t)
					i
					'(:struct wasm-ffi:wasm-val-t)) 
	      collect (val-value result) into result-values
	      finally (return (values-list result-values)))))))
