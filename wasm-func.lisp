(in-package #:cl-wasm-runtime)

(define-wasm-ref func)

(cffi:defcfun "wasm_func_new" %wasm-func-type ; own
  (store %wasm-store-type)
  (functype %wasm-functype-type)
  (callback :pointer))

(cffi:defcfun "wasm_func_new_with_env" %wasm-func-type ; own
  (store %wasm-store-type)
  (functype %wasm-functype-type)
  (callback :pointer)
  (env :pointer)
  (finalizer :pointer))

(cffi:defcfun "wasm_func_type" %wasm-functype-type ; own
  (func %wasm-func-type))

(cffi:defcfun "wasm_func_param_arity" %size-type
  (func %wasm-func-type))

(cffi:defcfun "wasm_func_result_arity" %size-type
  (func %wasm-func-type))

(cffi:defcfun "wasm_func_call" %wasm-trap-type
  (func %wasm-func-type)
  (args %wasm-val-vec-type)
  (results %wasm-val-vec-type))

(define-wasm-object-class func)

(defstruct host-function
  (store)
  (callback)
  (user-env))

(defclass wasm-host-func (wasm-func)
  ((host-function :initarg :host-function
		  :reader host-function)))

(defstruct host-function-store
  (lock (make-rwlock))
  (table (make-hash-table)))

(defun host-function-store-load (host-function-store index)
  (let ((host-function (with-read-lock (host-function-store-lock host-function-store)
			 (gethash index
				  (host-function-store-table host-function-store)))))
    (or host-function (error (format nil "Host function ~a does not exist" index)))))

(defun host-function-store-save (host-function-store host-function)
  (with-write-lock (host-function-store-lock host-function-store)
    (let* ((table (host-function-store-table host-function-store))
	   (index (or (loop for i being the hash-keys in table using (hash-value hf)
			    when (null hf)
			      do (return i)
			    end)
		      (hash-table-count table))))
      (setf (gethash index table) host-function)
      index)))

(defun host-function-store-remove (host-function-store index)
  (with-write-lock (host-function-store-lock host-function-store)
    (setf (gethash index (host-function-store-table host-function-store))
	  nil)))

(defvar *host-function-store* (make-host-function-store))

(cffi:defcstruct %function-environment-struct
  (store %wasm-store-type)
  (index :uint))

(defclass function-environment (wasm-object)
  ((delete-function :initform #'cffi:foreign-free)))

(defun alloc-function-environment (store index)
  (let ((pointer (cffi:foreign-alloc '(:struct %function-environment-struct))))
    (setf (cffi:foreign-slot-value pointer '(:struct %function-environment-struct) 'store)
	  (pointer store)
	  (cffi:foreign-slot-value pointer '(:struct %function-environment-struct) 'index)
	  index)
    pointer))

(defun make-function-environment (store index)
  (enable-gc (make-instance 'function-environment
			    :pointer (alloc-function-environment store index)
			    :parent store)))

(defgeneric to-wasm-trap-message (err)
  (:method ((err condition))
    (trivial-backtrace:print-condition err nil)))

(defun host-funcall-with-trampoline (env args results &key with-environment)
  (cffi:with-foreign-slots ((index store) env (:struct %function-environment-struct))
    (handler-case
	(let ((host-function (host-function-store-load *host-function-store* index)))
	  (handler-case
	      (let* ((args-list (wasm-vec-to-list args '(:struct %wasm-val-vec-struct) '%wasm-val-type))
		     (function (host-function-callback host-function))
		     (results-list (apply function
					  (append (when with-environment
						    (list (host-function-user-env host-function)))
						  args-list)))
		     (num-results (length results-list)))
		(cffi:with-foreign-slots ((data size) results (:struct %wasm-val-vec-struct))
		  (unless (= num-results size)
		    (error (format nil "Expected ~a results, but host function returned ~a" size num-results)))
		  (loop for result in results-list
			for i from 0
			do (%wasm-val-copy (cffi:mem-aptr data :pointer i) result))
		  (cffi:null-pointer)))
	    (t (c)
	      (make-wasm-trap (host-function-store host-function)
			      (to-wasm-trap-message c)))))
    (t (c)
      (make-wasm-trap store
		      (to-wasm-trap-message c))))))
      
(cffi:defcallback function-trampoline %wasm-trap-type
    ((env :pointer)
     (args %wasm-val-vec-type)
     (results  %wasm-val-vec-type))
  (host-funcall-with-trampoline env args results))

(cffi:defcallback function-with-environment-trampoline %wasm-trap-type
    ((env :pointer)
     (args %wasm-val-vec-type)
     (results %wasm-val-vec-type))
  (host-funcall-with-trampoline env args results :with-environment t))
   
(cffi:defcallback function-environment-finalizer :void
    ((env :pointer))
    ;; NOOP
  (declare (ignore env)))

(defun make-wasm-callback (function &key with-wasm-val-arguments)
  (lambda (&rest args-val-list)
    (let ((args (if with-wasm-val-arguments
		    args-val-list
		    (mapcar #'wasm-val-to-lisp args-val-list))))
      (loop for result in (multiple-value-list (apply function args))
	    collect (typecase result
		      (wasm-val result)
		      (t (lisp-to-wasm-val result)))))))
	    
(defun make-wasm-func (store functype callback &optional env)
  (let* ((host-function (make-host-function :store store :callback callback :user-env env))
	 (index (host-function-store-save *host-function-store* host-function))
	 (function-env (make-function-environment store index))
	 (pointer (%wasm-func-new-with-env store
					   functype
					   (if env
					       (cffi:callback function-with-environment-trampoline)
					       (cffi:callback function-trampoline))
					   (pointer function-env)
					   (cffi:callback function-environment-finalizer))))
    (enable-gc
     (make-instance 'wasm-host-func
		    :host-function host-function
		    :pointer pointer
		    :parent function-env
		    :delete-function (lambda (pointer)
				       (host-function-store-remove *host-function-store* index)
				       (%wasm-func-delete pointer))))))

(defun wrap-wasm-func (pointer &key owner)
  (enable-gc (make-instance 'wasm-func
			    :pointer pointer
			    :owner owner)))

(defun wasm-func-type (func &key owner)
  (wrap-wasm-functype (%wasm-func-type func) :owner owner))

(defun wasm-funcall (func &rest received-args)
  ;; Wasmer does not expose its internal trampoline to the C API. Therefore calling host functions
  ;; as WASM functions is not supported and will result in a panic. We could just call the host function
  ;; callback instead  of signalling a condition...
  (when (typep func 'wasm-host-func)
    (error "Calling host functions is not supported."))
  (let* ((functype (wasm-func-type func))
	 (num-received-args (length received-args))
	 (params (wasm-functype-params functype))
	 (num-params  (%wasm-func-param-arity func))
	 (num-results (%wasm-func-result-arity func)))
    (when (not (= num-received-args num-params))
      (error (format nil "WASM function called with ~a arguments, but wants exactly ~a" num-received-args num-params)))
    ;; Try to keep as many ephemeral objects in dynamic extent as possible
    (cffi:with-foreign-objects ((arg-arr '(:struct %wasm-val-struct) num-params)
				(args '(:struct %wasm-val-vec-struct))
				(results '(:struct %wasm-val-vec-struct)))
      (loop for arg in received-args
	    for i from 0
	    for kind = (%wasm-valtype-kind (wasm-vec-aref (pointer params)
							  '(:struct %wasm-valtype-vec-struct)
							  i))
	    for arg-val-ptr = (cffi:mem-aptr arg-arr '(:struct %wasm-val-struct) i)
	    do (wasm-val-init arg-val-ptr kind arg))
      (unwind-protect
	   (progn
	     (%wasm-val-vec-new args num-params arg-arr)
	     (%wasm-val-vec-new-uninitialized results num-results)
	     (let ((trap (%wasm-func-call func args results)))
	       (unless (null? trap)
		 (error 'wasm-trap-error
			:message (message trap)
			:origin (origin trap)
			:trace (trap-trace trap)))
	       (loop for i below num-results
		     collect (wasm-val-type-value (wasm-vec-aptr results '(:struct %wasm-val-vec-struct) i))
		       into result-values
		     finally (return (values-list result-values)))))
	(%wasm-val-vec-delete results)
	(%wasm-val-vec-delete args)))))
