(defpackage #:cl-wasm-runtime.prelude/util
  (:use #:cl)
  (:nicknames #:wasm-rt/util)
  (:import-from #:alexandria)
  (:import-from #:cffi)
  (:export #:package-symbolicate
	   #:process-slots
	   #:process-supers
	   #:find-slot
	   #:get-slot-option
	   #:find-slot-option
	   #:unsharp-quote
	   #:signed-to-unsigned
	   #:unsigned-to-signed
	   #:prog1-let
	   #:define-function
	   #:with-memoized-slot
	   #:with-auto-foreign-object
	   #:with-auto-foreign-objects
	   #:unwind-protect-let*
	   #:auto-bind
	   #:handler-case-let*
	   #:safe-bind))

(in-package #:cl-wasm-runtime.prelude/util)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun process-supers (base-classes supers)
    `(,@supers
      ,@(remove-if (lambda (base-class)
		     (find-if (lambda (super) (subtypep super base-class))
			      supers))
		   base-classes)))

  (defun process-slots (base-slots slots)
    `(,@(remove-if (lambda (slot)
		     (let ((slot-name (if (listp slot) (first slot) slot)))
			 (or (find slot-name slots)
			     (assoc slot-name slots))))
		   base-slots)
      ,@slots))

  (defun find-slot (name slots)
    (let ((slot (find-if
		 (lambda (slot)
		   (eql name
			(if (listp slot) (first slot) slot)))
		 slots)))
      (if (listp slot)
	  slot
	  (list slot))))

  (defun get-slot-option (slot option)
    (getf (rest slot) option))

  (defun find-slot-option (name slots option)
    (get-slot-option (find-slot name slots) option))

  (defun unsharp-quote (thing)
    (cond
      ((and (listp thing)
	    (eql 'function (first thing)))
       (second thing))
      ((and (listp thing)
	    (eql 'lambda (first thing)))
       thing)))
  
  (defun package-symbolicate (package &rest things)
    (find-symbol (apply #'concatenate
			'string
			(mapcar #'string things))
		 (find-package package)))

  (defun signed-to-unsigned (signed num-bits)
    (check-type signed (signed-byte *))
    (let ((min-value (ash -1 (1- num-bits)))
	  (max-value (1- (expt 2 num-bits))))
      (the (unsigned-byte *)
	   (cond
	     ((or  (< signed min-value) (> signed max-value))
	      (error "Value ~A is out of range for for (signed-byte ~A)." signed num-bits))
	     ((< signed 0) (+ signed (expt 2 num-bits)))
	     (t signed)))))
 
  (defun unsigned-to-signed (unsigned num-bits)
    (check-type unsigned (unsigned-byte *))
    (let ((max-value (1- (expt 2 num-bits)))
	  (half (ash 1 (1- num-bits))))
      (the (signed-byte *)
	   (cond
	     ((or (< unsigned 0) (> unsigned max-value))
	      (error "Value ~A is out of range for (unsigned-byte ~A)." unsigned num-bits))
	     ((>= unsigned half)
	      (- unsigned (expt 2 num-bits)))
	     (t unsigned)))))

  (defmacro prog1-let (binding &body body)
    (alexandria:once-only ())
    `(let (,binding)
       (prog1 ,(if (listp binding) (first binding) binding)
	 ,@body)))

  (defun %match-arg-types (lambda-list arg-types)
    (let ((lambda-map (make-hash-table)))
      (loop with current-key = nil
	    for sym in arg-types
	    if (member sym lambda-list-keywords)
	      do (setf current-key sym)
	    else
	      do (push sym (gethash current-key lambda-map))
	    end)
      (multiple-value-bind (required optional rest keys has-allow-other-keys aux has-keys)
	  (alexandria:parse-ordinary-lambda-list lambda-list)
	(declare (ignorable rest has-allow-other-keys aux))
	(append (mapcar #'list
			required
			(nreverse (gethash nil lambda-map)))
		(mapcar (lambda (arg type) `(,arg (or null ,type)) )
			optional
			(nreverse (gethash '&optional lambda-map)))
		(when has-keys
		  (mapcar (lambda (key-arg)
			    (destructuring-bind ((key arg) &rest key-arg-rest) key-arg
			      (declare (ignorable key-arg-rest))
			      (let ((key-type (second (assoc key (gethash '&key lambda-map)))))
				(unless key-type (error "&KEY arg ~A must have type" arg))
				`(,arg (or null ,key-type)))))
			  keys))))))

  (defun %ensure-return-type (return-type)
    (if (and (consp return-type)
	     (eql 'values (first return-type)))
	(if (intersection return-type lambda-list-keywords)
	    return-type
	    `(,@return-type &optional))
	`(values ,return-type &optional)))

  (defmacro define-function (name args arg-types return-type &body body)
    "Defines proclamation of function signature and CHECK-TYPE assertions for arguments.

Syntax:
(DEFINE-FUNCTION name args (arg-type*) return-type [docstring] declaration* body)"
    (setf return-type (%ensure-return-type return-type))
    (multiple-value-bind (body declares docstring)
	(alexandria:parse-body body :documentation t)
      `(progn
	 (declaim (ftype (function ,arg-types ,return-type) ,name))
	 (defun ,name ,args
	   ,@(when docstring `(,docstring))
	   ,@declares
	   ,@(loop for (arg type) in (%match-arg-types args arg-types)
		   collect `(check-type ,arg ,type))
	   ,@body))))
 
  (defmacro with-memoized-slot ((slot object) &body body)
    "If SLOT is bound on OBJECT, then the form evaluates to its value. Otherwise BODY is executed and SLOT is assigned its value.

Syntax:
(WITH-MEMOIZED-SLOT (slot object) body)"
    (alexandria:once-only (object) 
      `(if (slot-boundp ,object ',slot)
	   (slot-value ,object ',slot)
	   (setf (slot-value ,object ',slot)
		 (progn ,@body)))))

  (defmacro with-auto-foreign-object
      (((var foreign-type &optional (count 1)) &optional cleanup-function) &body body)
    "Wraps the body of WITH-FOREIGN-OBJECT within an UNWIND-PROTECT and calls an optional cleanup function automatically within the UNWIND-PROTECT body.

Syntax:
(WITH-AUTO-FOREIGN-OBJECT ((var foreign-type [count]) [cleanup-function]) body)"
    (alexandria:once-only (cleanup-function)
      `(cffi:with-foreign-object (,var ,foreign-type ,count)
	 (unwind-protect 
	      (progn ,@body)
	   (when ,cleanup-function (funcall ,cleanup-function ,var))))))

  (defmacro with-auto-foreign-objects (bindings &body body)
    "WITH-FOREIGN-OBJECTS version of WITH-AUTO-FOREIGN-OBJECT.

Syntax:
(WITH-AUTO-FOREIGN-OBJECTS (((var foreign-type [count]) [cleanup-function])*) body)"
    (if (null bindings)
	`(progn ,@body)
	`(with-auto-foreign-object ,(first bindings)
	   (with-auto-foreign-objects ,(rest bindings) ,@body))))

  (defmacro unwind-protect-let* (bindings protected &body body)
    "Performs each LET* binding within an UNWIND-PROTECT. This allows for cleanup of previous 
bindings should a subsequent one unwind the stack (e.g. signal a condition). Unbound variables will have value NIL within the UNWIND-PROTECT body. Declarations are not supported.

Syntax:
(UNWIND-PROTECT-LET* ({var|(var [init-form])}* protected-form cleanup-form*))"
    `(let ,(loop for bind in bindings collect (if (listp bind) (first bind) bind))
       (unwind-protect 
	    (progn
	      ,@(loop for bind in bindings
		      when (and (listp bind) (second bind))
			collect `(setf ,(first bind) ,(second bind)))
	      ,protected)
	 ,@body)))

  (defmacro auto-bind (bindings &body body)
    "Performs each binding within an UNWIND-PROTECT with an optional associated cleanup function that will be called automatically within the UNWIND-PROTECT body. Declarations are not supported.
Syntax:
(AUTO-BIND ({var|(var {[init-form]|[init-form cleanup-function]}}*) body)"
    `(unwind-protect-let*
	 ,(loop for bind in bindings
		collect (if (listp bind) `(,(first bind) ,(second bind)) bind))
	 (progn ,@body)
       ,@(loop for bind in bindings
	       when (and (listp bind) (third bind))
		 collect `(when ,(first bind) (funcall ,(third bind) ,(first bind))))))

  (defmacro handler-case-let* (bindings protected &body cases)
    "Performs each LET* binding within a HANDLER-CASE. This allows for cleanup of previous bindings should a subsequent one signal a condition. Unbound variables will have value NIL within the HANDLER-CASE cases. Declarations are not supported.

Syntax:
(HANDLER-CASE-LET* ({var|(var [init-form])}*) protected-form cleanup-form*)"
    `(let ,(loop for bind in bindings collect (if (listp bind) (first bind) bind))
       (handler-case
	   (progn
	     ,@(loop for bind in bindings
		     when (and (listp bind) (second bind))
		       collect `(setf ,(first bind) ,(second bind)))
	     
	     ,protected)
	 ,@cases)))

  (defmacro safe-bind (bindings &body body)
    "Performs each binding within an implicit HANDLER-CASE. If a condtion is signaled, the optionally associated cleanup function will run automatically. Declarations are not supported.

Syntax:
(SAFE-BIND ({var|(var {[init-form]|[init-form cleanup-function]}}*) body)"
    (let ((c (gensym)))
      `(handler-case-let*
	   ,(loop for bind in bindings
		  collect (if (listp bind) `(,(first bind) ,(second bind)) bind))
	   (progn ,@body)
	 (t (,c)
	    ,@(loop for bind in bindings
		    when (and (listp bind) (third bind))
		      collect `(when ,(first bind) (funcall ,(third bind) ,(first bind))))
	    (error ,c))))))
