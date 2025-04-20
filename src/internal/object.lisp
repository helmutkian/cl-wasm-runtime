(defpackage #:cl-wasm-runtime.internal/object
  (:nicknames #:wasm-rt/object)
  (:use #:cl)
  (:import-from #:cffi)
  (:import-from #:alexandria)
  (:import-from #:trivial-garbage)
  (:import-from #:trivial-backtrace)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util)  
  (:export #:wasm-object
	   #:define-wasm-object
	   ;; =============== WITH-WASM-NEW-OBJECT ===============
	   #:with-new-wasm-object
	   #:own
	   #:parent*
	   #:out
	   #:owner*
	   ;; ===================================================
	   #:wasm-object-eq?
	   #:wasm-object-wrap
	   #:delete-function
	   #:pointer
	   #:null?
	   #:owner
	   #:wasm-object-dispose
	   #:then-free)
  #+debug
  (:export #:*wasm-objects*))

(in-package #:cl-wasm-runtime.internal/object)

;;;; IMPLEMENTATION NOTE:
;;;; While most wasm runtimes perform reference counting of their objects and allow for
;;;; out-of-order releasing (i.e. calling wasm_..._delete) of objects, this is NOT a part
;;;; of the wasm-c-api spec and therefore not guaranteed to be safe. Therefore in order to
;;;; remain portable and reliable across runtime implementations and their versions, we must
;;;; enforce strict ordering of releasing objects.

(defstruct finalizer-data
  "Data used to finalize a WASM-OBJECT.

OWNED? - BOOLEAN flag indicating whether another object 'owns' this object, and if not NIL, then this object should not be disposed. This redudant property is necessary since we cannot reference the object directly in the finalizer's callback.

DYNAMIC? - BOOLEAN flag indicating whether this object's pointer has been dynamically allocated, i.e. do we need to manually free the pointer?

DISPOSED? - BOOLEAN flag to determine if the object has been disposed of already.

DEFERRED-DISPOSAL - A FUNCTION that is set if the containing WASM-OBJECT is GC'd but whose pointer could not be disposed to finally dispose of it when all its depedents are disposed.

PARENT-FINALIZER-DATA - FINALIZER-DATA of the object that this object is dependent on.

DEPENDENTS - LIST of TG:WEAK-POINTERs to WASM-OBJECTs that should be disposed of before this one.

DEPENDENT-COUNT - An INTEGER count of the number of yet-to-be disposed dependents of the object.

DYNAMIC-DEPENDENCIES - LIST of dynamically allocated FOREIGN-POINTERs that should be freed after this object been disposed of."
  (owned? nil :type boolean)
  (dynamic? nil :type boolean)
  (disposed? nil :type boolean)
  (deferred-disposal nil :type (or null function))
  (parent-finalizer-data nil :type (or null finalizer-data))
  (dependents (list) :type list)
  (dependent-count 0 :type integer)
  (dynamic-dependencies (list) :type list))

(defclass wasm-object ()
  ((pointer :initarg :pointer
	    :initform (cffi:null-pointer)
	    :documentation "A CFFI pointer from the WASM runtime.")
   (delete-function :initarg :delete-function
		    :reader delete-function
		    :initform nil
		    :documentation "Function that will clean-up the WASM object.")
   (owner :initarg :owner
	  :initform nil
	  :documentation "Owner of this WASM object. Calling the DELETE-FUNCTION on the owner will release this data, and therefore objects that are 'owned' should never be deleted directly.")
   (finalizer-data :initform (make-finalizer-data)
		   :reader finalizer-data
		   :documentation "Data used to finalize the WASM object upon garbage collection.")))

;;; POINTER interface

(deftype pointer (object)
  `(or cffi:foreign-pointer ,object))

(defun disposed? (object)
  (or (finalizer-data-disposed? (finalizer-data object))
      (and (slot-value object 'owner)
	   (disposed? (slot-value object 'owner)))))

(defun check-not-disposed (object)
  (when (disposed? object)
    (error "Attempting to reference disposed WASM-OBJECT ~S." object)))

(defun pointer (object)
  (etypecase object
    (wasm-object
     (check-not-disposed object)
     (slot-value object 'pointer))
    (cffi:foreign-pointer
     object)))

(defun null? (object)
  (or (null object)
      (etypecase object
	(wasm-object (null? (slot-value object 'pointer)))
	(cffi:foreign-pointer (cffi:null-pointer-p object)))))

;;; OWNER interface

(defun owner (object)
  (check-type object wasm-object)
  (check-not-disposed object)
  (or (slot-value object 'owner) object))

(defun transfer-ownership (new-owner object)
  (let ((final-owner (owner new-owner)))
    (check-not-disposed final-owner)
    (check-not-disposed object)
    #+debug(format t "TRANSFERING OWNERSHIP OF ~A TO ~A~%" object final-owner)
    (setf (slot-value object 'owner) final-owner
	  (finalizer-data-owned? (finalizer-data object)) t)
    ;; If the object being owned is dynamically allocated, then its pointer will need
    ;; to be freed when its new owner is disposed
    (when (finalizer-data-dynamic? (finalizer-data object))
      (push (pointer object)
	    (finalizer-data-dynamic-dependencies (finalizer-data final-owner))))
    (tg:cancel-finalization object)))

(defun (setf owner) (new-owner object)
  (check-type new-owner wasm-object)
  (check-type object wasm-object)
  (with-slots (owner) object
    (if owner
	(error "Cannot transfer ownership of ~A to ~A. Already owned by ~A."
	       object
	       new-owner
	       owner)
	(transfer-ownership new-owner object))))

;;; PARENT interface

(defun add-dependent (parent dependent)
  (check-type parent wasm-object)
  (check-type dependent wasm-object)
  (check-not-disposed parent)
  (check-not-disposed dependent)
  (let ((parent-finalizer-data (finalizer-data parent))
	(dependent-finalizer-data (finalizer-data dependent)))
    (when (finalizer-data-parent-finalizer-data dependent-finalizer-data)
      (error "Cannot make ~A parent of ~A. Already has parent." parent dependent))
    #+debug(format t "ADDING DEPENDENT ~A to ~A~%" dependent parent)
    ;; TODO: These operations are not atomic!
    (setf (finalizer-data-parent-finalizer-data dependent-finalizer-data)
	  parent-finalizer-data)
    (pushnew (tg:make-weak-pointer dependent)
	     (finalizer-data-dependents parent-finalizer-data)
	     :key #'tg:weak-pointer-value
	     :test (lambda (obj-a obj-b)
		     (and obj-a obj-b
			  (cffi:pointer-eq (pointer obj-a) (pointer obj-b)))))
    (incf (finalizer-data-dependent-count parent-finalizer-data))))

(defun remove-dependent (parent-finalizer-data)
  (check-type parent-finalizer-data finalizer-data)
  (decf (finalizer-data-dependent-count parent-finalizer-data)))

;;; WASM-OBJECT-EQ? interface

(defgeneric wasm-object-eq? (object-a object-b)
  (:documentation "Determines if two given WASM-OBJECTs are equivalent. If the subclass of WASM-OBJECT does not provide a method, then a strict comparison of the CFFI:FOREIGN-POINTER values is made. Defaults to STRING= for two STRINGs and EQL for all other types")
  ;; WASM-OBJECT == WASM-OBJECT
  (:method :around ((object-a wasm-object) (object-b wasm-object))
    (or (cffi:pointer-eq (pointer object-a) (pointer object-b))
	(and (next-method-p)
	     (call-next-method))))
  ;; WASM-OBJECT == FOREIGN-POINTER
  (:method :around ((object-a wasm-object) (object-b t))
    (or (and (cffi:pointerp object-b)
	     (cffi:pointer-eq (pointer object-a) object-b))
	(and (next-method-p)
	     (call-next-method))))
  ;; FOREIGN-POINTER == WASM-OBJECT
  (:method :around ((object-a t) (object-b wasm-object))
    (or (and (cffi:pointerp object-a)
	     (cffi:pointer-eq object-a (pointer object-b)))
	(and (next-method-p)
	     (call-next-method))))
  ;; STRING == STRING
  (:method ((object-a string) (object-b string))
    (string= object-a object-b))
  ;; FOREIGN-POINTER == FOREIGN-POINTER || T == T
  (:method ((object-a t) (object-b t))
    (if (and (cffi:pointerp object-a) (cffi:pointerp object-b))
	(cffi:pointer-eq object-a object-b)
	(eql object-a object-b))))

;;; WASM-OBJECT-WRAP interface

(defun wasm-object-wrap (type object &rest initargs)
  (etypecase object
    (cffi:foreign-pointer
     (apply #'make-instance type :pointer object initargs))
    (wasm-object
     (unless (eql (class-name object) type)
       ;; TODO: Improve error
       (error "Cannot call (WASM-OBJECT-WRAP ~A ...) on object of type ~A"
	      type
	      (class-name object)))
     object)))

;;; DEFINE-WASM-OBJECT macro

(defmacro define-wasm-object (name &optional supers slots &rest options)
  (multiple-value-bind (object-name foreign-name)
      (if (listp name)
	  (values (first name) (second name))
	  (values name name)) 
    (let ((foreign-pointer-type
	    (wasm-rt/util:package-symbolicate :wasm-ffi foreign-name '-ptr-type)))
      `(progn
	   (defclass ,object-name ,(wasm-rt/util:process-supers '(wasm-object) supers)
	     ,(wasm-rt/util:process-slots
	       `((delete-function
		  :initform #',(wasm-rt/util:package-symbolicate :wasm-ffi
								 foreign-name
								 '-delete)))
	       slots)
	     ,@options)
	 (defmethod cffi:translate-to-foreign
	     ((object ,object-name) (type ,foreign-pointer-type))
	   (pointer object))))))

;;; WITH-NEW-WASM-OBJECT

(defmacro with-new-wasm-object (args &body body)
  "TODO: Document this!

Syntax:

(WITH-NEW-WASM-OBJECT {class|({class|(class foreign-name)} initarg*)} body)"
  (destructuring-bind (type &rest initargs) (if (listp args) args (list args))
    (multiple-value-bind (class-name foreign-name)
	(if (listp type)
	    (values (first type) (second type))
	    (values type type))
      (let ((delete-function (wasm-rt/util:package-symbolicate :wasm-ffi foreign-name '-delete)))
	(alexandria:with-gensyms (owned parent out owner)
	  `(let ((,owned '())
		 (,parent nil)
		 (,out nil)
		 (,owner nil))
	     (flet ((own (object)
		      (when (typep object 'wasm-object) (push object ,owned))
		      object)
		    (parent* (object)
		      (when (typep object 'wasm-object) (setf ,parent object))
		      object)
		    (out (object)
		      (setf ,out object)
		      object)
		    (owner* (object)
		      (when (typep object 'wasm-object) (setf ,owner object))
		      object))
	       (declare (ignorable #'own #'parent* #'out #'owner*))
	       (declare (inline own parent* out owner*))
	       (let ((result (progn ,@body)))
		 (handler-bind ((t #',delete-function))
		   (let ((object 
			   (apply #'make-instance
				  ',class-name
				  :pointer (or ,out result)
				  ,@initargs
				  (append (when ,owner (list :owner ,owner))
					  (when ,parent (list :parent ,parent))))))
		     (prog1 object
		       (dolist (owned-object ,owned)
			 (setf (owner owned-object) (owner object))))))))))))))

;;; Garbage collection

(defun mapc-weak (function list)
  (mapc (lambda (weak-obj)
	  (let ((obj (tg:weak-pointer-value weak-obj)))
	    (when obj
	      (funcall function obj))))
	list))

(defun %dispose (finalizer-data pointer delete-function)
  #+debug(declare (optimize debug))
  (with-slots (dynamic? disposed? deferred-disposal owned? parent-finalizer-data dependents
	       dependent-count dynamic-dependencies) finalizer-data
    (when (or disposed? (null? pointer) owned?)
      #+debug(format t
		     (if disposed?
			 "ALREADY DISPOSED WITH ~A ~A ~A"
			 "%DISPOSE PRECONDITIONS FAILED ON ~A ~A ~A~%")
		     finalizer-data
		     pointer
		     delete-function)
      (return-from %dispose))
    ;; Dispose dependents if they haven't been already
    ;; TODO: This might not be necessary due to reference counting,
    ;; but it guarantees that if the parent is disposed that so are all of its dependents
    (mapc-weak #'dispose dependents)
    (cond
      ;; Dispose WASM object if it no longer has dependents
      ((zerop dependent-count)
       ;; Dispose own pointer
       #+debug(format t "CALLING ~a ON ~a~%" delete-function pointer)
       (funcall delete-function pointer)
       ;; If pointer was dynamically allocated, then free it
       (when dynamic?
	 #+debug(format t "CALLING CFFI:FOREIGN-FREE ON ~a~%" pointer)
	 (cffi:foreign-free pointer))
       ;; Free all dynamically allocated dependencies
       (mapc (lambda (dep-pointer)
	       #+debug(format t "CALLING CFFI:FOREIGN-FREE ON ~a~%" dep-pointer)
	       (cffi:foreign-free dep-pointer))
	     dynamic-dependencies)
       ;; Handle parent reference
       (when parent-finalizer-data
	 ;; Remove depedent from parent
	 (remove-dependent parent-finalizer-data)
	 ;; If parent has no more depedents and its disposal has been deferred,
	 ;; then dispose of it
	 (when (and (zerop (finalizer-data-dependent-count parent-finalizer-data))
		    (finalizer-data-deferred-disposal parent-finalizer-data))
	   (funcall (finalizer-data-deferred-disposal parent-finalizer-data))
	   (setf parent-finalizer-data nil)))
       ;; Mark as disposed
       (setf disposed? t))
      ;; If pointer is still referenced by dependents, then defer pointer disposal
      (t
       #+debug(format t "DEFERRING CALLING ~a ON ~a~%" delete-function pointer)
       (setf deferred-disposal
	     (lambda () (%dispose finalizer-data pointer delete-function)))))))

(defun dispose (object)
  (with-slots (finalizer-data pointer delete-function) object
    (unless (finalizer-data-disposed? finalizer-data)
      (%dispose finalizer-data 
			pointer
			delete-function)
      (tg:cancel-finalization object)
      (setf pointer nil))))

(defun wasm-object-finalize (object)
  ;; Can't use WITH-SLOTS here since SYMBOL-MACROLET leaks reference to OBJECT
  ;; into finalizer callback
  (let ((owner (slot-value object 'owner))
	(pointer (slot-value object 'pointer))
	(finalizer-data (slot-value object 'finalizer-data))
	(delete-function (slot-value object 'delete-function)))
    (cond
      ((null? pointer) object)
      ;; Do not finalize owned objects
      (owner
       (transfer-ownership owner object)
       object)
      (t
       (let ((class (class-of object))
	     (hash (sxhash object)))
	 #+debug(format t "WASM-OBJECT-FINALIZE ~a ~a ~a~%" object hash pointer)
	 (tg:finalize object
		      (lambda ()
			;; Do NOT close over OBJECT here, otherwise GC of the object
			;; will be blocked!
			#+debug(format t "FINALIZE ~a ~a ~a~%" class hash pointer)
			(handler-case
			    (%dispose finalizer-data pointer delete-function)
			  (t (c)
			    #+debug(format t "ERROR FINALIZING ~a ~a ~a~%" class hash pointer)
			    (trivial-backtrace:print-backtrace c)
			    (error c))))))))))

(defun wasm-object-dispose (object)
  (check-type object wasm-object)
  (prog1 nil
    (dispose object)))

(defun then-free (delete-function)
  (lambda (pointer)
    (funcall delete-function pointer)
    (cffi:foreign-free pointer)))

#+debug
(defvar *wasm-objects* '()
  "Set of weak pointers of all WASM-OBJECTs instantiated. Helpful for debugging GC/memory issues.")

(defmethod initialize-instance :after ((object wasm-object) &key parent dynamic?)
  "Enable GC for all instances of WASM-OBJECT by default"
  (check-type dynamic? boolean)
  (check-type parent (or null wasm-object))
  (with-slots (owner pointer delete-function finalizer-data) object
    (check-type pointer cffi:foreign-pointer)
    (check-type delete-function function)
    (check-type owner (or null wasm-object))
    (check-type finalizer-data finalizer-data)
    (setf (finalizer-data-dynamic? finalizer-data) dynamic?)
    (when parent (add-dependent parent object))
    #+debug(push (or #+sbcl(sb-ext:make-weak-pointer object)
		     #-sbcl(tg:make-weak-pointer object))
		 *wasm-objects*)
    (wasm-object-finalize object)))


