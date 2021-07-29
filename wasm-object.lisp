(in-package #:cl-wasm-runtime)

(defgeneric dispose (object))
(defgeneric enable-garbage-collection (object))
(defgeneric wasm-delete (object))

(defclass wasm-object ()
  ((pointer :initarg :pointer
	    :initform (cffi:null-pointer)
	    :documentation "A CFFI pointer from the WASM runtime.")
   (delete-function :initarg :delete-function
		    :accessor delete-function
		    :initform nil
		    :documentation "Function that will clean-up the WASM object, including free its pointer.")
   (parent :initarg :parent
	   :accessor parent
	   :initform nil
	   :documentation "WASM object this object is dependent on.")
   (owner :initarg :owner
	  :initform nil
	  :documentation "Owner of this WASM object. Calling the DELETE-FUNCTION on the owner will release this data, and therefore objects that are 'owned' should never be deleted directly.")
   (finalizer-data :accessor finalizer-data
		   :initform (list :owned? nil :disposed? nil :dependencies nil)
		   :documentation "Plist of data used to finalize the WASM object. :OWNED? is a T/NIL flag indicating whether another object 'owns' this object, and if not NIL, then this object should not be disposed. :DISPOSED? is a T/NIL flag to determine if the object has been disposed of already. :DEPENDENCIES are WASM objects that should be disposed of before this one.")))

(deftype pointer ()
  `(or wasm-object (satisfies cffi:pointerp)))

(defgeneric pointer (object))

(defmethod pointer ((object wasm-object))
  (slot-value object 'pointer))

(defmethod pointer (object)
  (if (cffi:pointerp object)
      object
      (error 'type-error :expected-type 'pointer :datum object)))

(defgeneric owner (object))

(defmethod owner (object)
  nil)

(defmethod owner ((object wasm-object))
  (or (slot-value object 'owner) object))

(defgeneric (setf owner) (owner object))

(defmethod (setf owner) (owner (object wasm-object))
  (setf (slot-value object 'owner)
	(unless (eql owner object) owner)
	(getf (finalizer-data object) :owned?)
	(not (null owner))))

(cffi:define-foreign-type wasm-object-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser %wasm-object-type))

(defmethod cffi:translate-to-foreign ((value wasm-object) (type wasm-object-type))
  (pointer value))

(defun null? (object)
  (or (null object)
      (typecase object
	(wasm-object (null? (pointer object)))
	(t (cffi:null-pointer-p object)))))

(defun mapc-weak (function list)
  (mapc (lambda (weak-obj)
	  (let ((obj (tg:weak-pointer-value weak-obj)))
	    (when obj
	      (funcall function obj))))
	list))

(defun internal-dispose (finalizer-data pointer delete-function)
  (unless (getf finalizer-data :disposed?)
    ;; Mark as disposed
    (setf (getf finalizer-data :disposed?) t)
    ;; Dispose dependencies
    (mapc-weak #'dispose (getf finalizer-data :dependencies))
    ;; Free WASM object if its not null and not owned
    (when (and (not (null? pointer))
	       (not (getf finalizer-data :owned?)))
      (format t "CALLING ~a ON ~a~%" delete-function pointer)
      (funcall delete-function pointer))))

(defmethod dispose ((object wasm-object))
  ;; Sever from parent
  (setf (parent object) nil)
  (unless (getf (finalizer-data object) :disposed?)
    (internal-dispose (finalizer-data object)
		      (slot-value object 'pointer) ; By-pass generic function
		      (delete-function object))
    (setf (slot-value object 'pointer) nil)))

(defmethod enable-gc ((object wasm-object))
  (let ((pointer (slot-value object 'pointer)) ; By-pass generic function
	(owner (slot-value object 'owner)) ; By-pass generic function
	(class (class-of object))) 
    (format t "ENABLE-GARBACE-COLLECTION ~a ~a~%" class pointer)
    (if (null? pointer)
	object
	(let ((finalizer-data (finalizer-data object))
	      (delete-function (delete-function object))
	      (parent (parent object)))
	  (unless finalizer-data
	    (error "finalizer-data must not be NIL"))
	  (unless delete-function
	    (error "delete-function must not be NIL"))
	  (when parent
	    (push (tg:make-weak-pointer object)
		  (getf (finalizer-data parent) :dependencies)))
	  (when owner
	    (setf (getf finalizer-data :owned?) t))
	  (tg:finalize object
		       (lambda ()
			 (format t "FINALIZE ~a ~a~%" class pointer)
			 (handler-case
			     (internal-dispose finalizer-data pointer delete-function)
			   (t (c)
			     (format t "ERROR FINALIZING ~a ~a~%" class pointer)
			     (trivial-backtrace:print-backtrace c)
			     (error c)))))))))

(defmethod wasm-delete ((object wasm-object))
    (prog1 nil
      (dispose object)))
  
