(in-package #:cl-wasm-runtime)

(defgeneric dispose (object))
(defgeneric enable-garbage-collection (object))
(defgeneric wasm-delete (object))


(defclass wasm-object ()
  ((pointer :initarg :pointer
	    :accessor pointer
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
   (finalizer-data :accessor finalizer-data
		   :initform (cons t nil)
		   :documentation "CONS cell of flag set to NIL when object is disposed of and additional WASM objects dependent on this object.")))

(cffi:define-foreign-type wasm-object-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser %wasm-object-type))

#|
(defmethod initialize-instance :after ((instance wasm-object) &key)
  (enable-garbage-collection instance))
|#

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
  (when (car finalizer-data)
    ;; Mark as disposed
    (setf (car finalizer-data) nil)
    ;; Dispose dependencies
    (mapc-weak #'dispose (car finalizer-data))
    ;; Free WASM object
    (unless (null? pointer)
      (funcall delete-function pointer))))

(defmethod dispose ((object wasm-object))
  ;; Sever from parent
  (setf (parent object) nil)
  (when (car (finalizer-data object))
    (internal-dispose (finalizer-data object)
		      (slot-value object 'pointer) ; By-pass generic functions
		      (delete-function object))
    (setf (pointer object) nil)))

(defmethod enable-gc ((object wasm-object))
  (let ((pointer (slot-value object 'pointer))) ; By-pass generic functions
    (format t "ENABLE-GARBACE-COLLECTION ~a~%" pointer)
    (unless (null? pointer)
      (let ((finalizer-data (finalizer-data object))
	    (delete-function (delete-function object))
	    (parent (parent object)))
	(unless finalizer-data
	  (error "finalizer-data must not be NIL"))
	(unless delete-function
	  (error "delete-function must not be NIL"))
	  (when parent
	    (push (tg:make-weak-pointer object)
		  (cdr (finalizer-data parent))))
	(tg:finalize object
		     (lambda ()
		       (format t "FINALIZE ~a" pointer)
		       (internal-dispose finalizer-data pointer delete-function)))))))

(defmethod wasm-delete ((object wasm-object))
    (prog1 nil
      (dispose object)))
  
