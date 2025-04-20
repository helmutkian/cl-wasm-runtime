(defpackage #:cl-wasm-runtime.internal/vector
  (:nicknames #:wasm-rt/vec)
  (:use #:cl
	#:cl-wasm-runtime.internal/object)
  (:import-from #:alexandria)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.internal/generic)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-vec-data
	   #:wasm-vec-size
	   #:wasm-vec-aref
	   #:wasm-vec-aptr
	   #:make-wasm-vec-iterator
	   #:do-wasm-vec
	   #:sequence-into-wasm-vec
	   #:wasm-vec-to-list
	   #:wasm-vec
	   #:wasm-vec-size+
	   #:wasm-vec-aref+
	   #:do-wasm-vec+
	   #:define-wasm-vec
	   #:vec-type
	   #:data-type
	   #:wrap-data-function
	   #:element-as-pointer?))

(in-package #:cl-wasm-runtime.internal/vector)

;; TODO: Make inline
(defun wasm-vec-data (pointer type)
  (cffi:foreign-slot-value pointer type 'wasm-ffi:data))

;; TODO: Make inline
(defun wasm-vec-size (pointer type)
  (cffi:foreign-slot-value pointer type 'wasm-ffi:size))

;; TODO: Make inline
(defun wasm-vec-aref (pointer vec-type &optional (offset 0) (element-type :pointer))
  (cffi:mem-aref (wasm-vec-data pointer vec-type) element-type offset))

(defun (setf wasm-vec-aref) (value pointer vec-type &optional (offset 0) (element-type :pointer))
  (setf (cffi:mem-aref (wasm-vec-data pointer vec-type) element-type offset)
	value))

(defun wasm-vec-aptr (pointer vec-type &optional (offset 0) (element-type :pointer))
  (cffi:mem-aptr (wasm-vec-data pointer vec-type) element-type offset))

(defmacro do-wasm-vec (((elm-var &optional (elm-type :pointer) (index-var (gensym)))
			vec-pointer vec-type &optional result)
		       &body body)
  "Syntax:
(DO-WASM-VEC ((elm-var [elm-type] [index-var]) vec-pointer vec-type [result]) body)"
  (alexandria:once-only (vec-pointer vec-type)
    (alexandria:with-gensyms (data size)
      `(let ((,data (cffi:foreign-slot-value ,vec-pointer ,vec-type 'wasm-ffi:data))
	     (,size (cffi:foreign-slot-value ,vec-pointer ,vec-type 'wasm-ffi:size)))
	 (dotimes (,index-var ,size ,result)
	   (let ((,elm-var (cffi:mem-aref ,data ,elm-type ,index-var)))
	     ,@body))))))

(defun sequence-into-wasm-vec (sequence vec-pointer vec-type elm-type &optional transform)
  "Copies the contents of SEQUENCE into the DATA slot of VEC-POINTER. Does NOT make copies of SEQUENCE's contents. If contents need to live longer than VEC-POINTER, then provide a copying function as the TRANSFORM argument."
  (etypecase sequence
    (list
     (loop with data = (wasm-vec-data vec-pointer vec-type)
	   for elm in sequence
	   for i upto (1- (wasm-vec-size vec-pointer vec-type))
	   do (setf (cffi:mem-aref data elm-type i)
		    (if transform (funcall transform elm) elm))))
    (vector
     (loop with data = (wasm-vec-data vec-pointer vec-type)
	   for elm across sequence
	   for i upto (1- (wasm-vec-size vec-pointer vec-type))
	   do (setf (cffi:mem-aref data elm-type i)
		    (if transform (funcall transform elm) elm))))))


;;; Wasm vector high level interface

(defclass wasm-vec (wasm-object)
  ;; Metadata
  ((vec-type :reader vec-type
	     :documentation "CFFI type of the vector, e.g. '(:struct wasm-ffi:wasm-valtype-vec-t")
   (data-type :reader data-type
	      :documentation "CFFI type of vector's elements, e.g. 'wasm-ffi:wal-valtype-t")
   (wrap-data-function :reader wrap-data-function
		       :documentation "FUNCTION that constructs a WASM-OBJECT from a pointer to one of the vector's elements.")
   (element-as-pointer? :initform t
			:reader element-as-pointer
			:documentation "BOOLEAN that indicates whether vector's elements are pointers or values.")))

;; TODO: Make inline
(defun wasm-vec-size+ (vec)
  (check-type vec wasm-vec)
  (wasm-vec-size (pointer vec) (vec-type vec)))

;; TODO: Make inline
(defun wasm-vec-aref+ (vec &optional (offset 0))
  (check-type vec wasm-vec)
  (funcall (wrap-data-function vec)
	   (wasm-vec-aref (pointer vec) (vec-type vec) offset (data-type vec))
	   :owner vec))

(defun (setf wasm-vec-aref+) (value vec &optional (offset 0))
  "If VALUE is a WASM-OBJECT, then VEC will be made its owner. If that shouldn't be the case,
then be sure to pass a copy!"
  (check-type vec wasm-vec)
  (prog1 (setf (wasm-vec-aref (pointer vec) (vec-type vec) offset (data-type vec)) value)
    (when (typep value 'wasm-object)
      (setf (owner value) vec))))

(defmacro do-wasm-vec+ (((elm-var &optional (index-var (gensym))) vec &optional result) &body body)
  "Metadata-aware version of DO-WASM-VEC. ELM-VAR is a wrapped WASM-OBJECT owned by VEC. If elements need to live longer than VEC, then they must be copied.

Syntax:
(DO-WASM-VEC+ ((elm-var [index-var]) vec [result]) body)"
  (alexandria:once-only (vec)
    (let ((elm-pointer (gensym)))
      `(do-wasm-vec ((,elm-pointer (data-type ,vec) ,index-var)
		     (pointer ,vec) (vec-type ,vec)
		     ,result)
	 (let ((,elm-var (funcall (wrap-data-function ,vec) ,elm-pointer :owner (owner ,vec))))
	   ,@body)))))

(defun wasm-vec-to-list (vec &optional transform-function)
  "Copies contents of a WASM-VEC into a LIST of wrapped objects. Does not make copies of elements, and if the underlying OWNER of the vector data is released, then these elements become invalidated. If elements need to live longer than OWNER, provide a copying function as TRANSFORM-FUNCTION."
  (check-type vec wasm-vec)
  (let ((list))
    (do-wasm-vec+ ((elm) vec (nreverse list)) 
      (push (if transform-function (funcall transform-function elm) elm)
	    list))))

(defun %make-wasm-vec (vec-class
		       vec-type
		       data-type
		       new-empty-function
		       new-uninit-function
		       new-function
		       delete-function
		       &key
			 size
			 initial-contents
			 parent
			 owner)
  (check-type new-empty-function function)
  (check-type new-uninit-function function)
  (check-type new-function function)
  (check-type delete-function function)
  (check-type size (or null (unsigned-byte 32)))
  (check-type owner (or null wasm-object))
  (let ((vec (safe-bind ((pointer (cffi:foreign-alloc vec-type) (then-free delete-function)))
	       (make-instance vec-class
			      :pointer pointer
			      :parent parent
			      :owner owner
			      :dynamic? t))))
    (prog1 vec
      (cond
	;; No SIZE or INITIAL-CONTENTS provided
	((and (or (null size) (zerop size))
	      (null initial-contents))
	 (funcall new-empty-function vec))
	;; SIZE provided, but not INITIAL-CONTENTS
	((and (and size (> size 0))
	      (null initial-contents))
	 (funcall new-uninit-function vec size))
	;; INITIAL-CONTENTS provided, SIZE is optional in the case where
	;; INITIAL-CONTENTS is a Lisp SEQUENCE	
	(t
	 (etypecase initial-contents
	   ;; INITIAL-CONTENTS is a C array
	   (cffi:foreign-pointer
	    (funcall new-function vec size initial-contents))
	   ;; INITIAL-CONTENTS is a Lisp SEQUENCE
	   (sequence
	    (let ((vec-size (or size (length initial-contents)))
		  (i 0))
	      (funcall new-uninit-function vec vec-size)
	      (block map-block
		(map nil
		     (lambda (elm)
		       (unless (< i vec-size) (return-from map-block))
		       ;; TODO: Handle WASM-VECs that don't store pointers to their
		       ;; elements, e.g. WASM-BYTE-VEC & WASM-VAL-VEC
		       (setf (wasm-vec-aref (pointer vec) vec-type i data-type) elm)
		       (when (typep elm 'wasm-object) (setf (owner elm) vec))
		       (incf i))
		     initial-contents))))))))))

(defmacro define-wasm-vec (name &optional supers slots &rest options)
"
Defines a new CLASS that is a subtype of WASM-VEC (which is a subtype of WASM-OBJECT).

The assumption is made that the corresponding symbols in the CL-WASM-RUNTIME/WASM-FFI package
follow this pattern:

  name-VEC-T => C-type of vector
  name-T => C-type of vector data member
  name-VEC-NEW-EMPTY => FUNCTION to construct new empty vector
  name-VEC-NEW-UNINITIALIZED => FUNCTION to construct new uninitialized vector of given size
  name-VEC-NEW => FUNCTION to construct new vector with initial data members
  name-COPY => FUNCTION to copy data members from one vector to another
  name-VEC-DELETE => FUNCTION to destruct vector

The following symbols are interned into the current package

  name-VEC => Class of subtype WASM-VEC
  MAKE-name-VEC => Constructor FUNCTION for the class

Syntax:
(DEFINE-WASM-VEC name [(super*)] [(slot*)] option*)
"
  (let* (;; VEC-TYPE
	 (default-vec-type
	   `(:struct ,(wasm-rt/util:package-symbolicate :wasm-ffi name '-vec-t)))
	 (vec-type
	   (or (wasm-rt/util:find-slot-option 'vec-type slots :initform)
	       default-vec-type))
	 ;; DATA-TYPE
	 (default-data-type
	   (wasm-rt/util:package-symbolicate :wasm-ffi name '-t))
	 (data-type
	   (or (wasm-rt/util:find-slot-option 'data-type slots :initform)
	       default-data-type))
	 ;; CLASS-NAME
	 (class-name (alexandria:symbolicate name '-vec))
	 ;; NEW-...-FUNCTIONs
	 (new-empty-function
	   (wasm-rt/util:package-symbolicate :wasm-ffi name '-vec-new-empty))
	 (new-uninitialized-function
	   (wasm-rt/util:package-symbolicate :wasm-ffi name '-vec-new-uninitialized))
	 (new-function
	   (wasm-rt/util:package-symbolicate :wasm-ffi name '-vec-new))
	 ;; COPY-...-FUNCTION
	 #|
	 (copy-function
	   (wasm-rt/util:package-symbolicate :wasm-ffi name '-vec-copy))
	 (copy-data-function
	   (or (wasm-rt/util:package-symbolicate :wasm-ffi name '-copy)
	       'identity))
	 |#
	 ;; DELETE-FUNCTION
	 (default-delete-function
	   (wasm-rt/util:package-symbolicate :wasm-ffi name '-vec-delete))
	 (delete-function
	   (or (wasm-rt/util:unsharp-quote
		(wasm-rt/util:find-slot-option 'delete-function slots :initform))
	       default-delete-function))
	 ;; WRAP-DATA-FUNCTION
	 (default-wrap-data-function
	   `(lambda (data &rest args) (apply #'wasm-object-wrap ',name data args)))
	 #|
	 (wrap-data-function
	   (or (wasm-rt/util:unsharp-quote
		(wasm-rt/util:find-slot-option 'wrap-data-function slots :initform))
	       default-wrap-data-function))
	 |#)
    `(progn
       ;; MAKE-WASM-VEC
       (defun ,(alexandria:symbolicate 'make- class-name) (&key size initial-contents parent owner)
	 (%make-wasm-vec ',class-name
			 ',vec-type
			 ',data-type
			 #',new-empty-function
			 #',new-uninitialized-function
			 #',new-function
			 #',delete-function
			 :size size
			 :initial-contents initial-contents
			 :parent parent
			 :owner owner))
       ;; CLASS
       (define-wasm-object ,class-name ,(wasm-rt/util:process-supers '(wasm-vec) supers)
	 ,(wasm-rt/util:process-slots
	   `((vec-type :allocation :class
		       :initform ',default-vec-type)
	     (data-type :allocation :class
			:initform ',default-data-type)
	     (delete-function :initform #',default-delete-function)
	     (wrap-data-function :allocation :class
				 :initform #',default-wrap-data-function))
	   slots)
	 ,@options))))
