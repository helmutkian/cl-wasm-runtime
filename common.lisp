(in-package #:cl-wasm-runtime)

(cffi:define-foreign-library libwasmer
  (:darwin (:or "shared/libwasmer/lib/libwasmer.dylib")))

(cffi:use-foreign-library libwasmer)

;;; UTIL

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun make-object-type-name-sym (name)
    (alexandria:symbolicate 'wasm- name '-type))

  (defun make-object-type-parser-sym (name)
    (alexandria:symbolicate '%wasm- name '-type))

  (defun translate-wasm-name (name)
    (etypecase name
      (string
       (alexandria:symbolicate '% (cffi:translate-underscore-separated-name name)))
      (symbol
       (let ((translated-name (cffi:translate-underscore-separated-name name)))
	 (if (eql (elt translated-name 0) #\%)
	     (subseq translated-name 1)
	     translated-name)))))

  (defun format-wasm-cfun-name (name cfunc-name-suffix)
    (format nil
	    "wasm_~a_~a"
	    (cffi:translate-underscore-separated-name name)
	    cfunc-name-suffix))

  (defmethod cffi:translate-name-from-foreign (foreign-name (package (eql *package*)) &optional varp)
    (let ((name (translate-wasm-name foreign-name)))
      (if varp
	  (alexandria:symbolicate '* name '*)
	  name))))

;;; COMMON

(cffi:defctype %size-type :uint)

(cffi:defctype %float-32-type :float)

(cffi:defctype %float-64-type :double)

(defmacro define-wasm-object-type (name &optional slots)
  "Defines a CFFI foreign type named WASM-{name}-TYPE with parser %WASM-{name}-TYPE"
  (let ((object-name-sym (make-object-type-name-sym name))
	 (parser-sym (make-object-type-parser-sym name)))
    `(cffi:define-foreign-type ,object-name-sym (wasm-object-type)
       ,slots
       (:simple-parser ,parser-sym))))

(defmacro define-wasm-vec (name)
  (let* ((vec-name (make-symbol (format nil "~A-VEC" name)))
	 (type-sym (make-object-type-parser-sym vec-name))
	 (struct-type-sym (alexandria:symbolicate '%wasm- vec-name '-struct))
	 (new-empty-name (format-wasm-cfun-name vec-name "new_empty"))
	 (new-uninitialized-name (format-wasm-cfun-name vec-name "new_uninitialized"))
	 (new-name (format-wasm-cfun-name vec-name "new"))
	 (copy-name (format-wasm-cfun-name vec-name "copy"))
	 (delete-name (format-wasm-cfun-name vec-name "delete")))
    `(progn
       ;; WASM object type
       (define-wasm-object-type ,vec-name)
       ;; Struct
       (cffi:defcstruct ,struct-type-sym
	 (size %size-type)
	 (data :pointer))
       ;; ...-VEC-NEW-EMPTY function
       (cffi:defcfun ,new-empty-name :void
	 (out ,type-sym))
       ;; ...-VEC-NEW-UNINITIALIZED function
       (cffi:defcfun ,new-uninitialized-name :void
	 (out ,type-sym)
	 (size %size-type))
       ;; ...-VEC-NEW function
       (cffi:defcfun ,new-name :void
	 (out ,type-sym)
	 (size %size-type)
	 (init :pointer))
       ;; ...-VEC-COPY function
       (cffi:defcfun ,copy-name :void
	 (out ,type-sym)
	 (src ,type-sym))
       ;; ...-VEC-DELETE function
       (cffi:defcfun ,delete-name :void
	 (vec ,type-sym)))))

(defun wasm-vec-init-empty (wasm-vec vec-struct-type)
  (setf (cffi:foreign-slot-value wasm-vec vec-struct-type 'size)
	0
	(cffi:foreign-slot-value wasm-vec vec-struct-type 'data)
	(cffi:null-pointer)))

(defun wasm-vec-init (wasm-vec vec-struct-type c-array size)
  (setf (cffi:foreign-slot-value wasm-vec vec-struct-type 'size)
	size
	(cffi:foreign-slot-value wasm-vec vec-struct-type 'data)
	c-array))

(defun wasm-vec-size (pointer type)
  (cffi:foreign-slot-value pointer type 'size))

(defun wasm-vec-aref (pointer vec-type offset)
  (cffi:mem-aref (cffi:foreign-slot-value pointer vec-type 'data)
		 :pointer
		 offset))

(defun wasm-vec-aptr (pointer vec-type offset)
  (cffi:mem-aptr (cffi:foreign-slot-value pointer vec-type 'data)
		 :pointer
		 offset))


(defmacro do-wasm-vec (((elm-var elm-type &optional (index-var (gensym))) vec-pointer vec-type) &body body)
  (alexandria:with-gensyms (data size)
    `(let ((,data (cffi:foreign-slot-value ,vec-pointer ,vec-type 'data))
	   (,size (cffi:foreign-slot-value ,vec-pointer ,vec-type 'size)))
       (dotimes (,index-var ,size)
	 (let ((,elm-var (cffi:mem-aref ,data ,elm-type ,index-var)))
	   ,@body)))))

(defmacro define-wasm-own (name)
  (let* ((parser-sym (make-object-type-parser-sym name))
	 (delete-name (format-wasm-cfun-name name "delete")))
    `(progn
       (define-wasm-object-type ,name)
       (cffi:defcfun ,delete-name :void
	 (,name ,parser-sym)))))

(defmacro define-wasm-type (name)
  (let ((type-sym (make-object-type-parser-sym name))
	(copy-name (format-wasm-cfun-name name "copy")))
    `(progn
       (define-wasm-own ,name)
       (define-wasm-vec ,name)
       (cffi:defcfun ,copy-name ,type-sym
	 (,name ,type-sym)))))

(defmacro define-wasm-object-class (name &optional supers slots &rest options)
  (let ((class-name (alexandria:symbolicate 'wasm- name))
	(delete-name (translate-wasm-name (format-wasm-cfun-name name "delete"))))
    `(defclass ,class-name
	 (,@supers ,@(unless (find-if (lambda (super) (subtypep super 'wasm-object))
				      supers)
			'(wasm-object)))
       (,@slots
	,@(unless (or (find 'delete-function slots)
		      (assoc 'delete-function slots))
	    `((delete-function :initform (symbol-function ',delete-name)))))
       ,@options)))	 

;;; Wasm vector high level interface

(defun make-wasm-vec-instance (class-name type &key owner)
  (enable-gc (make-instance class-name
			    :pointer (cffi:foreign-alloc type)
			    :owner owner)))

(defun wrap-wasm-vec (class-name pointer &key owner)
  (enable-gc (make-instance class-name
			    :pointer pointer
			    :owner owner)))

(defmacro define-wasm-vec-class (name)
  (let* ((vec-name (make-symbol (format nil "~A-VEC" name)))
	 (struct-type-sym (alexandria:symbolicate '%wasm- vec-name '-struct))
	 (class-name (alexandria:symbolicate 'wasm- vec-name))
	 (make-empty-name (alexandria:symbolicate 'make-wasm- vec-name '-empty))
	 (new-empty-name (translate-wasm-name (format-wasm-cfun-name vec-name "new_empty")))
	 (make-uninitialized-name (alexandria:symbolicate 'make-wasm- vec-name '-uninitialized))
	 (new-uninitialized-name (translate-wasm-name (format-wasm-cfun-name vec-name "new_uninitialized")))
	 (make-name (alexandria:symbolicate 'make-wasm- vec-name))
	 (new-name (translate-wasm-name (format-wasm-cfun-name vec-name "new")))
	 (copy-name (translate-wasm-name (format-wasm-cfun-name vec-name "copy"))))
    `(progn
       (define-wasm-object-class ,vec-name)
       (defun ,make-empty-name (&key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,new-empty-name vec)
	   vec))
       (defun ,make-uninitialized-name (size &key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,new-uninitialized-name vec size)
	   vec))
       (defun ,make-name (size init-data &key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,new-name vec size init-data)
	   vec))
       (defun ,(alexandria:symbolicate 'wrap-wasm- vec-name) (pointer &key owner)
	 (wrap-wasm-vec ',class-name pointer :owner owner))
       (defun ,(alexandria:symbolicate 'wasm- vec-name '-copy) (src &key owner)
	 (let ((vec (make-wasm-vec-instance ',class-name '(:struct ,struct-type-sym) :owner owner)))
	   (,copy-name vec src)
	   vec))
       (defun ,(alexandria:symbolicate 'wasm- vec-name '-size) (vec)
	 (wasm-vec-size (pointer vec) '(:struct ,struct-type-sym))))))

(defun wasm-vec-to-list (vec vec-type wrap-function &key owner)
  (let ((list nil))
    (do-wasm-vec ((elm-pointer :pointer)
		  (if (cffi:pointerp vec) vec (pointer vec))
		  vec-type)
      (push (funcall wrap-function elm-pointer
		     :owner (or owner
				(unless (cffi:pointerp vec) vec)))
	    list))
    (nreverse list)))

(defun list-to-wasm-vec (list vec-ctr-function elm-copy-function &key owner)
  (let ((size (length list)))
    (cffi:with-foreign-pointer (arr size)
      (loop for elm in list
	    for i from 0
	    do (funcall elm-copy-function
			(cffi:mem-aptr arr :pointer i)
			(pointer elm)))
      (funcall vec-ctr-function size arr :owner owner))))

;;; Byte vectors

(cffi:defctype %wasm-byte-type :uint8)

(define-wasm-vec byte)

(define-wasm-object-class byte)
(define-wasm-vec-class byte)

(cffi:defctype %wasm-message-struct (:struct %wasm-byte-vec-struct))

(cffi:define-foreign-type wasm-message-type (wasm-byte-vec-type)
  ()
  (:simple-parser %wasm-message-type))

(cffi:defctype %wasm-name-struct %wasm-message-struct) ; Null terminated

(cffi:define-foreign-type wasm-name-type (wasm-message-type) ; Null terminated
  ()
  (:simple-parser %wasm-name-type))

(setf (symbol-function '%wasm-name-new) #'%wasm-byte-vec-new)
(setf (symbol-function '%wasm-name-new-empty) #'%wasm-byte-vec-new-empty)
(setf (symbol-function '%wasm-name-new-uninitialized) #'%wasm-byte-vec-new-uninitialized)
(setf (symbol-function '%wasm-name-copy) #'%wasm-byte-vec-copy)
(setf (symbol-function '%wasm-name-delete) #'%wasm-byte-vec-delete)

(defun octets-to-wasm-byte-vec (octets &key null-terminated owner)
  (let* ((size (length octets))
	 (byte-vec (make-wasm-byte-vec-uninitialized (+ size
							(if null-terminated 1 0)))))
    (cffi:with-foreign-slots ((data) (pointer byte-vec) (:struct %wasm-byte-vec-struct))
      (fast-io:with-fast-input (buffer octets)
	(loop for i below size
	      do (setf (cffi:mem-aref data '%wasm-byte-type i)
		       (fast-io:fast-read-byte buffer))
	      finally (when null-terminated
			(setf (cffi:mem-aref data '%wasm-byte-type (1+ i)) 0))))
      (setf (owner byte-vec) owner)
      byte-vec)))

(defun wasm-byte-vec-to-octets (byte-vec &key null-terminated)
  (cffi:with-foreign-slots ((size data)
			    (if (cffi:pointerp byte-vec) byte-vec (pointer byte-vec))
			    (:struct %wasm-byte-vec-struct))
    (fast-io:with-fast-output (buffer :vector)
      (loop for i below (- size (if null-terminated 1 0))
	    for byte = (cffi:mem-aref data '%wasm-byte-type i)
	    do (fast-io:fast-write-byte byte buffer)))))

(defun string-to-wasm-byte-vec (str &key null-terminated owner)
  (cffi:with-foreign-string (c-str str)
    (make-wasm-byte-vec (+ (babel:string-size-in-octets str)
			   (if null-terminated 1 0))
			c-str
			:owner owner)))


(defun wasm-byte-vec-to-string (byte-vec &key null-terminated)
  (babel:octets-to-string (wasm-byte-vec-to-octets byte-vec :null-terminated null-terminated)))
    
(defmethod cffi:translate-to-foreign ((str string) (type wasm-byte-vec-type))
  (pointer (string-to-wasm-byte-vec str)))

(defmethod cffi:translate-to-foreign ((str string) (type wasm-name-type))
  (pointer (string-to-wasm-byte-vec str :null-terminated t)))

(defmethod cffi:translate-to-foreign ((octets simple-array) (type wasm-byte-vec-type))
  (pointer (octets-to-wasm-byte-vec octets)))

(defmethod cffi:translate-to-foreign ((octets simple-array) (type wasm-name-type))
  (pointer (octets-to-wasm-byte-vec octets :null-terminated t)))
