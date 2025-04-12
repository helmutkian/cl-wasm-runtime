(defpackage #:cl-wasm-runtime.internal/byte-vec
  (:nicknames #:wasm-rt/byte-vec)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector)
  (:import-from #:cffi)
  (:import-from #:fast-io)
  (:import-from #:babel)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:unsigned-to-signed
		#:signed-to-unsigned
		#:prog1-let
		#:safe-bind)
  (:export #:wasm-byte
	   #:wasm-byte-vec
	   #:wasm-name
	   #:make-wasm-name
	   #:wasm-message
	   #:make-wasm-message
	   #:copy-wasm-byte-vec
	   #:octets-to-wasm-byte-vec
	   #:wasm-byte-vec-to-octets
	   #:string-to-wasm-byte-vec
	   #:wasm-byte-vec-to-string))

(in-package #:cl-wasm-runtime.internal/byte-vec)

(deftype wasm-byte ()
  `(signed-byte 8))

(define-wasm-vec wasm-byte ()
  ((wrap-data-function :allocation :class
		       :initform (lambda (byte &key owner)
				   (declare (ignorable owner))
				   byte)))
  (:documentation "A sequence of bytes. Used to transfer data to functions."))

(define-wasm-object wasm-name (wasm-byte-vec)
  ()
  (:documentation "A sequence of bytes that represents a string name of an object."))

(defun make-wasm-name (name-string)
  "Creates a new WASM-NAME instance.

Syntax:
(MAKE-WASM-NAME name-string) => name

name-string - A STRING to be converted
name - WASM-NAME instance"
  (cffi:with-foreign-string (c-str name-string)
    (safe-bind ((pointer (cffi:foreign-alloc 'wasm-ffi:wasm-name-t) #'cffi:foreign-free))
      (with-new-wasm-object (wasm-name :dynamic? t)
	(wasm-ffi:wasm-name-new-from-string (out pointer) (own c-str))))))

(define-wasm-object wasm-message (wasm-name)
  ((delete-function :initform #'wasm-ffi:wasm-name-delete))
  (:documentation "A sequence of bytes that represents a null-terminated string."))

(defun make-wasm-message (message)
    "Creates a new WASM-MESSAGE instance.

Syntax:
(MAKE-WASM-MESSAGE message-string) => message

message-string - A STRING to be converted
message - WASM-MESSAGE instance"
  (cffi:with-foreign-string (c-str message)
    (safe-bind ((pointer (cffi:foreign-alloc 'wasm-ffi:wasm-message-t) #'cffi:foreign-free))
      (with-new-wasm-object ((wasm-message wasm-name) :dynamic? t)
	(wasm-ffi:wasm-name-new-from-string-nt (out pointer) (own c-str))))))

(defun copy-wasm-byte-vec (byte-vec)
  "Copies all the bytes from one sequence of bytes into a new WASM-BYTE-VEC instance.

Syntax:
(COPY-WASM-BYTE-VEC byte-vec) => new-byte-vec

byte-vec - A STRING, (VECTOR (UNSIGNED-BYTE 8) *), CFFI:FOREIGN-POINTER, or WASM-BYTE-VEC representing the source of bytes to be copied
new-byte-vec - A new WASM-BYTE-VEC with bytes copied over"
  (etypecase byte-vec
    (string
     (string-to-wasm-byte-vec byte-vec))
    ((vector (unsigned-byte 8) *)
     (octets-to-wasm-byte-vec byte-vec))
    ((pointer wasm-byte-vec)
     (prog1-let
	 (new-vec (make-wasm-byte-vec :size (wasm-vec-size byte-vec
							   '(:struct wasm-ffi:wasm-byte-vec-t))))
       (wasm-ffi:wasm-byte-vec-copy new-vec byte-vec)))))

(defun octets-to-wasm-byte-vec (octets &key null-terminated owner)
  "Converts a (VECTOR (UNSIGNED-BYTE 8) *) to a WASM-BYTE-VEC instance

Syntax:
(OCTETS-TO-WASM-BYTE-VEC octets &key null-terminated owner) => byte-vec

octets - (VECTOR (UNSIGNED-BYTE 8) *) to be converted
null-terminated - BOOLEAN that indicates if the argument ends with a null byte
owner - WASM-OBJECT that would own the resulting WASM-BYTE-VEC
byte-vec - A new WASM-BYTE-VEC instance"
  (check-type octets (vector (unsigned-byte 8) *))
  (let* ((size (length octets))
	 (byte-vec (make-wasm-byte-vec :size (+ size (if null-terminated 1 0))
				       :owner owner)))
    (cffi:with-foreign-slots (((data wasm-ffi:data)) (pointer byte-vec) (:struct wasm-ffi:wasm-byte-vec-t))
      (fast-io:with-fast-input (buffer octets)
	(loop for i below size
	      for unsigned-byte = (fast-io:fast-read-byte buffer)
	      do (setf (cffi:mem-aref data 'wasm-ffi:wasm-byte-t i)
		       (unsigned-to-signed unsigned-byte 8))
	      finally (when null-terminated
			(setf (cffi:mem-aref data 'wasm-ffi:wasm-byte-t (1+ i)) 0))))
      byte-vec)))

(defun wasm-byte-vec-to-octets (byte-vec &key null-terminated)
  "Converts a WASM-BYTE-VEC instance to a (VECTOR (UNSIGNED-BYTE 8) *).

Syntax:
(WASM-BYTE-VEC-TO-OCTETS byte-vec &key null-terminated) => octet-vector

byte-vec - WASM-BYTE-VEC instance to be converted
null-terminated - BOOLEAN that indicates if the argument ends with a null byte
octet-vector - A new (VECTOR (UNSIGNED-BYTE 8) *)"
  (cffi:with-foreign-slots (((size wasm-ffi:size) (data wasm-ffi:data))
			    (etypecase byte-vec
			      (cffi:foreign-pointer byte-vec)
			      (wasm-byte-vec (pointer byte-vec)))
			    (:struct wasm-ffi:wasm-byte-vec-t))
    (fast-io:with-fast-output (buffer :vector)
      (loop for i below (- size (if null-terminated 1 0))
	    for byte = (signed-to-unsigned (cffi:mem-aref data 'wasm-ffi:wasm-byte-t i)
					   8)
	    do (fast-io:fast-write-byte byte buffer)))))

(defun string-to-octets (str)
  ;; TODO: Should be UTF-8?
  (babel:string-to-octets str))

(defun string-to-wasm-byte-vec (str &key null-terminated owner)
    "Converts a STRING to a WASM-BYTE-VEC instance

Syntax:
(STRING-TO-WASM-BYTE-VEC str &key null-terminated owner) => byte-vec

str - A STRING to be converted
null-terminated - A BOOLEAN that indicates if the argument ends with a null byte
owner - A WASM-OBJECT that would own the resulting WASM-BYTE-VEC
byte-vec - A new WASM-BYTE-VEC instance"
  (octets-to-wasm-byte-vec (string-to-octets str)
			   :null-terminated null-terminated
			   :owner owner))

(defun wasm-byte-vec-to-string (byte-vec &key null-terminated)
  "Converts a WASM-BYTE-VEC instance to a STRING.

Syntax:
(WASM-BYTE-VEC-TO-OCTETS byte-vec &key null-terminated) => str

byte-vec - WASM-BYTE-VEC instance to be converted
null-terminated - BOOLEAN that indicates if the argument ends with a null byte
str - A new STRING"
  (babel:octets-to-string (wasm-byte-vec-to-octets byte-vec :null-terminated null-terminated)))

(defmethod cffi:translate-to-foreign ((str string) (type wasm-ffi:wasm-byte-vec-ptr-type))
  (pointer (string-to-wasm-byte-vec str)))

(defmethod cffi:translate-to-foreign ((str string) (type wasm-ffi:wasm-message-ptr-type))
  (pointer (string-to-wasm-byte-vec str :null-terminated t)))

(defmethod cffi:translate-to-foreign ((octets simple-array) (type wasm-ffi:wasm-byte-vec-ptr-type))
  (pointer (octets-to-wasm-byte-vec octets)))

(defmethod cffi:translate-to-foreign ((octets simple-array) (type wasm-ffi:wasm-message-ptr-type))
  (pointer (octets-to-wasm-byte-vec octets :null-terminated t)))

(defun byte-vec-eq? (byte-vec-a byte-vec-b)
  (do-wasm-vec+ ((c i) byte-vec-a t)
    (unless (= c (wasm-vec-aref+ byte-vec-b i))
      (return-from byte-vec-eq? nil))))

;; WASM-BYTE-VEC == WASM-BYTE-VEC
(defmethod wasm-object-eq? ((byte-vec-a wasm-byte-vec) (byte-vec-b wasm-byte-vec))
  (byte-vec-eq? byte-vec-a byte-vec-b))

;; OCTET-VECTOR == WASM-BYTE-VEC
(defmethod wasm-object-eq? ((byte-vec-a vector) (byte-vec-b wasm-byte-vec))
  (and (typep byte-vec-a '(vector (unsigned-byte 8) *))
       (every #'= byte-vec-a (wasm-byte-vec-to-octets byte-vec-b))))

;; WASM-BYTE-VEC == OCTET-VECTOR
(defmethod wasm-object-eq? ((byte-vec-a wasm-byte-vec) (byte-vec-b vector))
  (and (typep byte-vec-b '(vector (unsigned-byte 8) *))
       (every #'= (wasm-byte-vec-to-octets byte-vec-a) byte-vec-b)))

;; WASM-NAME == STRING
(defmethod wasm-object-eq? ((name-a wasm-name) (name-b string))
  (string= (wasm-byte-vec-to-string name-a) name-b))

;; STRING == WASM-NAME
(defmethod wasm-object-eq? ((name-a string) (name-b wasm-name))
  (string= name-a (wasm-byte-vec-to-string name-b)))

;; WASM-MESSAGE == WASM-MESSAGE
(defmethod wasm-object-eq? ((byte-vec-a wasm-message) (byte-vec-b wasm-message))
  (byte-vec-eq? byte-vec-a byte-vec-b))

;; WASM-MESSAGE == STRING
(defmethod wasm-object-eq? ((byte-vec-a wasm-message) (byte-vec-b string))
  (string= (wasm-byte-vec-to-string byte-vec-a :null-terminated t)
	   byte-vec-b))

;; STRING == WASM-MESSAGE
(defmethod wasm-object-eq? ((byte-vec-a string) (byte-vec-b wasm-message))
  (string= byte-vec-a
	   (wasm-byte-vec-to-string byte-vec-b :null-terminated t)))

