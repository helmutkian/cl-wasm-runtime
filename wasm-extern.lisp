(in-package #:cl-wasm-runtime)

(define-wasm-ref extern)
(define-wasm-vec extern)

(cffi:defcfun ("wasm_extern_kind" %wasm-extern-kind) %wasm-extern-kind-type
  (extern %wasm-extern-type))

(cffi:defcfun ("wasm_extern_type" %wasm-extern-type) %wasm-externtype-type ; own
  (extern %wasm-extern-type))

(defmacro define-wasm-extern-conversion (type)
  (let ((type-name (make-object-type-parser-sym type))
	(as-extern-name (make-cfun-name type "as_extern"))
	(as-type-name (list (format nil "wasm_extern_as_~a" (string-downcase type))
			    (alexandria:symbolicate '%wasm-extern-as- type))))
    `(progn
       (cffi:defcfun ,as-extern-name %wasm-extern-type
	 (,type ,type-name))
       (cffi:defcfun ,as-type-name ,type-name
	 (extern %wasm-extern-type)))))

(define-wasm-extern-conversion func)
(define-wasm-extern-conversion global)
(define-wasm-extern-conversion table)
(define-wasm-extern-conversion memory)

(define-wasm-object-class extern ()
  ((kind :reader wasm-extern-kind)
   (externtype :reader wasm-extern-type)))

(defun wrap-wasm-extern (pointer &key owner)
  (let ((extern (enable-gc (make-instance 'wasm-extern :pointer pointer :owner owner))))
    (setf (slot-value extern 'kind)
	  (wasm-externkind-to-key (%wasm-extern-kind pointer))
	  (slot-value extern 'externtype)
	  (wrap-wasm-externtype (%wasm-extern-type pointer) :owner (owner extern)))
    extern))

(defmacro define-to-wasm-extern-method (type)
  (let* ((class-name (alexandria:symbolicate 'wasm- type))
	 (as-type-name (second (make-cfun-name type "as_extern"))))
    `(defmethod to-wasm-extern ((,type ,class-name))
       (let ((pointer (,as-type-name ,type)))
	 (unless (null? pointer)
	   (wrap-wasm-extern pointer :owner (owner ,type)))))))

(defgeneric to-wasm-extern (object))

(define-to-wasm-extern-method func)
(define-to-wasm-extern-method global)
(define-to-wasm-extern-method table)
(define-to-wasm-extern-method memory)

(defmacro define-from-wasm-extern-method (type)
  (let* ((class-name (alexandria:symbolicate 'wasm- type))
	 (as-extern-name (alexandria:symbolicate '%wasm-extern-as- type))
	 (wrapper-name (alexandria:symbolicate 'wrap- class-name)))
    `(defmethod from-wasm-extern ((extern wasm-extern) (wasm-type (eql ',class-name)))
       (let ((pointer (,as-extern-name extern)))
	 (unless (null? pointer)
	   (,wrapper-name pointer :owner (owner extern)))))))
  
(defgeneric from-wasm-extern (extern type))

(define-from-wasm-extern-method func)
(define-from-wasm-extern-method global)
(define-from-wasm-extern-method table)
(define-from-wasm-extern-method memory)

(define-wasm-vec-class extern)

(defun wasm-extern-vec-to-list (extern-vec &key owner)
  (wasm-vec-to-list extern-vec
		    '(:struct %wasm-extern-vec-struct)
		    #'wrap-wasm-extern
		    :owner (or owner (owner extern-vec))))

(defun list-to-wasm-extern-vec (list &key owner)
  (let ((size (length list)))
    (if (zerop size)
	(make-wasm-extern-vec-empty :owner owner)
	(cffi:with-foreign-pointer (arr size)
	  (loop for elm in list
		for i from 0
		do (setf (cffi:mem-aref arr :pointer i) (pointer elm)))
	  (make-wasm-extern-vec size arr :owner owner)))))
