(in-package #:cl-wasm-runtime)

(define-wasm-ref memory)

(cffi:define-foreign-type %wasm-memory-pages ()
  ()
  (:simple-parser %wasm-memory-pages-type)
  (:actual-type :uint32))

(defconstant +memory-page-size+ #x10000)

(defun bytes (pages)
  (* pages +memory-page-size+))

(cffi:defcfun "wasm_memory_new" %wasm-memory-type
  (store %wasm-store-type)
  (memorttype %wasm-memorytype-type))

(cffi:defcfun "wasm_memory_type" %wasm-memorytype-type ;own
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_data" (:pointer %wasm-byte-type)
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_data_size" %size-type
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_size" %wasm-memory-pages-type
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_grow" :boolean
  (memory %wasm-memory-type)
  (delta %wasm-memory-pages-type))

(define-wasm-object-class memory)

(defclass wasm-memory-buffer ()
  ((pointer :initarg :pointer
	    :reader pointer)
   (memory :initarg :memory
	   :reader memory)))

(defmethod size ((buffer wasm-memory-buffer))
  (buffer-size (memory buffer)))

(defun buffer-aref (buffer index)
  (unless (< index (size buffer))
    (error "Out of bounds"))
  (cffi:mem-aref (pointer buffer) '%wasm-byte-type index))

(defmethod (setf buffer-aref) (value (buffer wasm-memory-buffer) index)
  (unless (< index (size buffer))
    (error "Out of bounds"))
  (setf (cffi:mem-aref (pointer buffer) '%wasm-byte-type index) value))

(defun buffer-to-octets (buffer &key (start 0) end)
  (let ((size (size buffer)))
    (when (or (< start 0)
	      (> start (1- size))
	      (and end
		   (or (< end start)
		       (> end size))))
      (error "Out of bounds"))
    (fast-io:with-fast-output (out :vector)
      (loop for i from start below (or end size)
	    for byte = (buffer-aref buffer i)
	    do (fast-io:fast-write-byte byte out)))))

(defun buffer-to-string (buffer &key (start 0) end)
  (babel:octets-to-string (buffer-to-octets buffer :start start :end end)))

(defun make-wasm-memory (store memorttype)
  (enable-gc (make-instance 'wasm-memory
			    :pointer (%wasm-memory-new store memorttype))))

(defun wrap-wasm-memory (pointer &key owner)
  (enable-gc (make-instance 'wasm-memory
			    :pointer pointer
			    :owner owner)))

(defun buffer (memory)
  (make-instance 'wasm-memory-buffer
		 :pointer (%wasm-memory-data memory)
		 :memory memory))

(defun buffer-size (memory)
  (%wasm-memory-data-size memory))

(defmethod size ((memory wasm-memory))
  (%wasm-memory-size memory))

(defun grow (memory pages-delta)
  (%wasm-memory-grow memory pages-delta))

(defun memory-type (memory)
  (enable-gc (wrap-wasm-memorytype (%wasm-memory-type memory))))
