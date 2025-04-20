(defpackage #:cl-wasm-runtime.internal/memory
  (:nicknames #:wasm-rt/memory)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/byte-vec
	#:cl-wasm-runtime.internal/memorytype)
  (:import-from #:cffi)
  (:import-from #:fast-io)
  (:import-from #:babel)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:with-memoized-slot
		#:unsigned-to-signed)
  (:export #:wasm-memory
	   #:make-wasm-memory
	   #:memory-type
	   #:memory-size
	   #:memory-data-size
	   #:memory-data
	   #:memory-grow
	   #:memory-buffer
	   #:memory-buffer-size
	   #:memory-buffer-aref
	   #:memory-buffer-to-octets
	   #:memory-buffer-to-string))

(in-package #:cl-wasm-runtime.internal/memory)

(defun %required (initarg)
  (error 'wasm-rt/error:required-initarg-error :initarg initarg))

(define-wasm-object wasm-memory ()
  ((%memorytype)
   (%buffer)))

(defstruct (memory-buffer (:constructor %make-memory-buffer)
			  (:conc-name %memory-buffer-))
  "A wrapper for a WASM-MEMORY object's data buffer.

POINTER - Foreign array of WASM-BYTE-T/(SIGNED-BYTE 8)
MEMORY - Reference to its containing WASM-MEMORY object"
  (pointer (%required :pointer) :type cffi:foreign-pointer :read-only t)
  (memory (%required :pointer) :type wasm-memory :read-only t))

(defun make-wasm-memory (store memorytype)
  (check-type store wasm-store)
  (let ((memorytype* (etypecase memorytype
		       ((pointer wasm-memorytype) memorytype)
		       ((and cons list) (apply #'make-wasm-memorytype memorytype))))) 
    (with-new-wasm-object wasm-memory
      (wasm-ffi:wasm-memory-new (parent* store) memorytype*))))

(defun memory-type (memory)
  (check-type memory wasm-memory)
  (with-memoized-slot (%memorytype memory)
    (with-new-wasm-object wasm-memorytype
      (wasm-ffi:wasm-memory-type memory))))

(defun memory-size (memory)
  (check-type memory (pointer wasm-memory))
  (wasm-ffi:wasm-memory-size memory))

(defun memory-data-size (memory)
  (check-type memory (pointer wasm-memory))
  (wasm-ffi:wasm-memory-data-size memory))

(defun memory-data (memory)
  (check-type memory wasm-memory)
  (with-memoized-slot (%buffer memory)
    (%make-memory-buffer :pointer (wasm-ffi:wasm-memory-data memory)
			 :memory memory)))

(defun memory-buffer-size (memory-buffer)
  (check-type memory-buffer memory-buffer)
  (memory-data-size (%memory-buffer-memory memory-buffer)))

(defun memory-buffer-aref (memory-buffer &optional (index 0))
  (check-type memory-buffer memory-buffer)
  (unless (and (>= index 0)
	       (< index (memory-buffer-size memory-buffer)))
    (wasm-rt/error:wasm-simple-error "Index ~A into MEMORY-BUFFER ~S is out of bounds."
				     index
				     memory-buffer))
  (cffi:mem-aref (%memory-buffer-pointer memory-buffer) 'wasm-ffi:wasm-byte-t index))

(defun (setf memory-buffer-aref) (value memory-buffer &optional (index 0))
  (check-type value (signed-byte 8))
  (check-type memory-buffer memory-buffer)
  (unless (and (>= index 0)
	       (< index (memory-buffer-size memory-buffer)))
    (wasm-rt/error:wasm-simple-error "Index ~A into WASM-MEMORY ~S is out of bounds."
				     index
				     memory-buffer))
  (setf (cffi:mem-aref (%memory-buffer-pointer memory-buffer) 'wasm-ffi:wasm-byte-t index)
	value))

(defun memory-buffer-to-octets (memory-buffer &key (start 0) end)
  (check-type memory-buffer memory-buffer)
  (let ((%end (or end (memory-buffer-size memory-buffer))))
    (unless (and (<= start %end)
		 (>= start 0)
		 (<= %end (memory-buffer-size memory-buffer))) ; end is exclusive
      (wasm-rt/error:wasm-simple-error "Index ~A into WASM-MEMORY ~S is out of bounds."
				       (list start %end)
				       memory-buffer))
    (fast-io:with-fast-output (out :vector)
      (loop for i from start below %end
	    for byte = (cffi:mem-aref (%memory-buffer-pointer memory-buffer)
				      'wasm-ffi:wasm-byte-t
				      i)
	    do (fast-io:fast-write-byte (unsigned-to-signed byte 8) out)))))

(defun memory-buffer-to-string (memory-buffer &key (start 0) end)
  (babel:octets-to-string (memory-buffer-to-octets memory-buffer :start start :end end)))

(defun memory-grow (memory delta)
  (check-type memory (pointer wasm-memory))
  (check-type delta (unsigned-byte 32))
  (wasm-ffi:wasm-memory-grow memory delta))
