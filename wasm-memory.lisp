(in-package #:cl-wasm-runtime)

(define-wasm-ref memory)

(cffi:define-foreign-type %wasm-memory-pages ()
  ()
  (:simple-parser %wasm-memory-pages-type)
  (:actual-type %uint-32-type))

(defconstant +memory-page-size+ #x10000)

(defun bytes (pages)
  (* pages +memory-page-size+))

(cffi:defcfun "wasm_memory_new" %wasm-memory-type
  (store %wasm-store-type)
  (memorttype %wasm-memorytype-type))

(cffi:defcfun "wasm_memory_type" %wasm-memorytype-type ;own
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_data" %wasm-byte-type
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_data_size" %size-type
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_size" %wasm-memory-pages-type
  (memory %wasm-memory-type))

(cffi:defcfun "wasm_memory_grow" :boolean
  (memory %wasm-memory-type)
  (delta %wasm-memory-pages-type))

(define-wasm-object-class memory)

(defun make-wasm-memory (store memorttype)
  (enable-gc (make-instance 'wasm-memory
			    :pointer (%wasm-memory-new store memorttype))))

(defun wrap-wasm-memory (pointer &key owner)
  (enable-gc (make-instance 'wasm-memory
			    :pointer pointer
			    :owner owner)))

(defun buffer (memory)
  (%wasm-memory-data memory))

(defun buffer-size (memory)
  (%wasm-memory-data-size memory))

(defmethod size ((memory wasm-memory))
  (%wasm-memory-size memory))

(defun grow (memory pages-delta)
  (%wasm-memory-grow memory pages-delta))
