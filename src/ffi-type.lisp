(uiop:define-package #:cl-wasm-runtime.prelude/ffi-type
  (:nicknames #:wasm-rt/ffi-type)
  (:import-from #:cffi)
  (:import-from #:alexandria)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:process-slots
		#:process-supers)
  (:export #:*wasm-ptr-types*
	   #:wasm-ptr-type
	   #:wasm-vec-ptr-type))

(in-package #:cl-wasm-runtime.prelude/ffi-type)

(defparameter *wasm-ptr-types* '())

(cffi:define-foreign-type wasm-ptr-type ()
  ()
  (:actual-type :pointer))

(cffi:define-foreign-type wasm-vec-ptr-type (wasm-ptr-type)
  ())

(defmacro define-wasm-type (name &optional supers slots &rest options)
  (let ((foreign-type-sym (alexandria:symbolicate 'wasm- name '-ptr-type))
	(ctype-name (format nil "WASM-~A-T" name))
	(parser-sym (alexandria:symbolicate 'wasm- name '-t-ptr)))
    `(progn
       (pushnew (list ,ctype-name ',parser-sym ',foreign-type-sym) *wasm-ptr-types*)
       (cffi:define-foreign-type ,foreign-type-sym
	   ,(process-supers '(wasm-ptr-type) supers)
	 ,(process-slots `((%ctype :reader ctype
				   :initform ',ctype-name))
			 slots)
	 (:actual-type :pointer)
	 (:simple-parser ,parser-sym)
	 ,@options))))

(defmacro define-wasm-vec-type (name &optional supers slots &rest options)
  `(define-wasm-type ,(alexandria:symbolicate name '-vec)
       ,(process-supers '(wasm-vec-ptr-type) supers)
     ,(process-slots `((%data-ctype :reader data-type
				    :initform ',(format nil "WASM-~A-T" name)))
		     slots)
     ,@options))

(define-wasm-vec-type byte)
(define-wasm-type name) ; Alias of wasm_byte_vector_t
(define-wasm-type message) ; Alias of wasm_name_t but always null terminated
(define-wasm-type config)
(define-wasm-type engine)
(define-wasm-type store)
(define-wasm-type limits)
(define-wasm-type valtype)
(define-wasm-vec-type valtype)
(define-wasm-type functype)
(define-wasm-vec-type functype)
(define-wasm-type globaltype)
(define-wasm-vec-type globaltype)
(define-wasm-type tabletype)
(define-wasm-vec-type tabletype)
(define-wasm-type memorytype)
(define-wasm-vec-type memorytype)
(define-wasm-type externtype)
(define-wasm-vec-type externtype)
(define-wasm-type importtype)
(define-wasm-vec-type importtype)
(define-wasm-type exporttype)
(define-wasm-vec-type exporttype)
(define-wasm-type val)
(define-wasm-vec-type val)
(define-wasm-type ref)
(define-wasm-type frame)
(define-wasm-vec-type frame)
(define-wasm-type trap)
(define-wasm-type foreign)
(define-wasm-type module)
(define-wasm-type shared-module)
(define-wasm-type func)
(define-wasm-type global)
(define-wasm-type table)
(define-wasm-type memory)
(define-wasm-type extern)
(define-wasm-vec-type extern)
(define-wasm-type instance)

(dolist (type *wasm-ptr-types*)
  (export (list (second type) (third type))))
