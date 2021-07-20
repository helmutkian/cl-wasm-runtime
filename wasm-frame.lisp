(in-package #:cl-wasm-runtime)

(define-wasm-own frame)
(define-wasm-vec frame)

(cffi:defcfun "wasm_frame_copy" %wasm-frame-type ; own
  (frame %wasm-frame-type))

(cffi:defcfun "wasm_frame_instance" :pointer ; %wasm-instance-type
  (frame %wasm-frame-type))

(cffi:defcfun "wasm_frame_func_index" :uint32
  (frame %wasm-frame-type))

(cffi:defcfun "wasm_frame_func_offset" %size-type
  (frame %wasm-frame-type))

(cffi:defcfun "wasm_frame_module_offset" %size-type
  (frame %wasm-frame-type))

(define-wasm-object-class frame)

(defun wrap-wasm-frame (pointer &key owner)
  (enable-gc (make-instance 'wasm-frame
			    :pointer pointer
			    :owner owner)))

(define-wasm-vec-class frame ()
  ((wrap-data-function :allocation :class
		       :initform #'wrap-wasm-frame)))
