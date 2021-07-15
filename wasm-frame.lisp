(in-package #:cl-wasm-runtime)

(define-wasm-own frame)
(define-wasm-vec frame)

(cffi:defcfun ("wasm_frame_copy" %wasm-frame-copy) %wasm-frame-type ; own
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_instance" %wasm-frame-instance) :pointer ; %wasm-instance-type
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_func_index" %wasm-frame-func-instance) :uint32
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_func_offset" %wasm-frame-func-offset) %size-type
  (frame %wasm-frame-type))

(cffi:defcfun ("wasm_frame_module_offset" %wasm-frame-module-offset) %size-type
  (frame %wasm-frame-type))

(define-wasm-object-class frame)
(define-wasm-vec-class frame)
