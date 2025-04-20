(uiop:define-package #:cl-wasm-runtime.wasmer-ffi/prelude
  (:use #:cl)
  (:use-reexport #:cl-wasm-runtime.prelude/ffi-prelude))

(in-package #:cl-wasm-runtime.wasmer-ffi/prelude)

(pushnew :wasmer *features*)

