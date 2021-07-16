(in-package #:cl-wasm-runtime)

(define-wasm-ref foreign)

(cffi:defcfun "wasm_foreign_new" %wasm-foreign-type ; own
  (store %wasm-store-type))

(define-wasm-object-class foreign)
