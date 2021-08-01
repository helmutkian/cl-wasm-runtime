(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.store
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.store)

(5am:test test-make-wasm-store
  (5am:finishes
    (let* ((engine (make-wasm-engine)))
      (make-wasm-store engine))))
