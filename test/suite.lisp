(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test)

(defparameter *test-wasm-binary-path* "test/data/test.wasm")

(defvar *test-wasm-binary*)

(5am:def-fixture test-module-fixture (&optional (path *test-wasm-binary-path*))
  (let* ((*test-wasm-binary* (wasm-rt::load-wasm path)))
    (&body)))
