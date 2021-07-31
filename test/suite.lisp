(in-package #:cl-wasm-runtime.test)

(defparameter *test-wasm-binary-path* "test/data/test.wasm")

(defvar *engine*)

(defvar *store*)

(defvar *module*)

(defvar *instance*)

(defvar *test-wasm-binary*)

(5am:def-fixture test-module-fixture (&optional (path *test-wasm-binary-path*))
  (let* ((*test-wasm-binary* (wasm-rt::load-wasm path)))
    (&body)))

(5am:def-fixture instance-fixture (&optional (bytes (wasm-rt::load-wasm *test-wasm-binary-path*)))
  (let* ((*engine* (make-wasm-engine))
        (*store* (make-wasm-store *engine*))
        (*test-wasm-binary* bytes)
        (*module* (make-wasm-module *store* *test-wasm-binary*))
        (*instance* (make-wasm-instance *store* *module*)))
    (&body)))

(5am:def-fixture engine-store-fixture ()
  (let* ((*engine* (make-wasm-engine))
        (*store* (make-wasm-store *engine*)))
    (&body)))

(5am:def-suite cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test)

