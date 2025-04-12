(uiop:define-package #:cl-wasm-runtime.test/fixture/module
  (:use #:cl)
  (:use-reexport #:cl-wasm-runtime.test/fixture/store)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime)
  (:export #:*test-wasm-binary-path*
	   #:*test-wasm-binary*
	   #:*module*
	   #:test-wasm-binary-fixture
	   #:module-fixture))

(in-package #:cl-wasm-runtime.test/fixture/module)

(defparameter *test-wasm-binary-path* "test/data/test.wasm")

(defvar *test-wasm-binary*)

(defvar *module*)

(5am:def-fixture test-wasm-binary-fixture (&optional (path *test-wasm-binary-path*))
  (let ((*test-wasm-binary-path* path)
	(*test-wasm-binary* (wasm-rt:load-wasm path)))
    (&body)))

(5am:def-fixture module-fixture (&key (path *test-wasm-binary-path*) config)
  (let* ((*test-wasm-binary-path* path)
	 (*test-wasm-binary* (wasm-rt:load-wasm path))
	 (*engine* (wasm-rt:make-wasm-engine config))
	 (*store* (wasm-rt:make-wasm-store *engine*))
	 (*module* (wasm-rt:make-wasm-module *store* *test-wasm-binary*)))
    (&body)))
