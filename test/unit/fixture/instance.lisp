(uiop:define-package #:cl-wasm-runtime.test/fixture/instance
  (:use #:cl)
  (:use-reexport #:cl-wasm-runtime.test/fixture/module)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime)
  (:export *instance*
	   *exports*
	   instance-fixture
	   instance-exports-fixture))

(in-package #:cl-wasm-runtime.test/fixture/instance)

(defvar *instance*)
(defvar *exports*)

(5am:def-fixture instance-fixture (&key config (path *test-wasm-binary-path*) imports)
 (let* ((*engine* (wasm-rt:make-wasm-engine config))
	(*store* (wasm-rt:make-wasm-store *engine*))
	(*test-wasm-binary* (wasm-rt:load-wasm path))
	 (*module* (wasm-rt:make-wasm-module *store* *test-wasm-binary*))
	(*instance* (wasm-rt:make-wasm-instance *store* *module* imports)))
   (&body)))

(5am:def-fixture instance-exports-fixture (&key config (path *test-wasm-binary-path*) imports)
 (let* ((*engine* (wasm-rt:make-wasm-engine config))
	(*store* (wasm-rt:make-wasm-store *engine*))
	(*test-wasm-binary* (wasm-rt:load-wasm path))
	(*module* (wasm-rt:make-wasm-module *store* *test-wasm-binary*))
	(*instance* (wasm-rt:make-wasm-instance *store* *module* imports))
	(*exports* (wasm-rt:instance-exports *instance*)))
    (&body)))
