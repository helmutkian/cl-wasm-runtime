(defpackage #:cl-wasm-runtime.test/engine
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/gc
	#:cl-wasm-runtime.test/fixture/module)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/engine)

(5am:def-suite* cl-wasm-runtime-test/engine :in cl-wasm-runtime-test)

(5am:test (test-make-wasm-engine :fixture gc-fixture)
  (5am:with-fixture test-wasm-binary-fixture ()
    (5am:finishes
      (let* ((engine (wasm-rt:make-wasm-engine))
	     (store (wasm-rt:make-wasm-store engine))
	     (module (wasm-rt:make-wasm-module store *test-wasm-binary*))
	     (instance (wasm-rt:make-wasm-instance store module))
	     (exports (wasm-rt:instance-exports instance))
	     (sum (wasm-rt:exports-get exports "sum")))
	(5am:is (= 42 (wasm-rt:wasm-funcall sum 37 5)))))))

(5am:test test-engine-wasm-object-eq?
  (let ((engine (wasm-rt:make-wasm-engine)))
    (5am:is-true (wasm-rt:wasm-object-eq? engine engine))
    (5am:is-false (wasm-rt:wasm-object-eq? engine (wasm-rt:make-wasm-engine)))))
