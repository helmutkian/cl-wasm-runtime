(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.engine
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.engine)

(5am:test test-make-wasm-engine
  (5am:with-fixture test-module-fixture ()
    (5am:finishes
      (let* ((engine (make-wasm-engine))
	     (store (make-wasm-store engine))
	     (module (make-wasm-module store *test-wasm-binary*))
	     (instance (make-wasm-instance store module))
	     (exports (exports instance))
	     (sum (get-export exports "sum" 'wasm-func)))
	(5am:is (= 42 (wasm-funcall sum 37 5)))))))
