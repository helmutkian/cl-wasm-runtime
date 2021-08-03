(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.trap
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.trap)

(5am:test test-make-wasm-trap
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let ((trap (make-wasm-trap *store* "test")))
	(5am:is-false (null? trap))
	(5am:is (string= "test" (message trap)))
	(5am:is-true (null? (origin trap)))
	(5am:is-false (null? (trap-trace trap)))
	(5am:is (= 0 (size (trap-trace trap))))))))
