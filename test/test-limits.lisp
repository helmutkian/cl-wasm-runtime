(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.limits
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.limits)

(5am:test test-make-wasm-limits
  (let* ((min 2)
	 (max 8)
	 (limits (make-wasm-limits 2 8)))
    (5am:is (= min (minimum limits)))
    (5am:is (= max (maximum limits)))))
