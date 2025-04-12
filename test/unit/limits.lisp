(defpackage #:cl-wasm-runtime.test/limits
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/limits)

(5am:def-suite cl-wasm-runtime-test/limits :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/limits)

(5am:test test-make-wasm-limits
  (5am:finishes
    (let* ((min 1)
	   (max 7)
	   (limits (wasm-rt:make-wasm-limits min :max max)))
      (5am:is (= min (wasm-rt:limits-min limits)))
      (5am:is (= max (wasm-rt:limits-max limits))))))

(5am:test test-limits-unbounded
  (5am:finishes
    (let* ((bounded-limits (wasm-rt:make-wasm-limits 1 :max 7))
	   (unbounded-limits (wasm-rt:make-wasm-limits 1)))
      (5am:is-false (wasm-rt:limits-unbounded? bounded-limits))
      (5am:is-true (wasm-rt:limits-unbounded? unbounded-limits)))))

(5am:test test-limits-wasm-object-eq?
  (let ((limits (wasm-rt:make-wasm-limits 1 :max 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? limits limits))
    (5am:is-true (wasm-rt:wasm-object-eq? limits (wasm-rt:make-wasm-limits 1 :max 7)))
    (5am:is-false (wasm-rt:wasm-object-eq? limits (wasm-rt:make-wasm-limits 2 :max 8)))))
