(defpackage #:cl-wasm-runtime.test/memorytype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/memorytype)

(5am:def-suite cl-wasm-runtime-test/memorytype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test)

(5am:test test-make-wasm-memorytype
  (5am:finishes
    (let* ((min 1)
	   (max 7)
	   (memorytype (wasm-rt:make-wasm-memorytype min max)))
      (multiple-value-bind (limits-min limits-max)
	  (wasm-rt:memorytype-limits memorytype)
	(5am:is (= min limits-min))
	(5am:is (= max limits-max))))))
