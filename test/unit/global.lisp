(defpackage #:cl-wasm-runtime.test/global
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/instance)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/global)

(5am:def-suite* cl-wasm-runtime-test/global :in cl-wasm-runtime-test)

(5am:test test-global-type
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-global.wasm")
    (5am:finishes 
      (let* ((global (wasm-rt:exports-get *exports* "x"))
	     (globaltype (wasm-rt:global-type global)))
	(5am:is (eql :wasm-i32 (wasm-rt:globaltype-type globaltype)))
	(5am:is-true (wasm-rt:globaltype-mutable? globaltype))))))

(5am:test test-global-mutable
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-global.wasm")
    (5am:finishes
      (let* ((x (wasm-rt:exports-get *exports* "x"))
	     (x-globaltype (wasm-rt:global-type x))
	     (y (wasm-rt:exports-get *exports* "y"))
	     (y-globaltype (wasm-rt:global-type y))
	     (z (wasm-rt:exports-get *exports* "z"))
	     (z-globaltype (wasm-rt:global-type z)))
	(5am:is-true (wasm-rt:globaltype-mutable? x-globaltype))
	(5am:is-true (wasm-rt:globaltype-mutable? y-globaltype))
	(5am:is-false (wasm-rt:globaltype-mutable? z-globaltype))))))

(5am:test test-global-value
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-global.wasm")
    (5am:finishes
      (let ((global (wasm-rt:exports-get *exports* "x")))
	(5am:is (= 0 (wasm-rt:global-value global)))
	(setf (wasm-rt:global-value global) 42)
	(5am:is (= 42 (wasm-rt:global-value global)))))))

(5am:test test-global-value-with-exported-funcs
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-global.wasm")
    (5am:finishes
      (let ((x (wasm-rt:exports-get *exports* "x"))
	    (get-x (wasm-rt:exports-get *exports* "get_x"))
	    (increment-x (wasm-rt:exports-get *exports* "increment_x")))
	(5am:is-true (zerop (wasm-rt:global-value x)))
	(setf (wasm-rt:global-value x) 1)
	(5am:is (= 1 (wasm-rt:wasm-funcall get-x)))
	(wasm-rt:wasm-funcall increment-x)
	(5am:is (= 2 (wasm-rt:wasm-funcall get-x)))))))

(5am:test test-global-value-not-mutable
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-global.wasm")
    (5am:finishes
      (let ((global (wasm-rt:exports-get *exports* "z")))
	(5am:is-false (wasm-rt:globaltype-mutable? (wasm-rt:global-type global)))
	(5am:is (= 42 (wasm-rt:global-value global)))
	(5am:signals wasm-rt/error:wasm-error
	  (setf (wasm-rt:global-value global) 84))
	(5am:is (= 42 (wasm-rt:global-value global)))))))
