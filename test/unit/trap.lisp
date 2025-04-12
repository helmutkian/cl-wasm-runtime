(defpackage #:cl-wasm-runtime.test/trap
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/store)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/trap)

(5am:def-suite cl-wasm-runtime-test/trap :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/trap)

(5am:test test-make-wasm-trap
  (5am:with-fixture store-fixture ()
    (5am:finishes
      (let* ((message "foobar")
	     (trap (wasm-rt/trap:make-wasm-trap *store* message)))
	(5am:is (string= message (wasm-rt:trap-message trap)))
	(5am:is-true (wasm-rt:null? (wasm-rt:trap-origin trap)))
	(5am:is-true (zerop (length (wasm-rt:trap-trace trap))))))))
