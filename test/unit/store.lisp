(defpackage #:cl-wasm-runtime.test/store
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/gc
	#:cl-wasm-runtime.test/fixture/engine)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/store)

(5am:def-suite cl-wasm-runtime-test/store :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/store)

(5am:test (test-make-wasm-store :fixture gc-fixture)
  (5am:with-fixture engine-fixture ()
    (5am:finishes
      (wasm-rt:make-wasm-store *engine*))))

(5am:test (test-store-wasm-object-eq? :fixture gc-fixture)
  (5am:with-fixture engine-fixture ()
    (let ((store (wasm-rt:make-wasm-store *engine*)))
      (5am:is-true (wasm-rt:wasm-object-eq? store store))
      (5am:is-false (wasm-rt:wasm-object-eq? store (wasm-rt:make-wasm-store *engine*))))))
