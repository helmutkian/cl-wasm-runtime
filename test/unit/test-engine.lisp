(defpackage #:cl-wasm-runtime.test/engine
  (:use #:cl)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime.test/suite)
  (:import-from #:cl-wasm-runtime/config)
  (:import-from #:cl-wasm-runtime/engine))

(5am:def-suite cl-wasm-runtime-test/engine :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test)

(5am:test test-make-wasm-engine
  (5am:finishes (make-wasm-engine)))

