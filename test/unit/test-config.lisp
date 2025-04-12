(defpackage #:cl-wasm-runtime/test/config
  (:use #:cl)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime/test/suite
		#:cl-wasm-runtime/test))

(in-package #:cl-wasm-runtime/test/config)

(5am:def-suite* cl-wasm-runtime/test/config :in cl-wasm-runtime/test)

(5am:test test-make-wasm-config
  (5am:finishes
    (let ((config (wasm-rt/config:make-wasm-config)))
      (5am:is-true (typep config 'wasm-rt/config:wasm-config)))))
