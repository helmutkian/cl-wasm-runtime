(defpackage #:cl-wasm-runtime.test/fixture/engine
  (:use #:cl)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime)
  (:export #:*engine*
	   #:engine-fixture))

(in-package #:cl-wasm-runtime.test/fixture/engine)

(defvar *engine*)

(5am:def-fixture engine-fixture (&optional config)
  (let ((*engine* (wasm-rt:make-wasm-engine config)))
    (&body)))
