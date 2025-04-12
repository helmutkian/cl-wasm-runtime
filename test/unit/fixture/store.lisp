(uiop:define-package #:cl-wasm-runtime.test/fixture/store
  (:use #:cl)
  (:use-reexport #:cl-wasm-runtime.test/fixture/engine)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime)
  (:export #:*store*
	   #:store-fixture))

(in-package #:cl-wasm-runtime.test/fixture/store)

(defvar *store*)

(5am:def-fixture store-fixture (&optional config)
  (let* ((*engine* (wasm-rt:make-wasm-engine config))
	 (*store* (wasm-rt:make-wasm-store *engine*)))
    (&body)))
