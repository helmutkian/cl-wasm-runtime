(defpackage #:cl-wasm-runtime.test/fixture/runtime-error
  (:use #:cl)
  (:import-from #:fiveam)
  (:import-from #:cffi)
  (:export #:runtime-error
	   #:*runtime-error-message*
	   #:runtime-does-not-error
	   #:runtime-does-errors))

(in-package #:cl-wasm-runtime.test/fixture/runtime-error)

(defvar *runtime-error-message*)


