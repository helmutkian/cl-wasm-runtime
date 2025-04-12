(defpackage #:cl-wasm-runtime.test/fixture/gc
  (:use #:cl)
  (:import-from #:trivial-garbage)
  (:import-from #:fiveam)
  (:export #:*gc-enabled?*
	   #:gc-fixture))

(in-package #:cl-wasm-runtime.test/fixture/gc)

(defvar *gc-enabled?* t)

(5am:def-fixture gc-fixture ()
  (&body)
  ;(when *gc-enabled?* (tg:gc :full t :verbose t))
  )
