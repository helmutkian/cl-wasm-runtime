(defpackage #:cl-wasm-runtime.internal/store
  (:nicknames #:wasm-rt/store)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/engine)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-store
	   #:make-wasm-store))

(in-package #:cl-wasm-runtime.internal/store)

(define-wasm-object wasm-store ()
  ()
  (:documentation "Contains all global state that can be manipulated by wasm code. Multiple WASM-STORE intances can be created within the same WASM-ENGINE. Each WASM-STORE should be thread-local and neither it nor its contained state should be shared across threads. A WASM-STORE cannot live longer than its containing WASM-ENGINE."))

(defun make-wasm-store (engine)
  "Creates a new WASM-STORE instance.

Syntax:

(MAKE-WASM-STORE engine) => store

engine - WASM-ENGINE in which the store will be created
store - WASM-STORE instance"
  (check-type engine wasm-engine)
  (with-new-wasm-object wasm-store
    (wasm-ffi:wasm-store-new (parent* engine))))
