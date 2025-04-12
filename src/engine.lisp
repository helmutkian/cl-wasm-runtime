(defpackage #:cl-wasm-runtime.internal/engine
  (:nicknames #:wasm-rt/engine)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/config)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-engine
	   #:make-wasm-engine))

(in-package #:cl-wasm-runtime.internal/engine)

(define-wasm-object wasm-engine ()
  ()
  (:documentation "Contains all the context and configuration necessary for compiling wasm code. A WASM-ENGINE is typically global and safe to share between threads with a single WASM-ENGINE instance usually created for the lifecycle of your program."))

(defun make-wasm-engine (&optional config)
  "Creates a new WASM-ENGINE instance. 

Syntax: 
(MAKE-WASM-ENGINE [config]) => engine

config - An optional WASM-CONFIG object
engine - WASM-ENGINE instance"
  (with-new-wasm-object wasm-engine
    (if config
	(wasm-ffi:wasm-engine-new-with-config (own config))
	(wasm-ffi:wasm-engine-new))))
