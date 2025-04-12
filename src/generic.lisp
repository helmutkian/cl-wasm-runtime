(defpackage #:cl-wasm-runtime.internal/generic
  (:use #:cl)
  (:nicknames #:wasm-rt/generic)
  (:export #:size
	   ; #:value
	   #:imports
	   #:exports
	   #:value-type
	   #:extern-type
	   #:limits
	   #:name
	   #:to-ref
	   #:from-ref
	   #:same?))

(in-package #:cl-wasm-runtime.internal/generic)


