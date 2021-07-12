(defpackage #:cl-wasm-runtime
  (:nicknames #:wasm-rt)
  (:use #:cl)
  (:export #:wasm-object
	    #:wasm-delete)
  ;; Bytes
  (:export #:wasm-byte
	    #:wasm-byte-vec)
  ;; Config
  (:export #:wasm-config
	    #:make-wasm-config)
  ;; Engine
  (:export #:wasm-engine
	    #:make-wasm-engine)
  ;; Store
  (:export #:wasm-store
	    #:make-wasm-store)
  ;; Value Types
  (:export #:wasm-valtype
	    #:make-wasm-valtype)
  ;; Function Types
  (:export #:wasm-functype
	    #:make-wasm-functype)
  ;; Global Types
  (:export #:wasm-globaltype
	    #:make-wasm-globaltype)
  ;; Table Types
  (:export #:wasm-tabletype
	    #:make-wasm-tabletype)
  ;; Memory Types
  (:export #:wasm-memorytype
	    #:make-wasm-memorytype)
  ;; Extern Types
  (:export #:wasm-externtype)
  ;; Import Types
  (:export #:wasm-importtype)
  ;; Export Types
  (:export #:wasm-exporttype)
  ;; References
  (:export #:wasm-ref)
  ;; Values
  (:export #:wasm-val
	    #:wasm-val-vec)
  ;; Frames
  (:export #:wasm-frame
	    #:wasm-frame-vec)
  ;; Traps
  (:export #:wasm-trap)
  ;; Foreign Objects
  (:export #:wasm-foreign)
  ;; Modules
  (:export #:wasm-module
	    #:make-wasm-module)
  ;; Function Instances
  (:export #:wasm-func
	    #:make-wasm-func)
  ;; Global Instances
  (:export #:wasm-global
	    #:make-wasm-global)
  ;; Table Instances
  (:export #:wasm-table)
  ;; Memory Instances
  (:export #:wasm-memory)
  ;; Externals
  (:export #:wasm-extern)
  ;; Module Instances
  (:export #:wasm-instance
	    #:make-wasm-instance)
  ;; WAT
  (:export #:wat-to-wasm)
  ;; High-level interface
  (:export #:read-wasm-module
	    #:binary-to-wasm-module))
	    
	    
	    