(defpackage #:cl-wasm-runtime
  (:nicknames #:wasm-rt)
  (:use #:cl)
  (:export #:wasm-object
	   #:wasm-delete)
  ;; Generic functions
  (:export #:size
	   #:value
	   #:data-aref
	   #:to-list
	   #:kind)
  ;; Bytes
  (:export #:wasm-byte
	   #:wasm-byte-vec
	   #:wasm-byte-vec-to-string
	   #:wasm-byte-vec-to-octets
	   #:string-to-wasm-byte-vec
	   #:octets-to-wasm-byte-vec)
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
	   #:make-wasm-valtype
	   #:value-type)
  ;; Function Types
  (:export #:wasm-functype
	   #:make-wasm-functype
	   #:params
	   #:results)
  ;; Global Types
  (:export #:wasm-globaltype
	   #:make-wasm-globaltype
	   #:mutable?
	   #:value)
  ;; Table Types
  (:export #:wasm-tabletype
	   #:make-wasm-tabletype)
  ;; Memory Types
  (:export #:wasm-limits
	   #:make-wasm-limits
	   #:minimum
	   #:maximum
	   #:wasm-memorytype
	   #:make-wasm-memorytype
	   #:limits)
  ;; Extern Types
  (:export #:wasm-externtype
	   #:extern-type
	   #:to-wasm-extern-type
	   #:to-wasm-func-type
	   #:to-wasm-memory-type
	   #:to-wasm-global-type
	   #:to-wasm-table-type)
  ;; Import Types
  (:export #:wasm-importtype
	   #:make-wasm-importtype
	   #:namespace)
  ;; Export Types
  (:export #:wasm-exporttype
	   #:make-wasm-exporttype)
  ;; References
  (:export #:wasm-ref)
  ;; Values
  (:export #:wasm-val
	   #:wasm-val-vec
	   #:make-wasm-val)
  ;; Frames
  (:export #:wasm-frame
	   #:func-index
	   #:func-offset
	   #:func-module
	   #:wasm-frame-vec)
  ;; Traps
  (:export #:wasm-trap
	   #:message
	   #:origin
	   #:trap-trace
	   #:wasm-trap-error
	   #:wasm-trap-error-message
	   #:wasm-trap-error-origin
	   #:wasm-trap-error-trace)
  ;; Foreign Objects
  (:export #:wasm-foreign)
  ;; Modules
  (:export #:wasm-module
	   #:make-wasm-module
	   #:serialize
	   #:deserialize
	   #:load-wasm
	   #:load-wasm-module)
  ;; Function Instances
  (:export #:wasm-func
	   #:make-wasm-callback
	   #:make-wasm-func
	   #:wasm-func-error
	   #:wasm-funcall)
  ;; Global Instances
  (:export #:wasm-global
	   #:make-wasm-global
	   #:global-type)
  ;; Table Instances
  (:export #:wasm-table)
  ;; Memory Instances
  (:export #:wasm-memory
	   #:make-wasm-memory
	   #:wasm-memory-buffer
	   #:buffer-aref
	   #:buffer-to-octets
	   #:buffer-to-string
	   #:bytes
	   #:buffer
	   #:buffer-size
	   #:grow
	   #:memory-type)
  ;; Externals
  (:export #:wasm-extern
	   #:wasm-extern-as-func)
  ;; Module Instances
  (:export #:wasm-imports
	   #:make-wasm-imports
	   #:make-wasm-namespace
	   #:make-wasm-import
	   #:import-modules
	   #:name
	   #:extern
	   #:imports
	   #:exports
	   #:get-export
	   #:wasm-instance
	   #:make-wasm-instance)
  ;; WAT
  (:export #:wat-to-wasm
	   #:wat-to-wasm-module))
	    
	    
	    
