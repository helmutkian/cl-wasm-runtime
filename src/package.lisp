(in-package #:cl-user)

(defvar *wasm-rt-package*
  (make-package (string '#:cl-wasm-runtime) :nicknames (list (string '#:wasm-rt))))

(defmacro import-reexport (package &rest symbols)
  `(flet ((%import-reexport (package &rest symbols)
	    (dolist (sym symbols) 
	     (let ((imported-sym (uiop:find-symbol* sym package)))
	       (uiop:rehome-symbol imported-sym *wasm-rt-package*)))
	    (export (mapcar (lambda (sym) (uiop:find-symbol* sym *wasm-rt-package*))
			    symbols)
		    *wasm-rt-package*)))
     (%import-reexport ',package ,@(loop for sym in symbols append `(',sym)))))

(import-reexport #:cl-wasm-runtime.internal/conditions
		 #:wasm-error
		 #:wasm-simple-error
		 #:wasm-runtime-error
		 #:wasm-runtime-error-message
		 #:required-initarg-error
		 #:required-initarg-error-initarg
		 #:wasm-translate-error
		 #:wasm-translate-error-kind
		 #:wasm-translate-float-overflow
		 #:wasm-translate-type-error
		 #:wasm-trap-error
		 #:wasm-trap-error-message
		 #:wasm-trap-error-origin
		 #:wasm-trap-error-trace
		 #:wasm-translate-error
		 #:import-error
		 #:import-error-module
		 #:import-error-name
		 #:import-error-externkind
		 #:import-missing-error
		 #:import-type-error)

(import-reexport #:cl-wasm-runtime.internal/object
		 #:wasm-object
		 #:wasm-object-eq?
		 #:null?)

(import-reexport #:cl-wasm-runtime.internal/engine
		 #:wasm-engine
		 #:make-wasm-engine)

(import-reexport #:cl-wasm-runtime.internal/store
		 #:wasm-store
		 #:make-wasm-store)

(import-reexport #:cl-wasm-runtime.internal/vector
		 #:wasm-vec)

(import-reexport #:cl-wasm-runtime.internal/byte-vec
		 #:wasm-byte
		 #:wasm-byte-vec
		 #:wasm-name
		 #:make-wasm-name
		 #:string-to-wasm-byte-vec
		 #:octets-to-wasm-byte-vec
		 #:wasm-byte-vec-to-string
		 #:wasm-byte-vec-to-octets)

(import-reexport #:cl-wasm-runtime.internal/limits
		 #:wasm-pages
		 #:+wasm-page-size+
		 #:+max-wasm-pages+
		 #:+min-wasm-pages+
		 #:pages-num-bytes
		 #:+limits-max-unbounded+
		 #:wasm-limits
		 #:make-wasm-limits
		 #:limits-min
		 #:limits-max
		 #:limits-unbounded?)

(import-reexport #:cl-wasm-runtime.internal/valtype
		 #:wasm-valkind-key
		 #:wasm-valtype
		 #:make-wasm-valtype
		 #:valtype-kind
		 #:valtype-reference?
		 #:valtype-number?)

(import-reexport #:cl-wasm-runtime.internal/functype
		 #:wasm-functype
		 #:make-wasm-functype
		 #:functype-params
		 #:functype-results)

(import-reexport #:cl-wasm-runtime.internal/globaltype
		 #:wasm-globaltype
		 #:make-wasm-globaltype
		 #:globaltype-type
		 #:globaltype-mutable?)

(import-reexport #:cl-wasm-runtime.internal/memorytype
		 #:wasm-memorytype
		 #:make-wasm-memorytype
		 #:memorytype-limits)

(import-reexport #:cl-wasm-runtime.internal/tabletype
		 #:wasm-tabletype
		 #:make-wasm-tabletype
		 #:tabletype-type
		 #:tabletype-limits)

(import-reexport #:cl-wasm-runtime.internal/externtype
		 #:wasm-externtype
		 #:externtype-able
		 #:externtype-kind
		 #:to-externtype
		 #:externtype-to-functype
		 #:externtype-to-globaltype
		 #:externtype-to-memorytype
		 #:externtype-to-tabletype
		 #:from-externtype)

(import-reexport #:cl-wasm-runtime.internal/importtype
		 #:wasm-importtype
		 #:make-wasm-importtype
		 #:importtype-name
		 #:importtype-module
		 #:importtype-type)

(import-reexport #:cl-wasm-runtime.internal/exporttype
		 #:wasm-exporttype
		 #:make-wasm-exporttype
		 #:exporttype-name
		 #:exporttype-type)

(import-reexport #:cl-wasm-runtime.internal/val
		 #:wasm-val
		 #:make-wasm-val
		 #:val-kind
		 #:val-value
		 #:translate-to-wasm)

(import-reexport #:cl-wasm-runtime.internal/trap
		 #:wasm-trap
		 #:make-wasm-trap
		 #:trap-message
		 #:trap-origin
		 #:trap-trace)

(import-reexport #:cl-wasm-runtime.internal/module
		 #:wasm-module
		 #:make-wasm-module
		 #:module-validate
		 #:module-imports
		 #:module-exports
		 #:module-serialize
		 #:module-deserialize
		 #:load-wasm
		 #:load-wasm-module)

(import-reexport #:cl-wasm-runtime.internal/func
		 #:wasm-func
		 #:make-wasm-func
		 #:wasm-funcall
		 #:func-type
		 #:func-param-arity
		 #:func-result-arity)

(import-reexport #:cl-wasm-runtime.internal/global
		 #:wasm-global
		 #:make-wasm-global
		 #:global-type
		 #:global-value)

(import-reexport #:cl-wasm-runtime.internal/table
		 #:wasm-table
		 #:table-size
		 #:ref
		 #:grow-table)

#-wasmer
(import-reexport #:cl-wasm-runtime.internal/table
		 #:table-type)

(import-reexport #:cl-wasm-runtime.internal/memory
		 #:wasm-memory
		 #:make-wasm-memory
		 #:memory-type
		 #:memory-size
		 #:memory-data-size
		 #:memory-data
		 #:memory-grow
		 #:memory-buffer
		 #:memory-buffer-size
		 #:memory-buffer-aref
		 #:memory-buffer-to-octets
		 #:memory-buffer-to-string)

(import-reexport #:cl-wasm-runtime.internal/extern
		 #:wasm-extern
		 #:extern-able
		 #:extern-type
		 #:to-extern
		 #:extern-to-func
		 #:extern-to-global
		 #:extern-to-table
		 #:extern-to-memory
		 #:from-extern)

#+wasmer
(import-reexport #:cl-wasm-runtime.internal/extern
		 #:table-type)

(import-reexport #:cl-wasm-runtime.internal/instance
		 #:imports
		 #:exports
		 #:make-imports
		 #:wasm-instance
		 #:make-wasm-instance
		 #:instance-exports
		 #:exports-get)

