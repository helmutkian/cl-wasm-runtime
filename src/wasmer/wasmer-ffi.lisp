(uiop:define-package #:cl-wasm-runtime.wasmer
  (:import-from #:cffi)
  (:use-reexport #:cl-wasm-runtime.wasmer/libwasmer)
  (:shadow #:wasm-extern-as-func-const
	   #:wasm-extern-as-global-const
	   #:wasm-extern-as-table-const
	   #:wasm-extern-as-memory-const)
  (:export #:wasm-extern-as-func-const
	   #:wasm-extern-as-global-const
	   #:wasm-extern-as-table-const
	   #:wasm-extern-as-memory-const))

(cl:in-package #:cl-wasm-runtime.wasmer)

;;; Wasmer does not currently define these functions

;; treat the non-const versions as const
(cl:setf (cl:fdefinition 'wasm-extern-as-func-const) #'wasm-extern-as-func
	 (cl:fdefinition 'wasm-extern-as-global-const) #'wasm-extern-as-global
	 (cl:fdefinition 'wasm-extern-as-table-const) #'wasm-extern-as-table
	 (cl:fdefinition 'wasm-extern-as-memory-const) #'wasm-extern-as-memory
	 (cl:fdefinition 'wasm-func-as-extern-const) #'wasm-func-as-extern
	 (cl:fdefinition 'wasm-global-as-extern-const) #'wasm-global-as-extern
	 (cl:fdefinition 'wasm-table-as-extern-const) #'wasm-table-as-extern
	 (cl:fdefinition 'wasm-memory-as-extern-const) #'wasm-memory-as-extern)

