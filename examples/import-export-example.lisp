(defvar *import-export-example-wat*
  "(module
     (func $host_function (import \"\" \"host_function\") (result i32))
     (global $host_global (import \"env\" \"host_global\") i32)
     (func $function (export \"guest_function\") (result i32) (global.get $global))
     (global $global (export \"guest_global\") i32 (i32.const 42))
     (table $table (export \"guest_table\") 1 1 funcref)
     (memory $memory (export \"guest_memory\") 1))")

(defun run-import-export-example ()
  (let* ((engine (wasm-rt:make-wasm-engine))
	 (store (wasm-rt:make-wasm-store engine))
	 (module (wasm-rt:wat-to-wasm store *import-export-example-wat*))
	 (host-functype
	   (wasm-rt:make-wasm-functype nil
				       (list (wasm-rt:make-wasm-valtype :wasm-i32))))
	 (host-func
	   (wasm-rt:make-wasm-func store
				   host-functype
				   (lambda (&rest args)
				     (declare (ignore args))
				     (wasm-rt:make-wasm-val 42 :wasm-i32))))
	 (host-globaltype
	   (wasm-rt:make-wasm-globaltype (wasm-rt:make-wasm-valtype :wasm-i32)
					 :wasm-const))
	 (host-global
	   (wasm-rt:make-wasm-global store
				     host-globaltype
				     (wasm-rt:make-wasm-val 42 :wasm-i32)))
	 (empty-namespace
	   (wasm-rt:make-wasm-namespace ""
					(list (wasm-rt:make-wasm-import "host_function"
									host-func))))
	 (env-namespace
	   (wasm-rt:make-wasm-namespace "env"
					(list (wasm-rt:make-wasm-import "host_global"
									host-global))))
	 (imports
	   (wasm-rt:make-wasm-imports module
				      (list empty-namespace
					    env-namespace)))
	 (instance (wasm-rt:make-wasm-instance store module imports))
	 (exports (wasm-rt:exports instance))
	 (guest-func (wasm-rt:get-export exports "guest_function" 'wasm-rt:wasm-func))
	 (guest-global (wasm-rt:get-export exports "guest_global" 'wasm-rt:wasm-global))
	 (guest-memory (wasm-rt:get-export exports "guest_memory" 'wasm-rt:wasm-memory))
	 (guest-table (wasm-rt:get-export exports "guest_table" 'wasm-rt:wasm-table)))
  (values guest-func
	  guest-global
	  guest-memory
	 guest-table)))
    
  
