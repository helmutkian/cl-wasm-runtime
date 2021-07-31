(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.instance
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.instance)

(5am:test test-make-wasm-instance
  (5am:finishes
    (let* ((engine (make-wasm-engine))
	   (store (make-wasm-store engine))
	   (module (wat-to-wasm-module store "(module)")))
      (make-wasm-instance store module))))

(5am:test test-instance-exports
 (5am:finishes
   (let* ((engine (make-wasm-engine))
	  (store (make-wasm-store engine))
	  (wat "(module
                  (func (export \"function\") (param i32 i64))
                  (global (export \"global\") i32 (i32.const 7))
                  (table (export \"table\") 0 funcref)
                  (memory (export \"memory\") 1))")
	  (module (wat-to-wasm-module store wat))
	  (instance (make-wasm-instance store module))
	  (exports (exports instance))
	  (func (get-export exports "function" 'wasm-func))
	  (global (get-export exports "global" 'wasm-global))
	  (table (get-export exports "table" 'wasm-table))
	  (memory (get-export exports "memory" 'wasm-memory)))
     (5am:is (not (null func)))
     (5am:is (not (null global)))
     (5am:is (not (null table)))
     (5am:is (not (null memory))))))

(5am:test test-instance-missing-imports
  (5am:finishes
    (let* ((engine (make-wasm-engine))
	   (store (make-wasm-store engine))
	   (wat "(module
                   (func (import \"missing\" \"function\"))
                   (func (import \"exists\" \"function\")))")
	   (module (wat-to-wasm-module store wat))
	   (func (make-wasm-func store
				 (make-wasm-functype nil nil)
				 (make-wasm-callback (lambda (&rest args)
						       (declare (ignore args))
						       nil))))
	   (namespace (make-wasm-namespace "exists" (make-wasm-import "function" func))))
      (5am:signals t
	(make-wasm-instance store module (make-wasm-imports module namespace))))))

(5am:test test-instance-import-modules
  (5am:finishes
    (let* ((engine (make-wasm-engine))
	   (store (make-wasm-store engine))
	   (wat "(module
                   (func (import \"env\" \"function\"))
                   (func (import \"foo\" \"function\")))")
	   (module (wat-to-wasm-module store wat))
	   (func1 (make-wasm-func store
				 (make-wasm-functype nil nil)
				 (make-wasm-callback (lambda (&rest args)
						       (declare (ignore args))
						       nil))))
	   (func2 (make-wasm-func store
				 (make-wasm-functype nil nil)
				 (make-wasm-callback (lambda (&rest args)
						       (declare (ignore args))
						       nil))))
	   (imports (import-modules module
				    ("env" ("function" func1))
				    ("foo" ("function" func2)))))
      (make-wasm-instance store module imports))))


(5am:test test-instance-traps
  (5am:finishes
    (let* ((engine (make-wasm-engine))
	   (store (make-wasm-store engine))
	   (wat "(module
		   (start $start_f)
		   (type $start_t (func))
		   (func $start_f (type $start_t) unreachable))")
	   (module (wat-to-wasm-module store wat)))
      ;; It's not possible to access the actual WASM trap object
      ;; with the common WASM C API. Need to use engine-specific API.
      ;; The common WASM C API will just signal a generic foreign trap.
      (5am:signals t (make-wasm-instance store module)))))
