(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.module
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.module)

(5am:test test-make-wasm-module
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module)"))
	(make-wasm-module *store* (wat-to-wasm wat))))))

(5am:test test-module-imports
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
		     (import \"ns\" \"function\" (func))
		     (import \"ns\" \"global\" (global f32))
		     (import \"ns\" \"table\" (table 1 2 anyfunc))
		     (import \"ns\" \"memory\" (memory 3 4)))")
	     (module (make-wasm-module *store* (wat-to-wasm wat)))
	     (imports (imports module)))
	(5am:is (= 4 (length imports)))
	(let* ((func-import-type (elt imports 0))
	       (func-extern-type (extern-type func-import-type))
	       (func-type (to-wasm-func-type func-extern-type))
	       (global-import-type (elt imports 1))
	       (global-extern-type (extern-type global-import-type))
	       (global-type (to-wasm-global-type global-extern-type))
	       (table-import-type (elt imports 2))
	       (table-extern-type (extern-type table-import-type))
	       (table-type (to-wasm-table-type table-extern-type))
	       (memory-import-type (elt imports 3))
	       (memory-extern-type (extern-type memory-import-type))
	       (memory-type (to-wasm-memory-type memory-extern-type)))
	  ;; FUNC
	  (5am:is (string= "ns" (namespace func-import-type)))
	  (5am:is (string= "function" (name func-import-type)))
	  (5am:is (eql :wasm-extern-func (kind func-extern-type)))
	  (5am:is (= 0 (size (params func-type))))
	  (5am:is (= 0 (size (results func-type))))
	  ;; GLOBAL
	  (5am:is (string= "ns" (namespace global-import-type)))
	  (5am:is (string= "global" (name global-import-type)))
	  (5am:is (eql :wasm-extern-global (kind global-extern-type)))
	  (5am:is (eql :wasm-f32 (kind (value-type global-type))))
	  (5am:is-false (mutable? global-type))
	  ;; TABLE
	  (5am:is (string= "ns" (namespace table-import-type)))
	  (5am:is (string= "table" (name table-import-type)))
	  (5am:is (eql :wasm-extern-table (kind table-extern-type)))
	  (5am:is (= 1 (minimum (limits table-type))))
	  (5am:is (= 2 (maximum (limits table-type))))
	  ;; MEMORY
	  (5am:is (string= "ns" (namespace memory-import-type)))
	  (5am:is (string= "memory" (name memory-import-type)))
	  (5am:is (eql :wasm-extern-memory (kind memory-extern-type)))
	  (5am:is (= 3 (minimum (limits memory-type))))
	  (5am:is (= 4 (maximum (limits memory-type)))))))))

(5am:test test-module-exports
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
		     (func (export \"function\") (param i32 i64))
		     (global (export \"global\") i32 (i32.const 7))
		     (table (export \"table\") 0 funcref)
		     (memory (export \"memory\") 1))")
	     (module (make-wasm-module *store* (wat-to-wasm wat)))
	     (exports (exports module)))
	(5am:is (= 4 (length exports)))
	(let* ((func-export-type (elt exports 0))
	       (func-extern-type (extern-type func-export-type))
	       (func-type (to-wasm-func-type func-extern-type))
	       (global-export-type (elt exports 1))
	       (global-extern-type (extern-type global-export-type))
	       (global-type (to-wasm-global-type global-extern-type))
	       (table-export-type (elt exports 2))
	       (table-extern-type (extern-type table-export-type))
	       (table-type (to-wasm-table-type table-extern-type))
	       (memory-export-type (elt exports 3))
	       (memory-extern-type (extern-type memory-export-type))
	       (memory-type (to-wasm-memory-type memory-extern-type)))
	  ;; FUNC
	  (5am:is (string= "function" (name func-export-type)))
	  (5am:is (eql :wasm-extern-func (kind func-extern-type)))
	  (5am:is (= 2 (size (params func-type))))
	  (5am:is (= 0 (size (results func-type))))
	  ;; GLOBAL
	  (5am:is (string= "global" (name global-export-type)))
	  (5am:is (eql :wasm-extern-global (kind global-extern-type)))
	  (5am:is (eql :wasm-i32 (kind (value-type global-type))))
	  (5am:is-false (mutable? global-type))
	  ;; TABLE
	  (5am:is (string= "table" (name table-export-type)))
	  (5am:is (eql :wasm-extern-table (kind table-extern-type)))
	  (5am:is (= 0 (minimum (limits table-type))))
	  ;; MEMORY
	  (5am:is (string= "memory" (name memory-export-type)))
	  (5am:is (eql :wasm-extern-memory (kind memory-extern-type)))
	  (5am:is (= 1 (minimum (limits memory-type)))))))))

(5am:test test-module-serialize
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module)")
	     (module (make-wasm-module *store* (wat-to-wasm wat))))
	(serialize module)))))

(5am:test test-module-deserialize
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
		     (func (export \"function\") (param i32 i64)))")
	     (module (make-wasm-module *store* (wat-to-wasm wat)))
	     (serialized-module (serialize module))
	     (deserialized-module (deserialize *store* serialized-module))
	     (exports (exports deserialized-module))
	     (export-type (elt exports 0))
	     (extern-type (extern-type export-type))
	     (func-type (to-wasm-func-type extern-type)))
	(5am:is (string= "function" (name export-type)))
	(5am:is (eql :wasm-extern-func (kind extern-type)))
	(5am:is (= 2 (size (params func-type))))
	(5am:is (= 0 (size (results func-type))))
	(5am:finishes (make-wasm-instance *store* deserialized-module))))))
