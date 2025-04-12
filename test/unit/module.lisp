(uiop:define-package #:cl-wasm-runtime.test/module
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:mix #:cl-wasm-runtime.test/fixture/store
	#:cl-wasm-runtime.test/fixture/module) 
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/module)

(5am:def-suite cl-wasm-runtime-test/module :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/module)

(5am:test test-make-wasm-module
  (5am:with-fixture store-fixture ()
    (5am:with-fixture test-wasm-binary-fixture ("test/data/test-minimal-module.wasm") 
      (5am:finishes
	(wasm-rt:make-wasm-module *store* *test-wasm-binary*)))))

(5am:test test-module-validate
  (5am:with-fixture store-fixture ()
    (5am:with-fixture test-wasm-binary-fixture ("test/data/test-minimal-module.wasm")
      (5am:finishes
	(wasm-rt:module-validate *store* *test-wasm-binary*)))))

(5am:test test-module-imports
  (5am:with-fixture store-fixture ()
    (5am:with-fixture test-wasm-binary-fixture ("test/data/test-module-imports.wasm")
      (5am:finishes
	(let* ((module (wasm-rt:make-wasm-module *store* *test-wasm-binary*))
	       (imports (wasm-rt:module-imports module)))
	  (5am:is (= 4 (length imports)))	  
	  (let* ((func-importtype (elt imports 0))
		 (func-externtype (wasm-rt:importtype-type func-importtype))
		 (functype (wasm-rt:externtype-to-functype func-externtype))
		 (global-importtype (elt imports 1))
		 (global-externtype (wasm-rt:importtype-type global-importtype))
		 (globaltype (wasm-rt:externtype-to-globaltype global-externtype))
		 (table-importtype (elt imports 2))
		 (table-externtype (wasm-rt:importtype-type table-importtype))
		 (tabletype (wasm-rt:externtype-to-tabletype table-externtype))
		 (memory-importtype (elt imports 3))
		 (memory-externtype (wasm-rt:importtype-type memory-importtype))
		 (memorytype (wasm-rt:externtype-to-memorytype memory-externtype)))
	    ;; func
	    (5am:is (string= "ns" (wasm-rt:importtype-module func-importtype)))
	    (5am:is (string= "function" (wasm-rt:importtype-name func-importtype)))
	    (5am:is (eql :wasm-extern-func (wasm-rt:externtype-kind func-externtype)))
	    (5am:is-true (zerop (length (wasm-rt:functype-params functype))))
	    (5am:is-true (zerop (length (wasm-rt:functype-results functype))))
	    ;; global
	    (5am:is (string= "ns" (wasm-rt:importtype-module global-importtype)))
	    (5am:is (string= "global" (wasm-rt:importtype-name global-importtype)))
	    (5am:is (eql :wasm-extern-global (wasm-rt:externtype-kind global-externtype)))
	    (5am:is (eql :wasm-f32 (wasm-rt:globaltype-type globaltype)))
	    (5am:is-false (wasm-rt:globaltype-mutable? globaltype))
	    ;; table
	    (5am:is (string= "ns" (wasm-rt:importtype-module table-importtype)))
	    (5am:is (string= "table" (wasm-rt:importtype-name table-importtype)))
	    (5am:is (eql :wasm-extern-table (wasm-rt:externtype-kind table-externtype)))
	    (5am:is (eql :wasm-funcref (wasm-rt:tabletype-type tabletype)))
	    (5am:is-true
	     (every #'=
		    '(1 2)
		    (multiple-value-list (wasm-rt:tabletype-limits tabletype))))
	    ;; memorytype
	    (5am:is (string= "ns" (wasm-rt:importtype-module memory-importtype)))
	    (5am:is (string= "memory" (wasm-rt:importtype-name memory-importtype)))
	    (5am:is (eql :wasm-extern-memory (wasm-rt:externtype-kind memory-externtype)))
	    (5am:is-true
	     (every #'=
		    '(3 4)
		    (multiple-value-list (wasm-rt:memorytype-limits memorytype))))))))))

(5am:test test-module-exports
  (5am:with-fixture module-fixture (:path "test/data/test-module-exports.wasm")
    (let* ((exports (wasm-rt:module-exports *module*)))
      (5am:is (= 4 (length exports)))
      (let* (;; func
	     (func-exporttype (elt exports 0))
	     (func-externtype (wasm-rt:exporttype-type func-exporttype))
	     (functype (wasm-rt:externtype-to-functype func-externtype))
	     ;; global
	     (global-exporttype (elt exports 1))
	     (global-externtype (wasm-rt:exporttype-type global-exporttype))
	     (globaltype (wasm-rt:externtype-to-globaltype global-externtype))
	     ;; table
	     (table-exporttype (elt exports 2))
	     (table-externtype (wasm-rt:exporttype-type table-exporttype))
	     (tabletype (wasm-rt:externtype-to-tabletype table-externtype))
	     ;; memory
	     (memory-exporttype (elt exports 3))
	     (memory-externtype (wasm-rt:exporttype-type memory-exporttype))
	     (memorytype (wasm-rt:externtype-to-memorytype memory-externtype)))
	;; func
	(5am:is (string= "function" (wasm-rt:exporttype-name func-exporttype)))
	(5am:is (eql :wasm-extern-func (wasm-rt:externtype-kind func-externtype)))
	(5am:is-true (every #'wasm-rt:wasm-object-eq?
			    '(:wasm-i32 :wasm-i64)
			    (wasm-rt:functype-params functype)))
	(5am:is-true (null (wasm-rt:functype-results functype)))
	;; global
	(5am:is (string= "global" (wasm-rt:exporttype-name global-exporttype)))
	(5am:is (eql :wasm-extern-global (wasm-rt:externtype-kind global-externtype)))
	(5am:is (eql :wasm-i32 (wasm-rt:globaltype-type globaltype)))
	(5am:is-false (wasm-rt:globaltype-mutable? globaltype))
	;; table
	(5am:is (string= "table" (wasm-rt:exporttype-name table-exporttype)))
	(5am:is (eql :wasm-extern-table (wasm-rt:externtype-kind table-externtype)))
	(5am:is (eql :wasm-funcref (wasm-rt:tabletype-type tabletype)))
	(5am:is-true (= 0 (wasm-rt:tabletype-limits tabletype)))
	;; memory
	(5am:is (string= "memory" (wasm-rt:exporttype-name memory-exporttype)))
	(5am:is (eql :wasm-extern-memory (wasm-rt:externtype-kind memory-externtype)))
	(5am:is (= 1 (wasm-rt:memorytype-limits memorytype)))))))

(5am:test test-module-serialize
  (5am:with-fixture module-fixture (:path "test/data/test-minimal-module.wasm")
    (5am:finishes
      (wasm-rt:module-serialize *module*))))

(5am:test test-module-deserialize
  (5am:with-fixture module-fixture (:path "test/data/test-module-deserialize.wasm")
    (5am:finishes
      (let* ((serialized-module (wasm-rt:module-serialize *module*))
	     (deserialized-module (wasm-rt:module-deserialize *store* serialized-module)))
	(5am:is-true (every #'wasm-rt:wasm-object-eq?
			    (wasm-rt:module-imports *module*)
			    (wasm-rt:module-imports deserialized-module)))
	(5am:is-true (every #'wasm-rt:wasm-object-eq?
			    (wasm-rt:module-exports *module*)
			    (wasm-rt:module-exports deserialized-module)))))))
