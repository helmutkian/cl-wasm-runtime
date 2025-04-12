(uiop:define-package #:cl-wasm-runtime.test/instance
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/gc
	#:cl-wasm-runtime.test/fixture/module)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/instance)

(5am:def-suite* cl-wasm-runtime-test/instance :in cl-wasm-runtime-test)

(5am:test (test-make-instance :fixture gc-fixture)
  (5am:with-fixture module-fixture (:path "test/data/test-minimal-module.wasm")
    (5am:finishes
      (wasm-rt:make-wasm-instance *store* *module*))))

(5am:test (test-instance-exports :fixture gc-fixture)
  (5am:with-fixture module-fixture (:path "test/data/test-module-exports.wasm")
    (5am:finishes
      ;; TODO: add func
      (let* ((instance (wasm-rt:make-wasm-instance *store* *module*))
	     (exports (wasm-rt:instance-exports instance))
	     (func (wasm-rt:exports-get exports "function"))
	     (functype (wasm-rt:func-type func))
	     (global (wasm-rt:exports-get exports "global")) 
	     (globaltype (wasm-rt:global-type global))
	     (table (wasm-rt:exports-get exports "table"))
	     (tabletype (wasm-rt:table-type table))
	     (memory (wasm-rt:exports-get exports "memory"))
	     (memorytype (wasm-rt:memory-type memory)))
	(5am:is-true (every #'wasm-rt:wasm-object-eq?
			    '(:wasm-i32 :wasm-i64)
			    (wasm-rt:functype-params functype)))
	(5am:is (null (wasm-rt:functype-results functype)))
	(5am:is (eql :wasm-i32 (wasm-rt:globaltype-type globaltype))) 
	(5am:is-false (wasm-rt:globaltype-mutable? globaltype))
	(5am:is (eql :wasm-funcref (wasm-rt:tabletype-type tabletype)))
	(5am:is (= 0 (wasm-rt:tabletype-limits tabletype)))
	(5am:is (= 1 (wasm-rt:memorytype-limits memorytype)))))))


(5am:test (test-instance-imports :fixture gc-fixture)
  (5am:with-fixture module-fixture (:path "test/data/test-instance-imports.wasm")
    (5am:finishes
      (let* ((foo-bar-global
	       (wasm-rt:make-wasm-global *store* (wasm-rt:make-wasm-globaltype :wasm-i32) 7))
	     (quux-quuy-global
	       (wasm-rt:make-wasm-global *store* (wasm-rt:make-wasm-globaltype :wasm-f32) 7.0))
	     (imports
	       (wasm-rt:imports ("foo" ("bar" foo-bar-global))
				("quux" ("quuy" quux-quuy-global)))))
	(wasm-rt:make-wasm-instance *store* *module* imports)))))

(5am:test (test-instance-imports-missing :fixture gc-fixture)
  (5am:with-fixture module-fixture (:path "test/data/test-instance-imports.wasm")
    (5am:signals wasm-rt:import-missing-error
      (let* ((foo-bar-global
	       (wasm-rt:make-wasm-global *store* (wasm-rt:make-wasm-globaltype :wasm-i32) 7))
	     (imports
	       (wasm-rt:imports ("foo" ("bar" foo-bar-global)))))
	(wasm-rt:make-wasm-instance *store* *module* imports)))))

(5am:test test-instance-imports-invalid
  (5am:with-fixture module-fixture (:path "test/data/test-instance-imports.wasm")
    (5am:signals wasm-rt:import-type-error
      (let* ((foo-bar-global
	       (wasm-rt:make-wasm-global *store* (wasm-rt:make-wasm-globaltype :wasm-i64) 7))
	     (quux-quuy-global
	       (wasm-rt:make-wasm-global *store* (wasm-rt:make-wasm-globaltype :wasm-f32) 7.0))
	     (imports
	       (wasm-rt:imports ("foo" ("bar" foo-bar-global))
				("quux" ("quuy" quux-quuy-global)))))
	(wasm-rt:make-wasm-instance *store* *module* imports)))))

(5am:test test-instance-traps
  (5am:with-fixture module-fixture (:path "test/data/test-instance-traps.wasm")
    (handler-case (wasm-rt:make-wasm-instance *store* *module*)
      (wasm-rt:wasm-trap-error (c)
	(5am:is (string= "unreachable" (wasm-rt/error:wasm-trap-error-message c)))))))

