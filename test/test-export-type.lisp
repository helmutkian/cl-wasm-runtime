(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.export-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.export-type)

(5am:test test-to-wasm-func-type
  (5am:finishes
    (let* ((params '(:wasm-i32 :wasm-i64))
	   (results '(:wasm-f64))
	   (func-type (make-wasm-functype params results))
	   (name "foo")
	   (export-type (make-wasm-exporttype name func-type))
	   (extern-type (extern-type export-type))
	   (func-type-from-extern-type (to-wasm-func-type extern-type)))

      (5am:is (string= name (name export-type)))
      (5am:is (eql :wasm-extern-func (kind extern-type)))

      (5am:is (= (length params)
		 (size (params func-type-from-extern-type))))
      (5am:is (= (length results)
		 (size (results func-type-from-extern-type)))))))

(5am:test test-to-wasm-global-type
  (5am:finishes
    (let* ((global-type (make-wasm-globaltype :wasm-i32 :mutable t))
	   (name "foo")
	   (export-type (make-wasm-exporttype name global-type))
	   (extern-type (extern-type export-type))
	   (global-type-from-extern-type (to-wasm-global-type extern-type)))
      (5am:is (string= name (name export-type)))
      (5am:is (eql :wasm-extern-global (kind extern-type)))
      (5am:is (eql :wasm-i32 (kind (value-type global-type-from-extern-type))))
      (5am:is (eql t (mutable? global-type))))))

(5am:test test-to-wasm-table-type
  (5am:finishes
    (let* ((min 1)
	   (max 7)
	   (limits (make-wasm-limits min max))
	   (table-type (make-wasm-tabletype :wasm-i32 limits))
	   (name "foo")
	   (export-type (make-wasm-exporttype name table-type))
	   (extern-type (extern-type export-type))
	   (table-type-from-extern-type (to-wasm-table-type extern-type))
	   (value-type-from-table-type (value-type table-type-from-extern-type))
	   (limits-from-table-type (limits table-type-from-extern-type)))
      (5am:is (string= name (name export-type)))
      (5am:is (eql :wasm-extern-table (kind extern-type)))
      (5am:is (eql :wasm-i32 (kind value-type-from-table-type)))
      (5am:is (= min (minimum limits-from-table-type)))
      (5am:is (= max (maximum limits-from-table-type))))))

(5am:test test-to-wasm-memory-type
  (5am:finishes
    (let* ((min 1)
	   (max 7)
	   (limits (make-wasm-limits min max))
	   (name "foo")
	   (memory-type (make-wasm-memorytype limits))
	   (export-type (make-wasm-exporttype name memory-type))
	   (extern-type (extern-type export-type))
	   (memory-type-from-extern-type (to-wasm-memory-type extern-type))
	   (limits-from-memory-type (limits memory-type-from-extern-type)))
      (5am:is (string= name (name export-type)))
      (5am:is (eql :wasm-extern-memory (kind extern-type)))
      (5am:is (= min (minimum limits-from-memory-type)))
      (5am:is (= max (maximum limits-from-memory-type))))))
