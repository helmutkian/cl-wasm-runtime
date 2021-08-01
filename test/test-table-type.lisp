(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.table-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.table-type)

(5am:test test-make-table-type
  (let* ((min 1)
	 (max 7)
	 (limits (make-wasm-limits min max))
	 (table-type (make-wasm-tabletype :wasm-i32 limits))
	 (value-type (value-type table-type))
	 (limits-from-table-type (limits table-type)))
    (5am:is (eql :wasm-i32 (kind value-type)))
    (5am:is (= min (minimum limits-from-table-type)))
    (5am:is (= max (maximum limits-from-table-type)))))

(5am:test test-table-type-to-extern-type
  (let* ((min 1)
	 (max 7)
	 (limits (make-wasm-limits min max))
	 (table-type (make-wasm-tabletype :wasm-i32 limits))
	 (extern-type (to-wasm-extern-type table-type))
	 (table-type-from-extern-type (to-wasm-table-type extern-type))
	 (value-type (value-type table-type-from-extern-type))
	 (limits-from-table-type (limits table-type-from-extern-type)))
    (5am:is (eql :wasm-extern-table (kind extern-type)))
    (5am:is (eql :wasm-i32 (kind value-type)))
    (5am:is (= min (minimum limits-from-table-type)))
    (5am:is (= max (maximum limits-from-table-type)))))
