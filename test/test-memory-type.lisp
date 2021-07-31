(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.memory-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.memory-type)

(5am:test test-make-wasm-memory-type
  (let* ((min 1)
	 (max 7)
	 (limits (make-wasm-limits min max))
	 (memory-type (make-wasm-memorytype limits))
	 (limits-from-memory-type (limits memory-type)))
    (5am:is (= min (minimum limits-from-memory-type)))
    (5am:is (= max (maximum limits-from-memory-type)))))

(5am:test test-memory-type-extern-type
  (let* ((min 2)
	 (max 8)
	 (limits (make-wasm-limits min max))
	 (memory-type (make-wasm-memorytype limits))
	 (extern-type (to-wasm-extern-type memory-type))
	 (memory-type-from-extern-type (to-wasm-memory-type extern-type))
	 (limits-from-memory-type (limits memory-type-from-extern-type)))
    (5am:is (eql :wasm-extern-memory (kind extern-type)))
    (5am:is (= min (minimum limits-from-memory-type)))
    (5am:is (= max (maximum limits-from-memory-type)))))
