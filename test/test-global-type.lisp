(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.global-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.global-type)

(5am:test test-make-wasm-global-type
  (let* ((global-type-from-key (make-wasm-globaltype :wasm-i32 :mutable t))
	 (val-type (make-wasm-valtype :wasm-f32))
	 (global-type-from-val-type (make-wasm-globaltype val-type)))
    (5am:is (mutable? global-type-from-key))
    (5am:is (eql :wasm-i32 (kind (value-type global-type-from-key))))
    (5am:is (not (mutable? global-type-from-val-type)))
    (5am:is (eql :wasm-f32 (kind (value-type global-type-from-val-type))))))

(5am:test test-wasm-global-type-to-extern-type
  (let* ((global-type (make-wasm-globaltype :wasm-i32))
	 (extern-type (to-wasm-extern-type global-type))
	 (global-type-from-extern-type (to-wasm-global-type extern-type)))
    (5am:is (eql :wasm-extern-global (kind extern-type)))
    (5am:is (eql (kind (value-type global-type)) (kind (value-type global-type-from-extern-type))))
    (5am:is (eql (mutable? global-type) (mutable? global-type-from-extern-type)))))
