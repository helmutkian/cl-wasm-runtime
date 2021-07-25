(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.func-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.func-type)

(5am:test test-make-wasm-func-type-with-keyword-val-types
  (let* ((params '(:wasm-i32 :wasm-i64))
	 (results '(:wasm-f32))
	 (func-type (make-wasm-functype params results)))
    (5am:is (= (length params) (size (params func-type))))
    (5am:is (= (length results) (size (results func-type))))))

(5am:test test-make-wasm-func-type-with-wasm-val-types
  (let* ((params (list (make-wasm-valtype :wasm-i32) (make-wasm-valtype :wasm-i64)))
	 (results (list (make-wasm-valtype :wasm-f32)))
	 (func-type (make-wasm-functype params results)))
    (5am:is (= (length params) (size (params func-type))))
    (5am:is (= (length results) (size (results func-type))))))

(5am:test test-func-type-to-wasm-extern-type
  (let* ((params '(:wasm-i32 :wasm-i64))
	 (results '(:wasm-f32))
	 (func-type (make-wasm-functype params results))
	 (extern-type (to-wasm-extern-type func-type))
	 (func-type-from-extern-type (to-wasm-func-type extern-type)))
    (5am:is (eql :wasm-extern-func (kind extern-type)))
    (5am:is (= (length params) (size (params func-type-from-extern-type))))
    (5am:is (= (length results) (size (results func-type-from-extern-type))))))
