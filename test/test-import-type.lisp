(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.import-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.import-type)

(5am:test test-import-type-func-type
  (let* ((params '(:wasm-i32 :wasm-i64))
	 (results '(:wasm-f32))
	 (func-type (make-wasm-functype params results))
	 (namespace "foo")
	 (name "bar")
	 (import-type (make-wasm-importtype namespace name func-type))
	 (extern-type (extern-type import-type))
	 (func-type-from-extern-type (to-wasm-func-type extern-type)))
    (5am:is (string= namespace (namespace import-type)))
    (5am:is (string= name (name import-type)))
    (5am:is (eql :wasm-extern-func (kind extern-type)))
    (5am:is (= (length params) (size (params func-type-from-extern-type))))
    (5am:is (= (length results) (size (results func-type-from-extern-type))))))
