(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.export-type
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.export-type)

(5am:test test-exporttype-functype
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

