(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.func
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.func)

(5am:test test-guest-func
  (5am:finishes
    (let* ((wat "(module
		   (type $sum_t (func (param i32 i32) (result i32)))
		   (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
		      local.get $x
		      local.get $y
		      i32.add)
		   (export \"sum\" (func $sum_f)))")
	   (bytes (wat-to-wasm wat)))
      (5am:with-fixture instance-fixture (bytes)
	(let* ((exports (exports *instance*))
	       (sum (get-export exports "sum" 'wasm-func)))
	  (5am:is (= 2 (param-arity sum)))
	  (5am:is (= 1 (result-arity sum)))
	  (5am:is (= 3 (wasm-funcall sum 1 2))))))))

(5am:test test-guest-func-zero-results
  (5am:finishes
    (let* ((wat "(module
		   (type $test_t (func (param i32 i32)))
		   (func $test_f (type $test_t) (param $x i32) (param $y i32))
		   (export \"test\" (func $test_f)))")
	   (bytes (wat-to-wasm wat)))
      (5am:with-fixture instance-fixture (bytes)
	(let* ((exports (exports *instance*))
	       (test (get-export exports "test" 'wasm-func)))
	  (5am:is-true (null (wasm-funcall test 1 2))))))))

(5am:test test-guest-func-multiple-results
  (5am:finishes
    (let* ((wat "(module
		   (type $swap_t (func (param i32 i64) (result i64 i32)))
		   (func $swap_f (type $swap_t) (param $x i32) (param $y i64) (result i64 i32)
		       local.get $y
		       local.get $x)
		   (export \"swap\" (func $swap_f)))")
	   (bytes (wat-to-wasm wat)))
      (5am:with-fixture instance-fixture (bytes)
	(let* ((exports (exports *instance*))
	       (swap (get-export exports "swap" 'wasm-func)))
	  (5am:is (equal '(22 11) (multiple-value-list (wasm-funcall swap 11 22)))))))))

(5am:test test-host-func
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
  	           (import \"math\" \"sum\" (func $sum (param i32 i32) (result i32)))
	           (func (export \"add_one\") (param $x i32) (result i32)
		     local.get $x
		     i32.const 1
		     call $sum))")
	     (module (wat-to-wasm-module *store* wat))
	     (func-type (make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (make-wasm-func *store*
					func-type
					(lambda (x y) (+ x y))))
	     (instance (make-wasm-instance *store*
					   module
					   (import-modules module
							   ("math" ("sum" host-func)))))
	     (add-one (get-export (exports instance) "add_one" 'wasm-func)))
	(5am:is (= 23 (wasm-funcall add-one 22)))))))

(5am:test test-host-func-with-wasm-val-args
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
  	           (import \"math\" \"sum\" (func $sum (param i32 i32) (result i32)))
	           (func (export \"add_one\") (param $x i32) (result i32)
		     local.get $x
		     i32.const 1
		     call $sum))")
	     (module (wat-to-wasm-module *store* wat))
	     (func-type (make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (make-wasm-func *store*
					func-type
					(lambda (x y)
					  (5am:is (eql :wasm-i32 (kind x)))
					  (5am:is (eql :wasm-i32 (kind y)))
					  (+ (value x) (value y)))
					:wasm-val-args t))
	     (instance (make-wasm-instance *store*
					   module
					   (import-modules module
							   ("math" ("sum" host-func)))))
	     (add-one (get-export (exports instance) "add_one" 'wasm-func)))
	(5am:is (= 23 (wasm-funcall add-one 22)))))))

(5am:test test-host-func-with-wasm-val-result
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
  	           (import \"math\" \"sum\" (func $sum (param i32 i32) (result i32)))
	           (func (export \"add_one\") (param $x i32) (result i32)
		     local.get $x
		     i32.const 1
		     call $sum))")
	     (module (wat-to-wasm-module *store* wat))
	     (func-type (make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (make-wasm-func *store*
					func-type
				        (lambda (x y)
					  (make-wasm-val (+ x y) :wasm-i32))))
	     (instance (make-wasm-instance *store*
					   module
					   (import-modules module
							   ("math" ("sum" host-func)))))
	     (add-one (get-export (exports instance) "add_one" 'wasm-func)))
	(5am:is (= 23 (wasm-funcall add-one 22)))))))


(5am:test test-host-func-with-environment
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
		   (import \"math\" \"sum\" (func $sum (param i32 i32) (result i32)))
		   (func (export \"add_one\") (param $x i32) (result i32)
			 local.get $x
			 i32.const 1
			 call $sum))")
	     (module (wat-to-wasm-module *store* wat))
	     (env '(:instance nil :answer 22))
	     (host-func (make-wasm-func *store*
					(make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32))
					(lambda (e x y)
					  (5am:is-false (null (getf e :instance)))
					  (+ x y (getf e :answer)))
					:environment env))
	     (imports (import-modules module
				      ("math" ("sum" host-func))))
	     (instance (make-wasm-instance *store* module imports)))
	(setf (getf env :instance) instance)
	(let ((add-one (get-export (exports instance) "add_one" 'wasm-func)))
	  (5am:is (= 30 (wasm-funcall add-one 7))))))))

(5am:test test-host-func-trap
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((wat "(module
		     (import \"math\" \"sum\" (func $sum (param i32 i32) (result i32)))
		     (func (export \"add_one\") (param $x i32) (result i32)
			 local.get $x
			 i32.const 1
			 call $sum))")
	     (module (wat-to-wasm-module *store* wat))
	     (host-func (make-wasm-func *store*
					(make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32))
					(lambda (&rest args)
					  (declare (ignore args))
					  (error "whoops"))))
	     (imports (import-modules module
				      ("math" ("sum" host-func))))
	     (instance (make-wasm-instance *store* module imports))
	     (add-one (get-export (exports instance) "add_one" 'wasm-func)))
	(5am:signals wasm-trap-error (wasm-funcall add-one 21))))))
