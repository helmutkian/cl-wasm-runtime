(defpackage #:cl-wasm-runtime.test/func
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/module
	#:cl-wasm-runtime.test/fixture/instance)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:signed-to-unsigned))

(in-package #:cl-wasm-runtime.test/func)

(5am:def-suite* cl-wasm-runtime-test/func :in cl-wasm-runtime-test)

(5am:test test-guest-func
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-func.wasm")
    (5am:finishes
      (let ((sum (wasm-rt:exports-get *exports* "sum")))
	(5am:is (= 2 (wasm-rt:func-param-arity sum)))
	(5am:is (= 1 (wasm-rt:func-result-arity sum)))
	(5am:is (= 3 (wasm-rt:wasm-funcall sum 1 2)))))))

(5am:test test-guest-func-zero-params
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "arity_0")))
	(5am:is (= 42 (wasm-rt:wasm-funcall func)))))))

(5am:test test-guest-func-zero-results
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-func-zero-results.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "test")))
	(5am:is-true (null (wasm-rt:wasm-funcall func 1 2)))))))

(5am:test test-guest-func-zero-params-results
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-func-noop.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "noop")))
	(5am:is-true (null (wasm-rt:wasm-funcall func)))))))

(5am:test test-guest-func-multiple-results
  (5am:with-fixture instance-exports-fixture (:path "test/data/test-func-multiple-results.wasm")
    (5am:finishes
      (let ((swap (wasm-rt:exports-get *exports* "swap")))
	(5am:is (equal '(22 11) (multiple-value-list (wasm-rt:wasm-funcall swap 11 22))))))))

(5am:test test-guest-func-i32-to-i32
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "i32_i32"))
	    (max-uint32  #xFFFFFFFF))
	(5am:is (= max-uint32 (signed-to-unsigned (wasm-rt:wasm-funcall func max-uint32) 32)))
	(5am:is (= -42 (wasm-rt:wasm-funcall func -42)))))))

(5am:test test-guest-func-i64-to-i64
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "i64_i64"))
	    (max-uint64 #xFFFFFFFFFFFFFFFF))
	(5am:is (= max-uint64 (signed-to-unsigned (wasm-rt:wasm-funcall func max-uint64) 64)))
	(5am:is (= -42 (wasm-rt:wasm-funcall func -42)))))))

(5am:test test-guest-func-f32-to-f32
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "f32_f32")))
	(5am:is (= 7.42 (wasm-rt:wasm-funcall func 7.42)))))))

(5am:test test-guest-func-f64-to-f64
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "f64_f64")))
	(5am:is (= 7.42 (wasm-rt:wasm-funcall func 7.42)))))))

(5am:test test-guest-func-i32-i64-f32-f64-to-f64
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let ((func (wasm-rt:exports-get *exports* "i32_i64_f32_f64_f64")))
	(5am:is (= 12 (round (wasm-rt:wasm-funcall func 1 2 3.4 5.6d0))))))))

(5am:test test-host-func
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (x y) (+ x y))))
	     (instance (wasm-rt:make-wasm-instance *store*
						   *module*
						   (wasm-rt:imports ("math" ("sum" host-func)))))
	     (exports (wasm-rt:instance-exports instance))
	     (add-one (wasm-rt:exports-get exports "add_one")))
	(5am:is (= 23 (wasm-rt:wasm-funcall add-one 22)))))))

(5am:test test-host-func-with-args-as-wasm-vals
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (x y)
						  (5am:is (eql :wasm-i32 (wasm-rt:val-kind x)))
						  (5am:is (eql :wasm-i32 (wasm-rt:val-kind y)))
						  (+ (wasm-rt:val-value x)
						     (wasm-rt:val-value y)))
						:with-args-as-wasm-vals? t))
	     (instance (wasm-rt:make-wasm-instance *store*
						   *module*
						   (wasm-rt:imports ("math" ("sum" host-func)))))
	     (exports (wasm-rt:instance-exports instance))
	     (add-one (wasm-rt:exports-get exports "add_one")))
	(5am:is (= 23 (wasm-rt:wasm-funcall add-one 22)))))))

(5am:test test-host-func-with-result-as-wasm-val
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (x y)
						  (wasm-rt:make-wasm-val (+ x y) :wasm-i32))))
	     (instance (wasm-rt:make-wasm-instance *store*
						   *module*
						   (wasm-rt:imports ("math" ("sum" host-func)))))
	     (exports (wasm-rt:instance-exports instance))
	     (add-one (wasm-rt:exports-get exports "add_one")))
	(5am:is (= 23 (wasm-rt:wasm-funcall add-one 22)))))))

(5am:test test-host-func-with-environment
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (env '(:instance nil :answer 22))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (e x y)
						  (5am:is-false (null (getf e :instance)))
						  (+ x y (getf e :answer)))
						:environment env))
	     (imports (wasm-rt:imports ("math" ("sum" host-func))))
	     (instance (wasm-rt:make-wasm-instance *store* *module* imports)))
	(setf (getf env :instance) instance)
	(let ((add-one (wasm-rt:exports-get (wasm-rt:instance-exports instance) "add_one")))
	  (5am:is (= 30 (wasm-rt:wasm-funcall add-one 7))))))))

(5am:test test-host-func-missing-result
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (x y)
						  (declare (ignorable x y))
						  (values))))
	     (imports (wasm-rt:imports ("math" ("sum" host-func))))
	     (instance (wasm-rt:make-wasm-instance *store* *module* imports)))
	(5am:signals wasm-rt/error:wasm-error
	    (let ((add-one (wasm-rt:exports-get (wasm-rt:instance-exports instance) "add_one")))
	      (5am:is (= 30 (wasm-rt:wasm-funcall add-one 7)))))))))

(5am:test test-host-func-invalid-result
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (x y)
						  (declare (ignorable x y))
						  7.42)))
	     (imports (wasm-rt:imports ("math" ("sum" host-func))))
	     (instance (wasm-rt:make-wasm-instance *store* *module* imports)))
	(5am:signals wasm-rt/error:wasm-error
	    (let ((add-one (wasm-rt:exports-get (wasm-rt:instance-exports instance) "add_one")))
	      (5am:is (= 30 (wasm-rt:wasm-funcall add-one 7)))))))))

(5am:test test-host-func-condition
  (5am:with-fixture module-fixture (:path "test/data/test-host-func.wasm")
    (5am:finishes
      (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32 :wasm-i32) '(:wasm-i32)))
	     (host-func (wasm-rt:make-wasm-func *store*
						functype
						(lambda (x y)
						  (declare (ignorable x y))
						  (error "whoops!"))))
	     (imports (wasm-rt:imports ("math" ("sum" host-func))))
	     (instance (wasm-rt:make-wasm-instance *store* *module* imports)))
	(5am:signals wasm-rt/error:wasm-error
	    (let ((add-one (wasm-rt:exports-get (wasm-rt:instance-exports instance) "add_one")))
	      (5am:is (= 30 (wasm-rt:wasm-funcall add-one 7)))))))))


