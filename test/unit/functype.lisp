(defpackage #:cl-wasm-runtime.test/functype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/functype)

(5am:def-suite cl-wasm-runtime-test/functype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/functype)

(5am:test test-make-wasm-functype
  (5am:finishes
    (let* ((params (list :wasm-i32
			 (wasm-rt:make-wasm-valtype :wasm-i64)
			 :wasm-f32
			 (wasm-rt:make-wasm-valtype :wasm-f64)))
	   (results (list (wasm-rt:make-wasm-valtype :wasm-i32)
			  :wasm-i64
			  (wasm-rt:make-wasm-valtype :wasm-f32)
			  :wasm-f64))
	   (functype (wasm-rt:make-wasm-functype params results))
	   (functype-params (wasm-rt:functype-params functype))
	   (functype-results (wasm-rt:functype-results functype)))
      (5am:is (= (length params) (length functype-params)))
      (5am:is (= (length results) (length functype-results)))
      (loop for expected in params
	    for actual in functype-params
	    do (5am:is (wasm-rt:wasm-object-eq? expected actual)))
      (loop for expected in results
	    for actual in functype-results
	    do (5am:is (wasm-rt:wasm-object-eq? expected actual))))))

(5am:test test-functype-wasm-object-eq?
  (let* ((params '(:wasm-i64))
	 (results '(:wasm-f64))
	 (functype (wasm-rt:make-wasm-functype params results)))
    (5am:is-true (wasm-rt:wasm-object-eq? functype functype))
    (5am:is-true (wasm-rt:wasm-object-eq? functype
					  (wasm-rt:make-wasm-functype params results)))
    (5am:is-false (wasm-rt:wasm-object-eq? functype
					  (wasm-rt:make-wasm-functype '(:wasm-i32) '(:wasm-f64))))
    (5am:is-false (wasm-rt:wasm-object-eq? functype
					  (wasm-rt:make-wasm-functype '(:wasm-i64) '(:wasm-f32))))
    (5am:is-false (wasm-rt:wasm-object-eq? functype
					   (wasm-rt:make-wasm-functype '() results)))
    (5am:is-false (wasm-rt:wasm-object-eq? functype
					   (wasm-rt:make-wasm-functype params '())))))
