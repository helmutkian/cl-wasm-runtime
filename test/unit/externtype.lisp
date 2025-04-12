(defpackage #:cl-wasm-runtime.test/externtype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/externtype)

(5am:def-suite cl-wasm-runtime-test/externtype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/externtype)

(5am:test functype-to-externtype-and-back
  (5am:finishes
    (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32) '(:wasm-f32)))
	   (externtype (wasm-rt:to-externtype functype))
	   (externtype-functype (wasm-rt:from-externtype externtype)))
      (5am:is (wasm-rt:wasm-object-eq? functype externtype-functype)))))

(5am:test globaltype-to-externtype-and-back
  (5am:finishes
    (let* ((globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	   (externtype (wasm-rt:to-externtype globaltype))
	   (externtype-globaltype (wasm-rt:from-externtype externtype)))
      (5am:is (wasm-rt:wasm-object-eq? (wasm-rt:globaltype-type globaltype)
				       (wasm-rt:globaltype-type externtype-globaltype))))))

(5am:test memorytype-to-externtype-and-back
  (5am:finishes
    (let* ((memorytype (wasm-rt:make-wasm-memorytype 1 7))
	   (externtype (wasm-rt:to-externtype memorytype))
	   (externtype-memorytype (wasm-rt:from-externtype externtype)))
      (5am:is (wasm-rt:wasm-object-eq? memorytype externtype-memorytype)))))

(5am:test tabletype-to-externtype-and-back
  (5am:finishes
    (let* ((tabletype (wasm-rt:make-wasm-tabletype :wasm-i64 1 7))
	   (externtype (wasm-rt:to-externtype tabletype))
	   (externtype-tabletype (wasm-rt:from-externtype externtype)))
      (5am:is (wasm-rt:wasm-object-eq? tabletype externtype-tabletype)))))

(5am:test test-functype-externtype-wasm-object-eq?
  (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32) '(:wasm-f32)))
	 (externtype (wasm-rt:to-externtype functype))
	 (functype-same (wasm-rt:make-wasm-functype '(:wasm-i32) '(:wasm-f32)))
	 (functype-diff (wasm-rt:make-wasm-functype '(:wasm-i64) '(:wasm-f32)))
	 (globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (memorytype (wasm-rt:make-wasm-memorytype 1 7))
	 (tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype externtype))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype
					  (wasm-rt:to-externtype functype-same)))
    (loop for externable in (list functype-diff globaltype memorytype tabletype)
	  do (5am:is-false (wasm-rt:wasm-object-eq? externtype
						    (wasm-rt:to-externtype externable))))))

(5am:test test-globaltype-externtype-wasm-object-eq?
  (let* ((globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (externtype (wasm-rt:to-externtype globaltype))
	 (globaltype-same (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (globaltype-diff (wasm-rt:make-wasm-globaltype :wasm-i32))
	 (functype (wasm-rt:make-wasm-functype '() '()))
	 (memorytype (wasm-rt:make-wasm-memorytype 1 7))
	 (tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype externtype))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype
					  (wasm-rt:to-externtype globaltype-same)))
    (loop for externable in (list globaltype-diff functype memorytype tabletype)
	  do (5am:is-false (wasm-rt:wasm-object-eq? externtype
						    (wasm-rt:to-externtype externable))))))

(5am:test test-memorytype-externtype-wasm-object-eq?
  (let* ((memorytype (wasm-rt:make-wasm-memorytype 1 7))
	 (externtype (wasm-rt:to-externtype memorytype))
	 (memorytype-same (wasm-rt:make-wasm-memorytype 1 7))
	 (memorytype-diff (wasm-rt:make-wasm-memorytype 2 8))
	 (functype (wasm-rt:make-wasm-functype '() '()))
	 (globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype externtype))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype
					  (wasm-rt:to-externtype memorytype-same)))
    (loop for externable in (list memorytype-diff functype globaltype tabletype)
	  do (5am:is-false (wasm-rt:wasm-object-eq? externtype
						    (wasm-rt:to-externtype externable))))))

(5am:test test-tabletype-externtype-wasm-object-eq?
  (let* ((tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7))
	 (externtype (wasm-rt:to-externtype tabletype))
	 (tabletype-same (wasm-rt:make-wasm-tabletype :wasm-f64 1 7))
	 (tabletype-diff (wasm-rt:make-wasm-tabletype :wasm-f64 2 8))
	 (functype (wasm-rt:make-wasm-functype '() '()))
	 (globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (memorytype (wasm-rt:make-wasm-memorytype 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype externtype))
    (5am:is-true (wasm-rt:wasm-object-eq? externtype
					  (wasm-rt:to-externtype tabletype-same)))
    (loop for externable in (list tabletype-diff functype globaltype memorytype)
	  do (5am:is-false (wasm-rt:wasm-object-eq? externtype
						    (wasm-rt:to-externtype externable))))))
