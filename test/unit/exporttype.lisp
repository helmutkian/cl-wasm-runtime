(defpackage #:cl-wasm-runtime.test/exporttype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/exporttype)

(5am:def-suite cl-wasm-runtime-test/exporttype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/exporttype)

(5am:test test-functype-into-exporttype-and-back
  (5am:finishes
    (let* ((params '(:wasm-i64))
	   (results '(:wasm-f64))
	   (functype (wasm-rt:make-wasm-functype params results))
	   (name "bar")
	   (exporttype (wasm-rt:make-wasm-exporttype name functype))
	   (externtype (wasm-rt:exporttype-type exporttype))
	   (externtype-functype (wasm-rt:from-externtype externtype)))
      (5am:is (string= name (wasm-rt:exporttype-name exporttype)))
      (5am:is (wasm-rt:wasm-object-eq? functype externtype-functype)))))

(5am:test test-globaltype-into-exporttype-and-back
  (5am:finishes
    (let* ((globaltype (wasm-rt:make-wasm-globaltype :wasm-i64))
	   (name "foo")
	   (exporttype (wasm-rt:make-wasm-exporttype name globaltype))
	   (externtype (wasm-rt:exporttype-type exporttype))
	   (externtype-globaltype (wasm-rt:from-externtype externtype)))
      (5am:is (string= name (wasm-rt:exporttype-name exporttype)))
      (5am:is (wasm-rt:wasm-object-eq? globaltype externtype-globaltype)))))

(5am:test test-memorytype-into-exporttype-and-back
  (5am:finishes
    (let* ((memorytype (wasm-rt:make-wasm-memorytype 1 7))
	   (name "foo")
	   (exporttype (wasm-rt:make-wasm-exporttype name memorytype))
	   (externtype (wasm-rt:exporttype-type exporttype))
	   (externtype-memorytype (wasm-rt:from-externtype externtype)))
      (5am:is (string= name (wasm-rt:exporttype-name exporttype)))
      (5am:is (wasm-rt:wasm-object-eq? memorytype externtype-memorytype)))))

(5am:test test-tabletype-into-exporttype-and-back
  (5am:finishes
    (let* ((tabletype (wasm-rt:make-wasm-tabletype :wasm-i32 1 7))
	   (name "foo")
	   (exporttype (wasm-rt:make-wasm-exporttype name tabletype))
	   (externtype (wasm-rt:exporttype-type exporttype))
	   (externtype-tabletype (wasm-rt:from-externtype externtype)))
      (5am:is (string= name (wasm-rt:exporttype-name exporttype)))
      (5am:is (wasm-rt:wasm-object-eq? tabletype externtype-tabletype)))))

(5am:test test-functype-externtype-wasm-object-eq?
  (let* ((functype (wasm-rt:make-wasm-functype '(:wasm-i32) '(:wasm-f32)))
	 (name "foo")
	 (exporttype (wasm-rt:make-wasm-exporttype name functype))
	 (functype-same (wasm-rt:make-wasm-functype '(:wasm-i32) '(:wasm-f32)))
	 (functype-diff (wasm-rt:make-wasm-functype '(:wasm-i64) '(:wasm-f32)))
	 (globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (memorytype (wasm-rt:make-wasm-memorytype 1 7))
	 (tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? exporttype exporttype))
    (5am:is-true
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype name functype-same)))
    (5am:is-false
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype "bar" functype-same)))
    (loop for externable in (list functype-diff globaltype memorytype tabletype)
	  do (5am:is-false
	      (wasm-rt:wasm-object-eq? exporttype
				       (wasm-rt:make-wasm-exporttype name externable))))))

(5am:test test-globaltype-exporttype-wasm-object-eq?
  (let* ((globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (name "foo")
	 (exporttype (wasm-rt:make-wasm-exporttype name globaltype))
	 (globaltype-same (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (globaltype-diff (wasm-rt:make-wasm-globaltype :wasm-i32))
	 (functype (wasm-rt:make-wasm-functype '() '()))
	 (memorytype (wasm-rt:make-wasm-memorytype 1 7))
	 (tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? exporttype exporttype))
    (5am:is-true
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype name globaltype-same)))
    (5am:is-false
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype "bar" globaltype-same)))
    (loop for externable in (list globaltype-diff functype memorytype tabletype)
	  do (5am:is-false
	      (wasm-rt:wasm-object-eq? exporttype
				       (wasm-rt:make-wasm-exporttype name externable))))))

(5am:test test-memorytype-exporttype-wasm-object-eq?
  (let* ((memorytype (wasm-rt:make-wasm-memorytype 1 7))
	 (name "foo")
	 (exporttype (wasm-rt:make-wasm-exporttype name memorytype))
	 (memorytype-same (wasm-rt:make-wasm-memorytype 1 7))
	 (memorytype-diff (wasm-rt:make-wasm-memorytype 2 8))
	 (functype (wasm-rt:make-wasm-functype '() '()))
	 (globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? exporttype exporttype))
    (5am:is-true
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype name memorytype-same)))
    (5am:is-false
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype "bar" memorytype-same)))
    (loop for externable in (list memorytype-diff functype globaltype tabletype)
	  do (5am:is-false
	      (wasm-rt:wasm-object-eq? exporttype
				       (wasm-rt:make-wasm-exporttype name externable))))))

(5am:test test-tabletype-exporttype-wasm-object-eq?
  (let* ((tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7))
	 (name "foo")
	 (exporttype (wasm-rt:make-wasm-exporttype name tabletype))
	 (tabletype-same (wasm-rt:make-wasm-tabletype :wasm-f64 1 7))
	 (tabletype-diff (wasm-rt:make-wasm-tabletype :wasm-f64 2 8))
	 (functype (wasm-rt:make-wasm-functype '() '()))
	 (globaltype (wasm-rt:make-wasm-globaltype :wasm-f64))
	 (memorytype (wasm-rt:make-wasm-memorytype 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? exporttype exporttype))
    (5am:is-true
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype name tabletype-same)))
    (5am:is-false
     (wasm-rt:wasm-object-eq? exporttype
			      (wasm-rt:make-wasm-exporttype "bar" tabletype-same)))
    (loop for externable in (list tabletype-diff functype globaltype memorytype)
	  do (5am:is-false
	      (wasm-rt:wasm-object-eq? exporttype
				       (wasm-rt:make-wasm-exporttype name externable))))))

