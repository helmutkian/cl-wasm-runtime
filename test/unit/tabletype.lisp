(defpackage #:cl-wasm-runtime.test/tabletype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/tabletype)

(5am:def-suite cl-wasm-runtime-test/tabletype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/tabletype)

(5am:test test-make-wasm-tabletype
  (5am:finishes
    (let* ((min 1)
	   (max 7)
	   (kind :wasm-f64)
	   (tabletype (wasm-rt:make-wasm-tabletype kind min max)))
      (multiple-value-bind (tabletype-min tabletype-max)
	  (wasm-rt:tabletype-limits tabletype)
	(5am:is (wasm-rt:wasm-object-eq? kind (wasm-rt:tabletype-type tabletype)))
	(5am:is (= min tabletype-min))
	(5am:is (= max tabletype-max))))))

(5am:test test-tabletype-wasm-object-eq?
  (let ((tabletype (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-true (wasm-rt:wasm-object-eq? tabletype tabletype))
    (5am:is-true (wasm-rt:wasm-object-eq? tabletype
					  (wasm-rt:make-wasm-tabletype :wasm-f64 1 7)))
    (5am:is-false (wasm-rt:wasm-object-eq? tabletype
					   (wasm-rt:make-wasm-tabletype :wasm-i32 1 7)))
    (5am:is-false (wasm-rt:wasm-object-eq? tabletype
					   (wasm-rt:make-wasm-tabletype :wasm-f64 1 8)))
    (5am:is-false (wasm-rt:wasm-object-eq? tabletype
					   (wasm-rt:make-wasm-tabletype :wasm-f64 2 7)))))
