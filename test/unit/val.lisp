(defpackage #:cl-wasm-runtime.test/val
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:ieee-floats)
  (:import-from #:cl-wasm-runtime)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:signed-to-unsigned))

(in-package #:cl-wasm-runtime.test/val)

(5am:def-suite cl-wasm-runtime-test/val :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/val)

(5am:test test-make-wasm-val-i32
  (let ((min-int32 -2147483648)
	(max-int32 #x7FFFFFFF)
	(max-uint32  #xFFFFFFFF))
    (5am:finishes
      (5am:is (= min-int32
		 (wasm-rt:val-value (wasm-rt:make-wasm-val min-int32 :wasm-i32))))
      (5am:is (= max-int32
		 (wasm-rt:val-value (wasm-rt:make-wasm-val max-int32 :wasm-i32))))
      (5am:is (= max-uint32
		 (wasm-rt/util:signed-to-unsigned
		  (wasm-rt:val-value (wasm-rt:make-wasm-val max-uint32 :wasm-i32))
		  32))))
    (5am:signals wasm-rt:wasm-translate-error
      (wasm-rt:make-wasm-val (1- min-int32) :wasm-i32)
      (wasm-rt:make-wasm-val (1+ max-uint32) :wasm-i32)
      (wasm-rt:make-wasm-val 1.5 :wasm-i32))))

(5am:test test-make-wasm-val-i64
  (let ((min-int64 -9223372036854775808)
	(max-int64 #x7FFFFFFFFFFFFFFF)
	(max-uint64 #xFFFFFFFFFFFFFFFF))
    (5am:finishes
      (5am:is (= min-int64
		 (wasm-rt:val-value (wasm-rt:make-wasm-val min-int64 :wasm-i64))))
      (5am:is (= max-int64
		 (wasm-rt:val-value (wasm-rt:make-wasm-val max-int64 :wasm-i64))))
      (5am:is (= max-uint64
		 (signed-to-unsigned
		  (wasm-rt:val-value (wasm-rt:make-wasm-val max-uint64 :wasm-i64))
		  64))))
    (5am:signals wasm-rt:wasm-translate-error
      (wasm-rt:make-wasm-val (1- min-int64) :wasm-i64)
      (wasm-rt:make-wasm-val (1+ max-uint64) :wasm-i64)
      (wasm-rt:make-wasm-val 1.5 :wasm-i64))))

(5am:test test-make-wasm-val-floats
  (let ((min-f32 (ieee-floats:decode-float32 #xFF7FFFFF))
	(max-f32 (ieee-floats:decode-float32 #x7F7FFFFF))
	(min-f64 (ieee-floats:decode-float64 #xFFEFFFFFFFFFFFFF))
	(max-f64 (ieee-floats:decode-float64 #x7FEFFFFFFFFFFFFF)))
    (5am:finishes
      (5am:is (= min-f32
		 (wasm-rt:val-value (wasm-rt:make-wasm-val min-f32 :wasm-f32))))
      (5am:is (= max-f32
		 (wasm-rt:val-value (wasm-rt:make-wasm-val max-f32 :wasm-f32))))
      (5am:is (= min-f64
		 (wasm-rt:val-value (wasm-rt:make-wasm-val min-f64 :wasm-f64))))
      (5am:is (= max-f64
		 (wasm-rt:val-value (wasm-rt:make-wasm-val max-f64 :wasm-f64)))))
    (5am:signals wasm-rt:wasm-translate-error
      (wasm-rt:make-wasm-val (the integer 1) :wasm-f32)
      (wasm-rt:make-wasm-val (the integer 2) :wasm-f64)
      (wasm-rt:make-wasm-val min-f64 :wasm-f32)
      (wasm-rt:make-wasm-val max-f64 :wasm-f32))))

(5am:test test-val-i32-wasm-object-eq?
  (let ((val (wasm-rt:make-wasm-val 42 :wasm-i32)))
    (5am:is-true (wasm-rt:wasm-object-eq? val val))
    (5am:is-true (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42 :wasm-i32)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42 :wasm-i64)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 32 :wasm-i32)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42.0 :wasm-f32)))))

(5am:test test-val-i64-wasm-object-eq?
  (let* ((i64 #xFFFFFFFF7FFFFFFF)
	 (val (wasm-rt:make-wasm-val i64 :wasm-i64)))
    (5am:is-true (wasm-rt:wasm-object-eq? val val))
    (5am:is-true (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val i64 :wasm-i64)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val (1- i64) :wasm-i64)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42.0d0 :wasm-f64)))))

(5am:test test-val-f32-wasm-object-eq?
  (let ((val (wasm-rt:make-wasm-val 42.0 :wasm-f32)))
    (5am:is-true (wasm-rt:wasm-object-eq? val val))
    (5am:is-true (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42.0 :wasm-f32)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 32.0 :wasm-f32)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42.0d0 :wasm-f64)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 42 :wasm-i32)))))

(5am:test test-val-f64-wasm-object-eq?
  (let* ((f64 1.7976931348623157d308)
	 (val (wasm-rt:make-wasm-val f64 :wasm-f64)))
    (5am:is-true (wasm-rt:wasm-object-eq? val val))
    (5am:is-true (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val f64 :wasm-f64)))
    (5am:is-false (wasm-rt:wasm-object-eq? val
					   (wasm-rt:make-wasm-val (- f64 1.0d308) :wasm-f64)))
    (5am:is-false (wasm-rt:wasm-object-eq? val (wasm-rt:make-wasm-val 100 :wasm-i64)))))
