(defpackage #:cl-wasm-runtime.test/valtype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/valtype)

(5am:def-suite cl-wasm-runtime-test/valtype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/valtype)

(5am:test test-make-wasm-valtype
  (5am:finishes
    (let ((kinds '(:wasm-i32 :wasm-i64 :wasm-f32 :wasm-f64 :wasm-funcref :wasm-anyref)))
      (loop for kind in kinds
	    for valtype = (wasm-rt:make-wasm-valtype kind)
	    do (5am:is (eql kind (wasm-rt:valtype-kind valtype)))))))

(5am:test test-valtype-number?
  (5am:finishes
    (let ((numeric-kinds '(:wasm-i32 :wasm-i64 :wasm-f32 :wasm-f64))
	  (non-numeric-kinds '(:wasm-funcref :wasm-anyref)))
      (loop for kind in numeric-kinds
	    for valtype = (wasm-rt:make-wasm-valtype kind)
	    do (5am:is-true (wasm-rt:valtype-number? valtype)))
      (loop for kind in non-numeric-kinds
	    for valtype = (wasm-rt:make-wasm-valtype kind)
	    do (5am:is-false (wasm-rt:valtype-number? kind))))))

(5am:test test-valtype-reference?
  (5am:finishes
    (let ((non-ref-kinds '(:wasm-i32 :wasm-i64 :wasm-f32 :wasm-f64))
	  (ref-kinds '(:wasm-funcref :wasm-anyref)))
      (loop for kind in non-ref-kinds
	    for valtype = (wasm-rt:make-wasm-valtype kind)
	    do (5am:is-false (wasm-rt:valtype-reference? valtype)))
      (loop for kind in ref-kinds
	    for valtype = (wasm-rt:make-wasm-valtype kind)
	    do (5am:is-true (wasm-rt:valtype-reference? kind))))))

(5am:test test-valtype-wasm-object-eq?
  (let* ((kind :wasm-i64)
	 (valtype (wasm-rt:make-wasm-valtype kind)))
    (5am:is-true (wasm-rt:wasm-object-eq? valtype valtype))
    (5am:is-true (wasm-rt:wasm-object-eq? valtype kind))
    (5am:is-true (wasm-rt:wasm-object-eq? kind valtype))
    (5am:is-true (wasm-rt:wasm-object-eq? valtype (wasm-rt:make-wasm-valtype kind)))
    (5am:is-false (wasm-rt:wasm-object-eq? valtype (wasm-rt:make-wasm-valtype :wasm-f32)))
    (5am:is-false (wasm-rt:wasm-object-eq? valtype :wasm-f32))))
