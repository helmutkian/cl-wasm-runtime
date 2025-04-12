(defpackage #:cl-wasm-runtime.test/globaltype
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/globaltype)

(5am:def-suite cl-wasm-runtime-test/globaltype :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test)

(5am:test test-make-wasm-globaltype-mutable
  (5am:finishes
    (let ((globaltype (wasm-rt:make-wasm-globaltype :wasm-i32 :mutable? t)))
      (5am:is-true (wasm-rt:globaltype-mutable? globaltype))
      (5am:is (wasm-rt:wasm-object-eq? :wasm-i32 (wasm-rt:globaltype-type globaltype))))))

(5am:test test-make-wasm-globaltype-immutable
  (5am:finishes
    (let* ((valtype (wasm-rt:make-wasm-valtype :wasm-i64))
	   (globaltype (wasm-rt:make-wasm-globaltype valtype :mutable? nil)))
      (5am:is-false (wasm-rt:globaltype-mutable? globaltype))
      (5am:is (wasm-rt:wasm-object-eq? valtype (wasm-rt:globaltype-type globaltype))))))

(5am:test test-globaltype-wasm-object-eq?
  (let ((globaltype (wasm-rt:make-wasm-globaltype :wasm-i32 :mutable? nil)))
    (5am:is-true (wasm-rt:wasm-object-eq? globaltype globaltype))
    (5am:is-true (wasm-rt:wasm-object-eq? globaltype
					  (wasm-rt:make-wasm-globaltype :wasm-i32 :mutable? nil)))
    (5am:is-false (wasm-rt:wasm-object-eq? globaltype
					  (wasm-rt:make-wasm-globaltype :wasm-i32 :mutable? t)))
    (5am:is-false (wasm-rt:wasm-object-eq? globaltype
					   (wasm-rt:make-wasm-globaltype :wasm-f64)))))
