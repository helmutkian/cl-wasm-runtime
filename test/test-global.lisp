(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.global
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.global)

(defvar *test-global-wat*
  "(module
     (global $x (export \"x\") (mut i32) (i32.const 0))
     (global $y (export \"y\") (mut i32) (i32.const 7))
     (global $z (export \"z\") i32 (i32.const 42))
     (func (export \"get_x\") (result i32)
	 (global.get $x))
     (func (export \"increment_x\")
	 (global.set $x
		     (i32.add (global.get $x) (i32.const 1)))))")

(defvar *test-global-instance*)

(5am:def-fixture global-instance-fixture (&optional (wat *test-global-wat*))
  (let* ((engine (make-wasm-engine))
	 (store (make-wasm-store engine))
	 (module (wat-to-wasm-module store wat))
	 (*test-global-instance* (make-wasm-instance store module)))
    (&body)))

(5am:test test-global-type
  (5am:with-fixture global-instance-fixture ()
    (5am:finishes
      (let* ((exports (exports *test-global-instance*))
	     (global (get-export exports "x" 'wasm-global))
	     (global-type (global-type global)))
	(5am:is (not (null global)))
	(5am:is (eql :wasm-i32 (kind (value-type global-type))))
	(5am:is (mutable? global-type))))))

(5am:test test-global-mutable?
  (5am:with-fixture global-instance-fixture ()
    (5am:finishes
      (let* ((exports (exports *test-global-instance*))
	     (x (get-export exports "x" 'wasm-global))
	     (y (get-export exports "y" 'wasm-global))
	     (z (get-export exports "z" 'wasm-global)))
	(5am:is (mutable? (global-type x)))
	(5am:is (mutable? (global-type y)))
	(5am:is (not (mutable? (global-type z))))))))

(5am:test test-global-value
  (5am:with-fixture global-instance-fixture ()
    (5am:finishes
      (let* ((exports (exports *test-global-instance*))
	     (global (get-export exports "y" 'wasm-global)))
	(5am:is (= 7 (value global)))
	;; Set with explicit WASM-VAL 
	(setf (value global) (make-wasm-val 8 :wasm-i32))
	(5am:is (= 8 (value global)))
	;; Set with WASM val type inference
	(setf (value global) 6)
	(5am:is (= 6 (value global)))))))

(5am:test test-global-with-exported-funcs
  (5am:with-fixture global-instance-fixture ()
    (5am:finishes
      (let* ((exports (exports *test-global-instance*))
	     (global (get-export exports "x" 'wasm-global))
	     (get-global (get-export exports "get_x" 'wasm-func))
	     (incr-global (get-export exports "increment_x" 'wasm-func)))
	(5am:is (= 0 (value global)))
	(setf (value global) 1)
	(5am:is (= 1 (wasm-funcall get-global)))
	(wasm-funcall incr-global)
	(5am:is (= 2 (wasm-funcall get-global)))))))

(5am:test test-global-immutable
  (5am:with-fixture global-instance-fixture ()
    (5am:finishes
      (let* ((exports (exports *test-global-instance*))
	     (global (get-export exports "z" 'wasm-global)))
	(5am:is (not (mutable? (global-type global))))
	(5am:is (= 42 (value global)))
	(5am:signals t (setf (value global) 84))
	(5am:is (= 42 (value global)))))))
