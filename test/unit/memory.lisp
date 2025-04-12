(defpackage #:cl-wasm-runtime.test/memory
  (:use #:cl
	#:cl-wasm-runtime.test/suite
	#:cl-wasm-runtime.test/fixture/store
	#:cl-wasm-runtime.test/fixture/instance)
  (:import-from #:fiveam)
  (:import-from #:babel)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/memory)

(5am:def-suite* cl-wasm-runtime-test/memory :in cl-wasm-runtime-test)

(5am:test test-make-wasm-memory
  (5am:with-fixture store-fixture ()
    (5am:finishes
      (wasm-rt:make-wasm-memory *store* '(1 7)))))

(5am:test test-memory-type
  (5am:with-fixture store-fixture ()
    (5am:finishes
      (let* ((memory (wasm-rt:make-wasm-memory *store* '(1 7)))
	     (memorytype (wasm-rt:memory-type memory)))
	(5am:is (equal '(1 7)
		       (multiple-value-list (wasm-rt:memorytype-limits memorytype))))))))

(5am:test test-memory-grow
  (5am:with-fixture store-fixture ()
    (5am:finishes
      (let ((memory (wasm-rt:make-wasm-memory *store* '(1 3))))
	(5am:is (= 1 (wasm-rt:memory-size memory)))
	(5am:is-true (wasm-rt:memory-grow memory 2))
	(5am:is (= 3 (wasm-rt:memory-size memory)))
	(5am:is-false (wasm-rt:memory-grow memory 1))))))

(5am:test test-memory-data-size
  (5am:with-fixture store-fixture ()
    (5am:finishes
      (let ((memory (wasm-rt:make-wasm-memory *store* '(1 7))))
	(5am:is (= (wasm-rt:pages-num-bytes 1) (wasm-rt:memory-data-size memory)))))))

;; TODO: Function test
(5am:test test-memory-data
  (5am:with-fixture instance-exports-fixture (:path "test/data/test.wasm")
    (5am:finishes
      (let* ((offset (wasm-rt:wasm-funcall (wasm-rt:exports-get *exports* "string")))
	     (memory-1 (wasm-rt:exports-get *exports* "memory"))
	     (memory-2 (wasm-rt:exports-get *exports* "memory"))
	     (len (length "Hello, World!")))
	(5am:is (= 1048576 offset))
	(5am:is (string= "Hello, World!"
			 (wasm-rt:memory-buffer-to-string (wasm-rt:memory-data memory-1)
							  :start offset
							  :end (+ offset len))))
	(5am:is (string= "Hello, World!"
			 (wasm-rt:memory-buffer-to-string (wasm-rt:memory-data memory-2)
							  :start offset
							  :end (+ offset len))))
	(setf (wasm-rt:memory-buffer-aref (wasm-rt:memory-data memory-1) offset)
	      (char-code #\A))
	(5am:is (string= "Aello, World!"
			 (wasm-rt:memory-buffer-to-string (wasm-rt:memory-data memory-1)
							  :start offset
							  :end (+ offset len))))
	(5am:is (string= "Aello, World!"
			 (wasm-rt:memory-buffer-to-string (wasm-rt:memory-data memory-2)
							  :start offset
							  :end (+ offset len))))))))

