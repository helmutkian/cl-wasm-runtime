(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.memory
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.memory)

(5am:test test-make-wasm-memory
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((min 1)
	     (max 7)
	     (limits (make-wasm-limits 1 7))
	     (memory-type (make-wasm-memorytype limits))
	     (memory (make-wasm-memory *store* memory-type))
	     (memory-type-from-memory (memory-type memory))
	     (limits-from-memory-type (limits memory-type-from-memory)))
	(5am:is (= min (minimum limits-from-memory-type)))
	(5am:is (= max (maximum limits-from-memory-type)))))))

(5am:test test-memory-size-and-grow
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((limits (make-wasm-limits 1 3))
	     (memory-type (make-wasm-memorytype limits))
	     (memory (make-wasm-memory *store* memory-type)))
	(5am:is (=  1 (size memory)))
	(grow memory 2)
	(5am:is (= 3 (size memory)))
	(grow memory 1)
      	(5am:is (= 3 (size memory)))))))

(5am:test test-memory-buffer-size
  (5am:finishes
    (5am:with-fixture engine-store-fixture ()
      (let* ((limits (make-wasm-limits 1 7))
	     (memory-type (make-wasm-memorytype limits))
	     (memory (make-wasm-memory *store* memory-type)))
	(5am:is (= #x10000 (buffer-size memory)))))))

(5am:test test-memory-buffer
  (5am:finishes
    (5am:with-fixture instance-fixture ()
      (let* ((exports (exports *instance*))
	     (get-string (get-export exports "string" 'wasm-func))
	     (memory-1 (get-export exports "memory" 'wasm-memory))
	     (buffer-1 (buffer memory-1))
	     (memory-2 (get-export exports "memory" 'wasm-memory))
	     (buffer-2 (buffer memory-2))
	     (pointer (wasm-funcall get-string))
	     (len (length "Hello, World!")))
	(5am:is (= 1048576 pointer))
	(5am:is (string= "Hello, World!"
			 (buffer-to-string buffer-1
					   :start pointer
					   :end (+ pointer len))))
	(5am:is (string= "Hello, World!"
			 (buffer-to-string buffer-2
					   :start pointer
					   :end (+ pointer len))))
	(setf (buffer-aref buffer-1 pointer) (char-code #\A))
	(5am:is (string= "Aello, World!"
			 (buffer-to-string buffer-1
					   :start pointer
					   :end (+ pointer len))))
	(5am:is (string= "Aello, World!"
			 (buffer-to-string buffer-2
					   :start pointer
					   :end (+ pointer len))))))))
