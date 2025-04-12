(defpackage #:cl-wasm-runtime.test/byte-vec
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cl-wasm-runtime))

(in-package #:cl-wasm-runtime.test/byte-vec)

(5am:def-suite cl-wasm-runtime-test/byte-vec :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/byte-vec)

(5am:test test-byte-vec-string
  (5am:finishes
    (let* ((string "foobar")
	   (byte-vec (wasm-rt:string-to-wasm-byte-vec string))
	   (string-from-byte-vec (wasm-rt:wasm-byte-vec-to-string byte-vec)))
      (5am:is (string= string string-from-byte-vec)))))

(5am:test test-byte-vec-octets
  (5am:finishes
    (let* ((octets (babel:string-to-octets "foobar"))
	   (byte-vec (wasm-rt:octets-to-wasm-byte-vec octets))
	   (octets-from-byte-vec (wasm-rt:wasm-byte-vec-to-octets byte-vec)))
      (loop for expected across octets
	    for actual across octets-from-byte-vec
	    do (5am:is (eql expected actual))))))

(5am:test test-make-wasm-name
  (5am:finishes
    (let* ((str "foobar")
	   (name (wasm-rt:make-wasm-name str)))
      (5am:is (string= str (wasm-rt:wasm-byte-vec-to-string name))))))

(5am:test test-byte-vec-wasm-object-eq?
  (let ((byte-vec (wasm-rt:string-to-wasm-byte-vec "foobar")))
    (5am:is (wasm-rt:wasm-object-eq? byte-vec byte-vec))
    (5am:is (wasm-rt:wasm-object-eq? byte-vec (wasm-rt:string-to-wasm-byte-vec "foobar")))))

(5am:test test-name-wasm-object-eq?
  (let ((name (wasm-rt:make-wasm-name "foobar")))
    ;; Same pointer
    (5am:is (wasm-rt:wasm-object-eq? name name))
    ;; Name with same string
    (5am:is (wasm-rt:wasm-object-eq? name (wasm-rt:make-wasm-name "foobar")))
    ;; Byte-vec with same string
    (5am:is (wasm-rt:wasm-object-eq? name (wasm-rt:string-to-wasm-byte-vec "foobar")))
    (5am:is (wasm-rt:wasm-object-eq? (wasm-rt:string-to-wasm-byte-vec "foobar") name))
    ;; Same string
    (5am:is (wasm-rt:wasm-object-eq? name "foobar"))
    (5am:is (wasm-rt:wasm-object-eq? "foobar" name))
    ;; Name with different string
    (5am:is-false (wasm-rt:wasm-object-eq? name (wasm-rt:make-wasm-name "quuxquuy")))
    ;; Byte-vec with different string
    (5am:is-false (wasm-rt:wasm-object-eq? name (wasm-rt:string-to-wasm-byte-vec "quuxquuy")))
    (5am:is-false (wasm-rt:wasm-object-eq?  (wasm-rt:string-to-wasm-byte-vec "quuxquuy") name))
    ;; Different string
    (5am:is-false (wasm-rt:wasm-object-eq? name "quuxquuy"))
    (5am:is-false (wasm-rt:wasm-object-eq? "quuxquuy" name))))
