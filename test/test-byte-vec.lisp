(in-package #:cl-wasm-runtime.test)

(5am:def-suite cl-wasm-runtime.test.byte-vec
  :in cl-wasm-runtime.test)

(5am:in-suite cl-wasm-runtime.test.byte-vec)

(5am:test test-byte-vec-string
  (let* ((string "foobar")
	 (byte-vec (string-to-wasm-byte-vec string))
	 (string-from-byte-vec (wasm-byte-vec-to-string byte-vec)))
    (5am:is (string= string string-from-byte-vec))))

(5am:test test-byte-vec-octets
  (let* ((octets (babel:string-to-octets "foobar"))
	 (byte-vec (octets-to-wasm-byte-vec octets))
	 (octets-from-byte-vec (wasm-byte-vec-to-octets byte-vec)))
    (loop for expected across octets
	  for actual across octets-from-byte-vec
	  do (5am:is (eql expected actual)))))
