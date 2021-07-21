(defvar *memory-wat*
  "(module
     (type $mem_size_t (func (result i32)))
     (type $get_at_t (func (param i32) (result i32)))
     (type $set_at_t (func (param i32) (param i32)))
     (memory $mem 1)
     (func $get_at (type $get_at_t) (param $idx i32) (result i32)
	 (i32.load (local.get $idx)))
     (func $set_at (type $set_at_t) (param $idx i32) (param $val i32)
	 (i32.store (local.get $idx) (local.get $val)))
     (func $mem_size (type $mem_size_t) (result i32)
	 (memory.size))
     (export \"get_at\" (func $get_at))
     (export \"set_at\" (func $set_at))
     (export \"mem_size\" (func $mem_size))
     (export \"memory\" (memory $mem)))")

(defun run-memory-example ()
  (let* ((engine (wasm-rt:make-wasm-engine))
	 (store (wasm-rt:make-wasm-store engine))
	 (module (wasm-rt:wat-to-wasm store *memory-wat*))
	 (imports (wasm-rt:make-wasm-imports module))
	 (instance (wasm-rt:make-wasm-instance store module imports))
	 (exports (wasm-rt:exports instance))
	 (mem-size (wasm-rt:get-export exports "mem_size" 'wasm-rt:wasm-func))
	 (get-at (wasm-rt:get-export exports "get_at" 'wasm-rt:wasm-func))
	 (set-at (wasm-rt:get-export exports "set_at" 'wasm-rt:wasm-func))
	 (memory (wasm-rt:get-export exports "memory" 'wasm-rt:wasm-memory)))
    (format t "Memory size (pages): ~a~%" (wasm-rt:size memory))
    (format t "Memory size (pages as bytes): ~a~%" (wasm-rt:bytes (wasm-rt:size memory)))
    (format t "Memory size (byes): ~a~%" (wasm-rt:buffer-size memory))
    (format t "Memory size (pages): ~a~%" (wasm-rt:wasm-funcall mem-size))
    (wasm-rt:grow memory 2)
    (format t "New memory size (pages): ~a~%" (wasm-rt:size memory))
    (let ((mem-addr #x2220)
	  (val #xFEFEFFE))
      (wasm-rt:wasm-funcall set-at mem-addr val)
      (format t "Value at 0x~X: 0x~X~%" mem-addr (wasm-rt:wasm-funcall get-at mem-addr))
      (let* ((page-size #x10000)
	     (mem-addr (- (* page-size 2) (cffi:foreign-type-size :uint32)))
	     (val #xFEA09))
	(wasm-rt:wasm-funcall set-at mem-addr val)
	(format t "Value at 0x~X: 0x~X" mem-addr (wasm-rt:wasm-funcall get-at mem-addr))))))