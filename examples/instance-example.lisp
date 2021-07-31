(defvar *add-one-wat*
   "(module
      (type $add_one_t (func (param i32) (result i32)))
      (func $add_one_f (type $add_one_t) (param $value i32) (result i32)
        local.get $value
        i32.const 1
        i32.add)
      (export \"add_one\" (func $add_one_f)))")

(defun run-instance-example ()
  (let* ((engine (wasm-rt:make-wasm-engine))
	 (store (wasm-rt:make-wasm-store engine))
	 (module (wasm-rt:wat-to-wasm-module store *add-one-wat*))
	 (imports (wasm-rt:make-wasm-imports module))
	 (instance (wasm-rt:make-wasm-instance store module imports))
	 (add-one (wasm-rt:get-export (wasm-rt:exports instance)
				       "add_one"
				       'wasm-rt:wasm-func)))
    (wasm-rt:wasm-funcall add-one 1)))
