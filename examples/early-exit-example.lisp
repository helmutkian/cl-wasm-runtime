(defvar *early-exit-wat*
  "(module
     (type $run_t (func (param i32 i32) (result i32)))
     (type $early_exit_t (func (param) (result)))
     (import \"env\" \"early_exit\" (func $early_exit (type $early_exit_t)))
     (func $run (type $run_t) (param $x i32) (param $y i32) (result i32)
       (call $early_exit)
       (i32.add
       local.get $x
       local.get $y))
     (export \"run\" (func $run)))")


(defun run-early-exit-example ()
  (let* ((engine (wasm-rt:make-wasm-engine))
	 (store (wasm-rt:make-wasm-store engine))
	 (module (wasm-rt:wat-to-wasm store *early-exit-wat*))
	 (host-functype (wasm-rt:make-wasm-functype nil nil))
	 (callback (wasm-rt:make-wasm-callback (lambda (&rest args)
						 (declare (ignore args))
						 (error "EARLY EXIT!"))))
	(host-func (wasm-rt:make-wasm-func store
					   host-functype
					   callback))
	 (env-namespace
	   (wasm-rt:make-wasm-namespace "env"
					(list (wasm-rt:make-wasm-import "early_exit"
								       host-func))))
	 (imports (wasm-rt:make-wasm-imports module (list env-namespace)))
	 (instance (wasm-rt:make-wasm-instance store module imports))
	 (exports (wasm-rt:exports instance))
	 (run (wasm-rt:get-export exports "run" 'wasm-rt:wasm-func)))
    (wasm-rt:wasm-funcall run 42 31)))
