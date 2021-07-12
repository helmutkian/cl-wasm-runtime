(defvar test-wat
   "(module
      (type $add_one_t (func (param i32) (result i32)))
      (func $add_one_f (type $add_one_t) (param $value i32) (result i32)
        local.get $value
        i32.const 1
        i32.add)
      (export \"add_one\" (func $add_one_f)))")

(defun run-test-module ()
  (let* ((engine (make-wasm-engine))
	 (store (make-wasm-store engine))
	 (module (wat-to-wasm-module store test-wat)))
    (cffi:with-foreign-objects ((imports '(:struct %wasm-extern-vec-type))
				(exports '(:struct %wasm-extern-vec-type)))
      (wasm-vec-init-empty imports '(:struct %wasm-extern-vec-type))
      (let* ((instance (make-wasm-instance store module imports)))
	(cond
	  ((cffi:null-pointer-p instance)
	    (error "Error instantiating instance"))
	  (t
	   (%wasm-instance-exports instance exports)
	   (cffi:with-foreign-slots ((size data) exports (:struct %wasm-extern-vec-type))
	     (if (zerop size)
		 (error "Error accessing exports")
		 (let ((add-one-func (%wasm-extern-as-func (cffi:mem-aref data
									  '%wasm-extern-type-ptr
									  0))))
		   (cond
		     ((cffi:null-pointer-p add-one-func)
		      (error "Error accessing export"))
		     (t
		      (cffi:with-foreign-objects ((arg-val '(:struct %wasm-val-struct) 1)
						  (result-val '(:struct %wasm-val-struct) 1)
						  (args '(:struct %wasm-val-vec-struct) 1)
						  (results '(:struct %wasm-val-vec-struct) 1))
			(wasm-val-init (cffi:mem-aptr arg-val '(:struct %wasm-val-struct) 0)
				       :wasm-i32
				       1)
			(wasm-val-init (cffi:mem-aptr result-val '(:struct %wasm-val-struct) 0)
				       :wasm-any-ref
				       (cffi:null-pointer))
			(wasm-vec-init args
				       '(:struct %wasm-val-vec-type)
				       arg-val
				       1)
			(wasm-vec-init results
				       '(:struct %wasm-val-vec-type)
				       result-val
				       1)
			(%wasm-func-call add-one-func args results)
			(prog1 #|(cffi:foreign-slot-value	(cffi:foreign-slot-value (cffi:mem-aptr result-val '(:struct %wasm-val-struct) 0)
										 '(:struct %wasm-val-struct)
										 'of)
							'(:union %wasm-val-union)
			    (wasm-val-type-kind-of :wasm-i32))|#
			    (cffi:convert-from-foreign (cffi:mem-aptr result-val '(:struct %wasm-val-struct) 0)
						     '%wasm-val-type)
			  (%wasm-extern-vec-delete exports))))))))))))))
			
		       
	 
		     
