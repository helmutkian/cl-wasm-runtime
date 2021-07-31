(asdf:defsystem "cl-wasm-runtime"
  :depends-on ("cffi"
	       "trivial-garbage"
	       "trivial-backtrace"
	       "alexandria"
	       "split-sequence"
	       "fast-io"
	       "babel"
	       "bordeaux-threads")
  :components ((:file "package")
	       
	       (:file "rwlock"
		:depends-on ("package"))
	       
	       (:file "wasm-object"
		:depends-on ("package"))
	       
	       (:file "common"
		:depends-on ("package"
			      "wasm-object"))
	       
	       (:file "runtime"
		:depends-on ("package"
			     "common"))
	       
	       (:file "wasm-type"
		:depends-on ("package"
			     "common"))
	       
	       (:file "wasm-val"
		:depends-on ("package"
			     "common"
			     "wasm-type"))

	       (:file "wasm-frame"
		:depends-on ("package"
			     "common"
			     "wasm-type"))

	       (:file "wasm-trap"
		:depends-on ("package"
			     "common"
			     "wasm-type"))

	       (:file "wasm-foreign"
		:depends-on ("package"
			     "common"
			     "wasm-type"))

	       (:file "wasm-func"
		:depends-on ("package"
			     "common"
			     "wasm-type"
			     "wasm-val"
			     "wasm-trap"
			     "rwlock"))

	       (:file "wasm-global"
		:depends-on ("package"
			     "common"
			     "wasm-type"
			     "wasm-val"))

	       (:file "wasm-table"
		:depends-on ("package"
			     "common"
			     "wasm-type"))

	       (:file "wasm-memory"
		:depends-on ("package"
			     "common"
			     "wasm-type"))

	       (:file "wasm-extern"
		:depends-on ("package"
			     "common"
			     "wasm-type"
			     "wasm-func"
			     "wasm-global"
			     "wasm-memory"
			     "wasm-table"))
	       
	       (:file "wasm-module"
		:depends-on ("package"
			     "common"
			     "wasm-type"
			     "wasm-trap"
			     "wasm-extern"))
	       
	       (:file "wasm-instance"
		:depends-on ("package"
			     "common"
			     "wasm-type"
			     "wasm-extern"
			     "wasm-module"))

	       (:file "wat"
		:depends-on ("package"
			     "common"
			     "runtime"))))


(asdf:defsystem "cl-wasm-runtime/test"
  :depends-on ("cl-wasm-runtime"
	       "fiveam")
  :pathname "test/"
  :components ((:file "package")
	       (:file "suite"
		:depends-on ("package"))
	       (:file "test-export-type"
		:depends-on ("package" "suite"))
	       (:file "test-engine"
		:depends-on ("package" "suite"))
	       (:file "test-func-type"
		:depends-on ("package" "suite"))
	       (:file "test-global"
		:depends-on ("package" "suite"))
	       (:file "test-global-type"
		:depends-on ("package" "suite"))
	       (:file "test-import-type"
		:depends-on ("package" "suite"))
	       (:file "test-instance"
		:depends-on ("package" "suite"))
	       (:file "test-limits"
		:depends-on ("package" "suite"))
	       (:file "test-memory"
		:depends-on ("package" "suite"))
	       (:file "test-memory-type"
		:depends-on ("package" "suite"))
	       (:file "test-module"
		:depends-on ("package" "suite"))))
