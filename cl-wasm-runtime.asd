(asdf:defsystem "cl-wasm-runtime"
  :depends-on ("cffi"
	       "trivial-garbage"
	       "alexandria"
	       "split-sequence"
	       "fast-io"
	       "babel")
  :components ((:file "package")
	       (:file "wasm-object"
		      :depends-on ("package"))
	       (:file "wasm"
		      :depends-on ("package" "wasm-object"))))
