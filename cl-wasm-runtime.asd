(asdf:defsystem #:cl-wasm-runtime.prelude
  :class :package-inferred-system 
  :defsystem-depends-on (:asdf-package-system)
  :pathname "src"
  :depends-on (#:cffi
	       #:alexandria
	       #:cl-wasm-runtime.prelude/util
	       #:cl-wasm-runtime.prelude/ffi-type
	       #:cl-wasm-runtime.prelude/ffi-prelude))

(asdf:defsystem #:cl-wasm-runtime.wasmer
  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (#:cffi #:cffi/c2ffi #:cl-ppcre #:alexandria #:cl-wasm-runtime.prelude)
  :components
  ((:file "wasmer-ffi-prelude"
    :pathname "src/wasmer/wasmer-ffi-prelude")
   (:module "spec"
     :depends-on ("wasmer-ffi-prelude")
     :components
     ((:cffi/c2ffi-file "wasmer.h"
       :c2ffi-executable "/home/hk/Development/c2ffi/build/bin/c2ffi"
       :package #:cl-wasm-runtime.wasmer/libwasmer
       :foreign-library-name "cl-wasm-runtime.wasmer/libwasmer::libwasmer"
       :include-sources ("bits/types\\.h$"
			 "bits/stdint.*\\.h"
			 "stdint\\.h$")
       :include-definitions ("^size_t$"
			     "^byte_t$"
			     "^float32_t$"
			     "^float64_t$"
			     "^wasm_"
			     "^wasi"
			     "^wat2wasm$")
       :exclude-sources :all
       :ffi-name-transformer "cl-wasm-runtime.wasmer/prelude/ffi-prelude:ffi-name-transformer"
       :ffi-name-export-predicate "cl-wasm-runtime.wasmer/prelude/ffi-prelude:ffi-name-export-predicate"
       :ffi-type-transformer "cl-wasm-runtime.wasmer/prelude/ffi-prelude:ffi-type-transformer"
       :foreign-library-spec ((:unix (:or "/home/hk/.wasmer/lib/libwasmer.so"))))))
   (:file "wasmer-ffi"
    :pathname "src/wasmer/wasmer-ffi"
    :depends-on ("spec"))))

(asdf:defsystem #:cl-wasm-runtime.internal
  :class :package-inferred-system 
  :defsystem-depends-on (:asdf-package-system)
  :pathname "src" 
  :depends-on (;; 3rd party dependencies, listed here for bookkeeping
	       #:alexandria
	       #:trivial-garbage
	       #:trivial-backtrace
	       #:cffi
	       #:fast-io
	       #:babel
	       #:ieee-floats
	       #:bordeaux-threads
	       ;; Internal dependencies
	       #:cl-wasm-runtime.prelude
	       ;; System packages
	       #:cl-wasm-runtime.internal/wasm-ffi
	       #:cl-wasm-runtime.internal/conditions
	       #:cl-wasm-runtime.internal/runtime
	       #:cl-wasm-runtime.internal/object
	       #:cl-wasm-runtime.internal/config
	       #:cl-wasm-runtime.internal/engine
	       #:cl-wasm-runtime.internal/store
	       #:cl-wasm-runtime.internal/vector
	       #:cl-wasm-runtime.internal/byte-vec
	       #:cl-wasm-runtime.internal/limits
	       #:cl-wasm-runtime.internal/valtype
	       #:cl-wasm-runtime.internal/functype
	       #:cl-wasm-runtime.internal/globaltype
	       #:cl-wasm-runtime.internal/memorytype
	       #:cl-wasm-runtime.internal/tabletype
	       #:cl-wasm-runtime.internal/externtype
	       #:cl-wasm-runtime.internal/importtype
	       #:cl-wasm-runtime.internal/exporttype
	       #:cl-wasm-runtime.internal/val
	       #:cl-wasm-runtime.internal/frame
	       #:cl-wasm-runtime.internal/trap
	       #:cl-wasm-runtime.internal/foreign
	       #:cl-wasm-runtime.internal/module
	       #:cl-wasm-runtime.internal/func
	       #:cl-wasm-runtime.internal/global
	       #:cl-wasm-runtime.internal/table
	       #:cl-wasm-runtime.internal/memory
	       #:cl-wasm-runtime.internal/extern
	       #:cl-wasm-runtime.internal/ref
	       #:cl-wasm-runtime.internal/instance))

(asdf:defsystem #:cl-wasm-runtime
  :depends-on (#:cl-wasm-runtime.internal)
  :components ((:file "package" :pathname "src/package")))

(asdf:defsystem #:cl-wasm-runtime.test
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname "test/unit/"
  :depends-on (;; 3rd party dependencies
	       #:fiveam
	       #:ieee-floats
	       #:babel
	       #:trivial-garbage
	       ;; CL-WASM-RUNTIME
	       #:cl-wasm-runtime.prelude
	       #:cl-wasm-runtime
	       ;; System packages
       	       #:cl-wasm-runtime.test/suite
	       #:cl-wasm-runtime.test/fixture/gc
	       #:cl-wasm-runtime.test/fixture/engine
	       #:cl-wasm-runtime.test/fixture/store
	       #:cl-wasm-runtime.test/fixture/module
	       #:cl-wasm-runtime.test/fixture/instance
	       #:cl-wasm-runtime.test/engine
	       #:cl-wasm-runtime.test/store
	       #:cl-wasm-runtime.test/byte-vec
	       #:cl-wasm-runtime.test/limits
	       #:cl-wasm-runtime.test/valtype
	       #:cl-wasm-runtime.test/functype
	       #:cl-wasm-runtime.test/memorytype
	       #:cl-wasm-runtime.test/tabletype
	       #:cl-wasm-runtime.test/externtype
	       #:cl-wasm-runtime.test/importtype
	       #:cl-wasm-runtime.test/exporttype
	       #:cl-wasm-runtime.test/val
	       #:cl-wasm-runtime.test/trap
	       #:cl-wasm-runtime.test/module
	       #:cl-wasm-runtime.test/func
	       #:cl-wasm-runtime.test/global
	       #:cl-wasm-runtime.test/memory
	       #:cl-wasm-runtime.test/instance))

