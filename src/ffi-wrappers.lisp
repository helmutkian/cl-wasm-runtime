(in-package #:cl-wasm-runtime.libwasmer)

(defwrapper ("wasm_name_new_from_string" wasm-name-new-from-string) :void
  (out wasm-name-t)
  (s :string))
