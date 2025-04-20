(defpackage #:cl-wasm-runtime.internal/runtime
  (:nicknames #:wasm-rt/rt)
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:bordeaux-threads)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:export #:*runtime-error-message*
	   #:with-runtime-error
	   #:with-signal-runtime-error))

(in-package #:cl-wasm-runtime.internal/runtime)

(defparameter *runtime-lock* (bt:make-lock)
  "Runtime-level lock")

(defvar *runtime-error-message* nil
  "Runtime-level error message")

(defmacro with-runtime-error (form &body body)
  "Excutes a form with a lock on the wasm runtime, checking for any runtime-level errors that may have been raised. If an error has been raised, then BODY is executed where the special variable *RUNTIME-ERROR-MESSAGE* can be accessed.

Syntax:
(WITH-RUNTIME-ERROR form body*)

form - Expression that could raise a runtime-level error.
body - Forms in which *RUNTIME-ERROR-MESSAGE* can be accessed after FORM is executed."
  (alexandria:with-gensyms (result error-message-size buffer)
    `(bt:with-lock-held (*runtime-lock*)
       (let ((,result ,form))
	 #+wasmer
	 (let ((,error-message-size (cffi:foreign-funcall "wasmer_last_error_length" :int)))
	   (if (zerop ,error-message-size)
	       ,result
	       (cffi:with-foreign-object (,buffer :char ,error-message-size)
		 (if (< (cffi:foreign-funcall "wasmer_last_error_message"
					      :pointer ,buffer
					      :int ,error-message-size
					      :int)
			0)
		     (error 'wasm-rt/error:wasm-runtime-error
			    :message "Failed to read runtime error message.")
		     
		     (let ((*runtime-error-message* (cffi:foreign-string-to-lisp ,buffer)))
		       ,@body)))))
	 #-wasmer
	 (progn ,result)))))

(defmacro with-signal-runtime-error (&body body)
  "Executes its body, and if a runtime-level error is raised, signals a WASM-RUNTIME-ERROR conditions. See WITH-RUNTIME-ERROR for details about runtime locking.

Syntax:
(WITH-SIGNAL-RUNTIME-ERROR body*)

body - Forms that could raise a runtime-level error."
  `(with-runtime-error (progn ,@body)
     (when *runtime-error-message*
       (error 'wasm-rt/error:wasm-runtime-error :message *runtime-error-message*))))
