(defpackage #:cl-wasm-runtime.internal/conditions
  (:use #:cl)
  (:nicknames #:wasm-rt/error)
  (:export #:wasm-error
	   #:wasm-simple-error
	   #:wasm-runtime-error
	   #:wasm-runtime-error-message
	   #:required-initarg-error
	   #:required-initarg-error-initarg
	   #:wasm-translate-error
	   #:wasm-translate-error-kind
	   #:wasm-translate-float-overflow
	   #:wasm-translate-type-error
	   #:wasm-trap-error
	   #:wasm-trap-error-message
	   #:wasm-trap-error-origin
	   #:wasm-trap-error-trace
	   #:import-error
	   #:import-error-module
	   #:import-error-name
	   #:import-error-externkind
	   #:import-missing-error
	   #:import-type-error))

(in-package #:cl-wasm-runtime.internal/conditions)

(define-condition wasm-error (error) ())

(define-condition wasm-simple-error (wasm-error simple-error) ())

(defun wasm-simple-error (format-control &rest format-arguments)
  (error 'wasm-simple-error
	 :format-control format-control
	 :format-arguments format-arguments))

(define-condition wasm-runtime-error (wasm-error)
  ((%message :initarg :message :reader wasm-runtime-error-message))
  (:report (lambda (c s)
	     (format s
		     "WASM runtime error: ~a" (wasm-runtime-error-message c)))))

(define-condition required-initarg-error (wasm-error)
  ((%initarg :initarg :initarg :reader required-initarg-error-initarg))
  (:report (lambda (c s)
	     (format s
		     "INITARG ~A is required." (required-initarg-error-initarg c)))))

(define-condition wasm-translate-error (wasm-error)
  ((kind :initarg :kind :initform nil :reader wasm-translate-error-kind)))

(define-condition wasm-translate-float-overflow (wasm-translate-error)
  ((datum :initarg :datum :initform nil :reader wasm-translate-float-overflow-datum))
  (:report (lambda (c s)
	     (format s
		     "Floating point overflow when translating ~A to WASM-VALKIND ~A."
		     (wasm-translate-float-overflow-datum c)
		     (wasm-translate-error-kind c)))))

(define-condition wasm-translate-type-error (wasm-translate-error type-error)
  ()
  (:report (lambda (c s)
	     (format s
		     "The value ~A of type ~A cannot be translated to WASM-VALKIND ~A. Expected one of ~A."
		     (type-error-datum c)
		     (type-of (type-error-datum c))
		     (wasm-translate-error-kind c)
		     (type-error-expected-type c)))))

(define-condition wasm-trap-error (wasm-error)
  ((%message :initarg :message :reader wasm-trap-error-message)
   (%origin :initarg :origin :reader wasm-trap-error-origin)
   (%trap-trace :initarg :trace :reader wasm-trap-error-trace))
  (:report (lambda (c s)
	     (print (wasm-trap-error-message c) s))))

;; TODO: Should this be a TYPE-ERROR?
(define-condition import-error (wasm-error)
  ((%namespace :initarg :module :reader import-error-module)
   (%name :initarg :name :reader import-error-name)
   (%externkind :initarg :externkind :reader import-error-externkind)))

(define-condition import-missing-error (import-error)
  ()
  (:report (lambda (c s)
	     (format s
		     "Missing import ~A in module ~A of WASM-EXTERNKIND ~A."
		     (import-error-name c)
		     (import-error-module c)
		     (import-error-externkind c)))))

(define-condition import-type-error (import-error)
  ()
  (:report (lambda (c s)
	     (format s
		     "The import ~A in module ~A is not expected WASM-EXTERN-KIND ~A"
		     (import-error-name c)
		     (import-error-module c)
		     (import-error-externkind c)))))
