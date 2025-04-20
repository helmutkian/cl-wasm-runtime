(defpackage #:cl-wasm-runtime.internal/instance
  (:nicknames #:wasm-rt/instance)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/store
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/trap
	#:cl-wasm-runtime.internal/module
	#:cl-wasm-runtime.internal/extern
	#:cl-wasm-runtime.internal/externtype
	#:cl-wasm-runtime.internal/importtype
	#:cl-wasm-runtime.internal/exporttype)
  (:import-from #:cffi)
  (:import-from #:alexandria)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.internal/conditions)
  (:import-from #:cl-wasm-runtime.internal/runtime
		#:with-signal-runtime-error)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:prog1-let
		#:safe-bind
		#:with-memoized-slot
		#:with-auto-foreign-object)
  (:export #:imports-namespace
	   #:make-imports-namespace
	   #:imports-namespace-name
	   #:imports-namespace-extern
	   #:imports
	   #:make-imports
	   #:imports-extern
	   #:wasm-instance
	   #:make-wasm-instance
	   #:make-exports
	   #:exports-get
	   #:instance-exports))

(in-package #:cl-wasm-runtime.internal/instance)

(defun %required (initarg)
  (error 'wasm-rt/error:required-initarg-error :initarg initarg))

;; IMPORTS-NAMESPACE 

(defstruct (imports-namespace (:constructor %make-imports-namespace)
			      (:conc-name %imports-namespace-))
  (name (%required :name) :type string :read-only t)
  (extern-map (%required :extern-map) :type hash-table :read-only t))

(defun make-imports-namespace (name &optional name-extern-able-list)
  (check-type name string)
  (let ((extern-map (make-hash-table :test 'equal)))
    (loop for (extern-name extern . nil) on name-extern-able-list by #'cddr
	  do (check-type extern-name string)
	  do (check-type extern extern-able)
	  do (setf (gethash extern-name extern-map) (to-extern extern)))
    (%make-imports-namespace :name name :extern-map extern-map)))

(defun imports-namespace-name (namespace)
  (check-type namespace imports-namespace)
  (%imports-namespace-name namespace))

(defun imports-namespace-extern (namespace name)
  (check-type namespace imports-namespace)
  (check-type name string)
  (when namespace
    (gethash name (%imports-namespace-extern-map namespace))))

(defmacro imports-namespace (namespace-name names-extern-ables)
  `(make-imports-namespace ,namespace-name (list ,@names-extern-ables)))

;;; IMPORTS

(defstruct (imports (:constructor %make-imports)
		    (:conc-name %imports-))
  (namespace-map (%required :namespace-map) :type hash-table :read-only t))

(defun make-imports (&rest namespaces)
  (let ((namespace-map (make-hash-table :test 'equal)))
    (dolist (namespace namespaces (%make-imports :namespace-map namespace-map))
      (setf (gethash (%imports-namespace-name namespace) namespace-map)
	    (%imports-namespace-extern-map namespace)))))

(defun imports-extern (imports namespace name)
  (let ((namespace-map (gethash namespace (%imports-namespace-map imports))))
    (when namespace-map (gethash name namespace-map))))

(defun (setf imports-extern) (extern-able imports namespace name)
  (check-type extern-able extern-able)
  (let ((extern-map (gethash namespace (%imports-namespace-map imports)))
	(extern (to-extern extern-able)))
    (if extern-map 
	(setf (gethash name extern-map) extern)
      (let ((extern-map (make-hash-table :test 'equal)))
	(setf (gethash name extern-map) extern
	      (gethash namespace (%imports-namespace-map imports)) extern-map)))))

(defmacro imports (&rest namespaces)
  "Syntax:
(IMPORTS (module-name (import-name import)*)*)"
  `(make-imports ,@(loop for namespace in namespaces collect `(imports-namespace ,@namespace))))

(defun imports-to-extern-vec (imports module)
  (flet ((import-missing (namespace name importtype)
	   (error 'wasm-rt/error:import-missing-error
		  :module namespace
		  :name name
		  :externkind (wasm-externkind-to-key
			       (wasm-ffi:wasm-externtype-kind
				(wasm-ffi:wasm-importtype-type importtype)))))
	 (import-invalid (namespace name importtype)
	   (error 'wasm-rt/error:import-type-error
		  :module namespace
		  :name name
		  :externkind (wasm-externkind-to-key
			       (wasm-ffi:wasm-externtype-kind
				(wasm-ffi:wasm-importtype-type importtype))))))
    (loop with importtypes = (module-imports module)
	  with extern-vec = (make-wasm-extern-vec :size (length importtypes))
	  for importtype in importtypes
	  for i from 0
	  for namespace = (importtype-module importtype)
	  for name = (importtype-name importtype)
	  for extern = (imports-extern imports namespace name)
	  if (null extern)
	    do (import-missing namespace name importtype)
	  else
	    if (not (wasm-object-eq? (extern-type extern)
				     (importtype-type importtype)))
	      do (import-invalid namespace name importtype)
	  else
	    do (setf (wasm-vec-aref+ extern-vec i) (wasm-ffi:wasm-extern-copy extern))
	  end
	  finally (return extern-vec))))

;;; WASM-INSTANCE

(define-wasm-object wasm-instance ()
  ((%module :initarg :module
	    :reader instance-module)
   (%imports :initarg :imports
	     :documentation "Need to hold a reference to the WASM-EXTERN-VEC to prevent imported functions and references from being GC'd before WASM-INSTANCE.")
   (%exports)))

(defun make-wasm-instance (store module &optional imports)
  (check-type store (pointer wasm-store))
  (check-type module wasm-module)
  (check-type imports (or null imports))
  (cffi:with-foreign-object (trap '(:pointer wasm-ffi:wasm-trap-t))
    (let ((imports-extern-vec (imports-to-extern-vec (or imports (make-imports)) module))) 
      (with-new-wasm-object (wasm-instance :module module :imports imports-extern-vec)
	(prog1-let
	    (pointer
	     (with-signal-runtime-error
	       (wasm-ffi:wasm-instance-new (parent* store)
					   module
					   imports-extern-vec
					   (own trap))))
	  (when (and (null? pointer)
		     (not (null? (cffi:mem-aref trap 'wasm-ffi:wasm-trap-t))))
	    (wasm-trap-error (cffi:mem-aref trap 'wasm-ffi:wasm-trap-t))))))))

;;; EXPORTS

(defclass exports (wasm-extern-vec)
  ((%extern-map :initform (make-hash-table :test 'equal))))

(defun %make-exports (instance)
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-extern-vec-t))
		       #'cffi:foreign-free))
    (with-new-wasm-object	
	((exports wasm-extern-vec) :dynamic? t)
      (wasm-ffi:wasm-instance-exports (parent* instance) (out pointer)))))

(defun %exports-extern-map (exports)
  (slot-value exports '%extern-map))

(defun make-exports (instance module)
  (check-type instance wasm-instance)
  (check-type module wasm-module)
  (with-memoized-slot (%exports instance)
    (prog1-let (exports (%make-exports instance))
      (loop for exporttype in (module-exports module)
	    for i from 0
	    for extern = (wasm-vec-aref+ exports i)
	    do (setf (gethash (exporttype-name exporttype) (%exports-extern-map exports))
		     extern)))))

(defun exports-get (exports name)
  (check-type exports exports)
  (check-type name string)
  (let ((extern (gethash name (%exports-extern-map exports))))
    (and extern (from-extern extern))))

(defun instance-exports (instance)
  (check-type instance wasm-instance)
  (make-exports instance (instance-module instance)))
