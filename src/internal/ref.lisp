(defpackage #:cl-wasm-runtime.internal/ref
  (:nicknames #:wasm-rt/ref)
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/module
	#:cl-wasm-runtime.internal/extern
	#:cl-wasm-runtime.internal/global
	#:cl-wasm-runtime.internal/func
	#:cl-wasm-runtime.internal/memory
	#:cl-wasm-runtime.internal/table
	#:cl-wasm-runtime.internal/trap
	#:cl-wasm-runtime.internal/instance
	#:cl-wasm-runtime.internal/foreign)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-ref
	   #:ref-able
	   #:to-ref
	   #:from-ref
	   #:same-ref?
	   #:table-ref))

(in-package #:cl-wasm-runtime.internal/ref)

(define-wasm-object wasm-ref)

(deftype ref-able ()
  `(or wasm-ref wasm-extern wasm-func wasm-global wasm-memory wasm-table wasm-trap
       wasm-instance wasm-foreign))

(defun %to-ref (object)
  (funcall (etypecase object
	     (wasm-ref #'identity)
	     (wasm-module #'wasm-ffi:wasm-module-as-ref-const)
	     (wasm-extern #'wasm-ffi:wasm-extern-as-ref-const)
	     (wasm-global #'wasm-ffi:wasm-global-as-ref-const)
	     (wasm-func #'wasm-ffi:wasm-func-as-ref-const)
	     (wasm-memory #'wasm-ffi:wasm-memory-as-ref-const)
	     (wasm-table #'wasm-ffi:wasm-table-as-ref-const)
	     (wasm-trap #'wasm-ffi:wasm-trap-as-ref-const)
	     (wasm-instance #'wasm-ffi:wasm-instance-as-ref-const)
	     (wasm-foreign #'wasm-ffi:wasm-foreign-as-ref-const))
	   object))

(defun to-ref (object)
  (with-new-wasm-object wasm-ref
    (%to-ref (owner* object))))

(defun %from-ref (type ref)
  (ecase type
    (wasm-ref ref)
    (wasm-module (wasm-ffi:wasm-ref-as-module-const ref))
    (wasm-extern (wasm-ffi:wasm-ref-as-extern-const ref))
    (wasm-global (wasm-ffi:wasm-ref-as-global-const ref))
    (wasm-func (wasm-ffi:wasm-ref-as-func-const ref))
    (wasm-memory (wasm-ffi:wasm-ref-as-memory-const ref))
    (wasm-table (wasm-ffi:wasm-ref-as-table-const ref))
    (wasm-trap (wasm-ffi:wasm-ref-as-trap-const ref))
    (wasm-instance (wasm-ffi:wasm-ref-as-instance-const ref))
    (wasm-foreign (wasm-ffi:wasm-ref-as-foreign-const ref))))

(defun from-ref (ref &key (to 'wasm-ref))
  (check-type ref wasm-ref)
  (make-instance to :pointer (%from-ref to ref) :owner (owner ref)))

(defun same-ref? (object-1 object-2)
  "Returns NIL if arguments are not of the same type. Does NOT do any implicit conversions."
  (check-type object-1 ref-able)
  (check-type object-2 ref-able)
  (when (eql (type-of object-1) (type-of object-2))
    (funcall (etypecase object-1
	       (wasm-ref #'wasm-ffi:wasm-ref-same)
	       (wasm-extern #'wasm-ffi:wasm-extern-same)
	       (wasm-global #'wasm-ffi:wasm-global-same)
	       (wasm-func #'wasm-ffi:wasm-func-same)
	       (wasm-memory #'wasm-ffi:wasm-memory-same)
	       (wasm-table #'wasm-ffi:wasm-table-same)
	       (wasm-trap #'wasm-ffi:wasm-trap-same)
	       (wasm-instance #'wasm-ffi:wasm-instance-same)
	       (wasm-foreign #'wasm-ffi:wasm-foreign-as-ref-const))
	     object-1
	     object-2)))

;; These functions need to be defined here to prevent a circular dependency with
;; the CL-WASM-RUNTIME.INTERNAL/TABLE package
(defun table-ref (table index)
  (check-type table wasm-table)
  (check-type index (unsigned-byte 32))
  (with-new-wasm-object wasm-ref
    ;; TODO: Should TABLE be PARENT?
    (wasm-ffi:wasm-table-get table index)))

(defun (setf table-ref) (index object table)
  (check-type index (unsigned-byte 32))
  (let ((ref (typecase object
	       (cffi:foreign-pointer object)
	       (t (%to-ref object))))) 
    (prog1 ref
      (wasm-ffi:wasm-table-set table index ref))))
