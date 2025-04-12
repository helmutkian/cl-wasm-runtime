(defpackage #:cl-wasm-runtime.test/wasm-ffi
  (:use #:cl
	#:cl-wasm-runtime.test/suite)
  (:import-from #:fiveam)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime/wasm-ffi)
  (:export #:cl-wasm-runtime-test/wasm-ffi))

(in-package #:cl-wasm-runtime.test/wasm-ffi)

(5am:def-suite cl-wasm-runtime-test/wasm-ffi :in cl-wasm-runtime-test)

(5am:in-suite cl-wasm-runtime-test/wasm-ffi)

(defun %collect (new-function count)
  (declare (type function new-function))
  (declare (type integer count))
  (loop repeat count collect (funcall new-function)))

(defun %delete-all (delete-function pointer-list)
  (declare (type function delete-function))
  (declare (type list pointer-list))
  (dolist (pointer pointer-list)
    (funcall delete-function pointer)))

(defun %wasm-vec-new (vec-pointer new-function elm-pointer-list elm-type)  
  (declare (type function new-function))
  (declare (type list elm-pointer-list))
  (let ((num-elms (length elm-pointer-list)))
    (cffi:with-foreign-object (elm-arr elm-type num-elms)
      (loop for elm-pointer in elm-pointer-list
	    for i from 0
	    do (setf (cffi:mem-aref elm-arr elm-type i) elm-pointer))
      (funcall new-function vec-pointer num-elms elm-arr))))

;;; Test all WASM-...-DELETE functions on initialized "stack" allocated objects of their
;;; corresponding type to test for "stack" allocation/dealloction safety

;;; WASM-ENGINE-DELETE =========================================================

(5am:test test-wasm-engine-delete ()
  (5am:finishes
    (wasm-ffi:wasm-engine-delete (wasm-ffi:wasm-engine-new))))

;;; WASM-STORE-DELETE ==========================================================

(5am:test test-wasm-store-delete ()
  (5am:finishes
    (let* ((engine (wasm-ffi:wasm-engine-new))
	   (store (wasm-ffi:wasm-store-new engine)))
      (wasm-ffi:wasm-store-delete store)
      (wasm-ffi:wasm-engine-delete engine))))

;;; WASM-BYTE-VEC-DELETE =======================================================

;; TODO: Test WASM-BYTE-VEC-DELETE

;;; WASM-VALTYPE-DELETE ========================================================

(defun %wasm-valtype-new (kind-key)
  (declare (type keyword kind-key))
  (wasm-ffi:wasm-valtype-new (cffi:foreign-enum-value 'wasm-ffi:wasm-valkind-enum kind-key)))

(5am:test test-wasm-valtype-delete ()
  (5am:finishes
    (let ((valtype (%wasm-valtype-new :wasm-i32)))
      (wasm-ffi:wasm-valtype-delete valtype))))


;;; WASM-VALTYPE-VEC-DELETE ====================================================

(declaim (type integer *num-valtypes*))
(defvar *num-valtypes* 1)

(defun %wasm-valtype-vec-new (valtype-vec valtypes)
  (%wasm-vec-new valtype-vec
		 #'wasm-ffi:wasm-valtype-vec-new
		 valtypes
		 'wasm-ffi:wasm-valtype-t))

(5am:test test-wasm-valtype-vec-delete-empty ()
  (5am:finishes
    (cffi:with-foreign-object (valtype-vec '(:struct wasm-ffi:wasm-valtype-vec-t))
      (wasm-ffi:wasm-valtype-vec-new-empty valtype-vec))))

(5am:test test-wasm-valtype-vec-delete ()
  (5am:finishes
    (cffi:with-foreign-object (valtype-vec '(:struct wasm-ffi:wasm-valtype-vec-t))
      (let ((valtypes (%collect (lambda () (%wasm-valtype-new :wasm-i32)) *num-valtypes*)))
	(%wasm-valtype-vec-new valtype-vec valtypes)
	(wasm-ffi:wasm-valtype-vec-delete valtype-vec)))))

;;; WASM-FUNCTYPE-DELETE =======================================================

(5am:test test-wasm-functype-delete-0-0 ()
  (5am:finishes
    (cffi:with-foreign-objects ((params '(:struct wasm-ffi:wasm-valtype-vec-t))
				(results '(:struct wasm-ffi:wasm-valtype-vec-t)))
      (wasm-ffi:wasm-valtype-vec-new-empty params)
      (wasm-ffi:wasm-valtype-vec-new-empty results)
      (let ((functype (wasm-ffi:wasm-functype-new params results)))
	(wasm-ffi:wasm-valtype-vec-delete params)
	(wasm-ffi:wasm-valtype-vec-delete results)
	(wasm-ffi:wasm-functype-delete functype)))))


(declaim (type integer *num-params*))
(defvar *num-params* 1)

(declaim (type integer *num-results*))
(defvar *num-results* 1)

(5am:test test-wasm-functype-delete-n-0 ()
  (5am:finishes
    (cffi:with-foreign-objects ((params '(:struct wasm-ffi:wasm-valtype-vec-t))
				(results '(:struct wasm-ffi:wasm-valtype-vec-t)))
      (let ((valtypes (%collect (lambda () (%wasm-valtype-new :wasm-i32)) *num-params*)))
	(%wasm-valtype-vec-new params valtypes)
	(wasm-ffi:wasm-valtype-vec-new-empty results)
	(let ((functype (wasm-ffi:wasm-functype-new params results)))
	  (wasm-ffi:wasm-valtype-vec-delete params)
	  (wasm-ffi:wasm-valtype-vec-delete results)
	  (wasm-ffi:wasm-functype-delete functype))))))


(5am:test test-wasm-functype-delete-0-n ()
  (5am:finishes
    (cffi:with-foreign-objects ((params '(:struct wasm-ffi:wasm-valtype-vec-t))
				(results '(:struct wasm-ffi:wasm-valtype-vec-t)))
      (let ((valtypes (%collect (lambda () (%wasm-valtype-new :wasm-i32)) *num-results*)))
	(wasm-ffi:wasm-valtype-vec-new-empty params)
	(%wasm-valtype-vec-new results valtypes)
	(let ((functype (wasm-ffi:wasm-functype-new params results)))
	  (wasm-ffi:wasm-valtype-vec-delete params)
	  (wasm-ffi:wasm-valtype-vec-delete results)
	  (wasm-ffi:wasm-functype-delete functype))))))


(5am:test test-wasm-functype-delete-n-n ()
  (5am:finishes
    (cffi:with-foreign-objects ((params '(:struct wasm-ffi:wasm-valtype-vec-t))
				(results '(:struct wasm-ffi:wasm-valtype-vec-t)))
      (let ((params-list (%collect (lambda () (%wasm-valtype-new :wasm-i32)) *num-params*))
	    (results-list (%collect (lambda () (%wasm-valtype-new :wasm-i32)) *num-results*)))
	(%wasm-valtype-vec-new params params-list)
	(%wasm-valtype-vec-new results results-list)
	(let ((functype (wasm-ffi:wasm-functype-new params results)))
	  (wasm-ffi:wasm-valtype-vec-delete params)
	  (wasm-ffi:wasm-valtype-vec-delete results)
	  (wasm-ffi:wasm-functype-delete functype))))))

;;; WASM-GLOBALTYPE-DELETE =====================================================

(defun %wasm-globaltype-new (valtype mutability-key)
  (wasm-ffi:wasm-globaltype-new
   valtype
   (cffi:foreign-enum-value 'wasm-ffi:wasm-mutability-enum mutability-key)))

(5am:test test-wasm-globalttype-delete ()
  (5am:finishes
    (let* ((valtype (%wasm-valtype-new :wasm-i32))
	   (globaltype (%wasm-globaltype-new valtype :wasm-var)))
      (wasm-ffi:wasm-globaltype-delete globaltype))))

;;; WASM-MEMORYTYPE-DELETE =====================================================

(defun %init-wasm-limits (limits min max)
  (setf (cffi:foreign-slot-value limits '(:struct wasm-ffi:wasm-limits-t) 'wasm-ffi:min)
	min
	(cffi:foreign-slot-value limits '(:struct wasm-ffi:wasm-limits-t) 'wasm-ffi:max)
	max))

(5am:test test-wasm-memorytype-delete ()
  (5am:finishes
    (cffi:with-foreign-object (limits '(:struct wasm-ffi:wasm-limits-t))
      (%init-wasm-limits limits 1 2)
      (wasm-ffi:wasm-memorytype-delete (wasm-ffi:wasm-memorytype-new limits)))))

;;; WASM-TABLETYPE-DELETE ======================================================


