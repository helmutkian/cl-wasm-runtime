(defpackage #:cl-wasm-runtime.prelude/ffi-prelude
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:cffi)
  (:import-from #:alexandria)
  (:import-from #:cl-wasm-runtime.prelude/ffi-type
		#:*wasm-ptr-types*)
  (:export #:ffi-name-transformer
	   #:*export-symbol-names*
	   #:*exclude-symbol-names*
	   #:ffi-name-export-predicate
	   #:ffi-type-transformer))

(in-package #:cl-wasm-runtime.prelude/ffi-prelude)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (check-type name string)
  (if (and (eq kind :member)
	   (alexandria:starts-with-subseq "WASM_" name))
      (intern (symbol-name (cffi:translate-underscore-separated-name name)) "KEYWORD")
      (ppcre:register-groups-bind (prefix suffix) ("(^_*)(.+)" name)
	(format nil
		"~A~A"
		(with-output-to-string (str)
		  (dotimes (i (length prefix))
		    (princ #\% str)))
		(cffi:translate-underscore-separated-name suffix)))))

(defparameter *export-symbol-names*
  (mapcar #'symbol-name
	  '(invalid-version
	    latest
	    snapshot0
	    snapshot1
	    wat2wasm
	    size
	    data
	    min
	    max
	    int32-t
	    int64-t
	    float32-t
	    float64-t
	    i32
	    i64
	    f32
	    f64
	    ref
	    kind
	    of
	    size-t
	    byte-t
	    uint32-max
	    uint64-max
	    uintptr-max)))

(defparameter *exclude-symbol-names*
  (mapcar #'symbol-name
	  '(wasm-val-t)))

(defun ffi-name-export-predicate (symbol &key &allow-other-keys)
  (let ((sym-name (symbol-name symbol)))
    (unless (member sym-name *exclude-symbol-names* :test #'string=) 
      (or (member sym-name *export-symbol-names* :test #'string=)
	  (alexandria:starts-with-subseq "WASI-" sym-name)
	  (alexandria:starts-with-subseq "WASM-" sym-name)))))

(defun match-wasm-ptr-type (type)
  (and (symbolp type) 
       (second (find (symbol-name type)
		     *wasm-ptr-types*
		     :test (lambda (type-name wasm-obj-t)
			     (string= type-name (first wasm-obj-t)))))))

(defun context-tag (context)
  (first context))

(defun pointer-type? (type)
  (and (consp type)
       (eq (first type) :pointer)))

(defun unwrap-pointer-type (type)
  (loop for content = type then (second content)
	until (or (null content)
		  (not (pointer-type? content)))
	sum 1 into count
	finally (return (values content count))))

(defun wrap-pointer-type (pointed-type nested-count)
  (loop with tail = pointed-type
	for i from 1 upto nested-count
	do (setf tail (list :pointer tail))
	finally (return tail)))

(defun ffi-type-transformer (type context &rest args &key &allow-other-keys)
  (let ((type (apply 'cffi/c2ffi:default-ffi-type-transformer type context args)))
    (cond
      ((and (find (context-tag context) '(:struct :function))
	    (pointer-type? type))
       (multiple-value-bind (pointed-type nested-count) (unwrap-pointer-type type)
	 (let ((matching-type (and pointed-type
				   (match-wasm-ptr-type pointed-type))))
	   (if matching-type
	       (wrap-pointer-type matching-type (1- nested-count))
	       type))
	 ))
      (t type))))
