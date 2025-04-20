(defpackage #:cl-wasm-runtime.internal/limits
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector)
  (:nicknames #:wasm-rt/limits)
  (:import-from #:cffi)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:import-from #:cl-wasm-runtime.prelude/util
		#:safe-bind)
  (:export #:wasm-pages
	   #:+wasm-page-size+
	   #:+max-wasm-pages+
	   #:+min-wasm-pages+
	   #:pages-num-bytes
	   #:wasm-limits
	   #:init-wasm-limits
	   #:make-wasm-limits
	   #:limits-min
	   #:limits-max
	   #:+limits-max-unbounded+
	   #:limits-unbounded?))

(in-package #:cl-wasm-runtime.internal/limits)

(deftype wasm-pages ()
  `(unsigned-byte 32))

(defconstant +wasm-page-size+ #x10000
  "Wasm page size in bytes--64 kilobytes")

(defconstant +max-wasm-pages+ #x10000
  "Maximum number of wasm pages")

(defconstant +min-wasm-pages+ #x100
  "Minimum number of wasm pages")

(defun pages-num-bytes (num-pages)
  "Wasm pages in bytes"
  (check-type num-pages wasm-pages)
  (the (unsigned-byte 32)
       (* num-pages +wasm-page-size+)))

(defconstant +limits-max-unbounded+ wasm-ffi:+wasm-limits-max-default+
  "Number of wasm pages that represents unbounder upper limit")

(define-wasm-object wasm-limits ()
  ((delete-function :initform (lambda (pointer)
				;; noop
				(declare (ignorable pointer)))))
  (:documentation "Minimum and maximum number of pages for WASM-MEMORY and WASM-TABLE objects."))

(defun init-wasm-limits (pointer min &optional (max +limits-max-unbounded+))
  (check-type min (unsigned-byte 32))
  (check-type max (unsigned-byte 32))
  (prog1 pointer
    (setf (cffi:foreign-slot-value pointer '(:struct wasm-ffi:wasm-limits-t) 'wasm-ffi:min)
	  min
	  (cffi:foreign-slot-value pointer '(:struct wasm-ffi:wasm-limits-t) 'wasm-ffi:max)
	  max)))

(defun make-wasm-limits (min &key (max +limits-max-unbounded+) owner)
  "Creates new WASM-LIMITS instance

Syntax:

(MAKE-WASM-LIMITS min &key max owner) => limits

min - minimum number of initial pages
max - maximum number of pages, defaults to +LIMITS-MAX-UNBOUNDED+
owner - WASM-OBJECT that owns this instance
limits - new WASM-LIMITS instance"
  (safe-bind ((pointer (cffi:foreign-alloc '(:struct wasm-ffi:wasm-limits-t))
		       #'cffi:foreign-free))
    (make-instance 'wasm-limits
		   :pointer (init-wasm-limits pointer min max)
		   :dynamic? t
		   :owner owner)))

(defun limits-min (limits)
  "Minimum number of pages.

Syntax:

(LIMITS-MIN limits) => min

limits - WASM-LIMITS instance
min - minimum number of pages"
  (declare (values (unsigned-byte 32)))
  (cffi:foreign-slot-value (etypecase limits
			     (cffi:foreign-pointer limits)
			     (wasm-limits (pointer limits)))
			   '(:struct wasm-ffi:wasm-limits-t)
			   'wasm-ffi:min))

(defun limits-max (limits)
  "Maximum number of pages.

Syntax:

(LIMITS-MAX limits) => max

limits - WASM-LIMITS instance
max - maximum number of pages"
  (declare (values (unsigned-byte 32)))
  (cffi:foreign-slot-value (etypecase limits
			     (cffi:foreign-pointer limits)
			     (wasm-limits (pointer limits)))
			   '(:struct wasm-ffi:wasm-limits-t)
			   'wasm-ffi:max))

(defun limits-unbounded? (limits)
  "Does WASM-LIMITS instance have a maximum lower than system-defined maximum?

Syntax:

(LIMITS-UNBOUNDED? limits) => unbounded?

limits - WASM-LIMITS instance
unbounded - BOOLEAN"
  (= +limits-max-unbounded+ (limits-max limits)))

(defmethod wasm-object-eq? ((limits-a wasm-limits) (limits-b wasm-limits))
  (and (= (limits-min limits-a) (limits-min limits-b))
       (= (limits-max limits-a) (limits-max limits-b))))
