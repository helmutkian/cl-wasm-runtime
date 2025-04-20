(defpackage #:cl-wasm-runtime.internal/functype
  (:use #:cl
	#:cl-wasm-runtime.internal/object
	#:cl-wasm-runtime.internal/vector
	#:cl-wasm-runtime.internal/valtype)
  (:nicknames #:wasm-rt/functype)
  (:import-from #:cl-wasm-runtime.internal/wasm-ffi)
  (:export #:wasm-functype
	   #:make-wasm-functype
	   #:functype-params
	   #:functype-results))

(in-package #:cl-wasm-runtime.internal/functype)

(define-wasm-object wasm-functype)

(defun make-wasm-functype (params results)
  "PARAMS & RESULTS may be SEQUENCEs of WASM-VALTYPE objects, WASM-VALTYPE-T pointers, or KEYWORDs representing WASM-VALKIND enum. The KEYWORD interface is preferred for end-users."
  (check-type params sequence)
  (check-type results sequence)
  ;; PARAMs and RESULTs will be owned by WASM-FUNCTYPE. Make copies to avoid unexpectly
  ;; releasing arguments 
  (let ((params-vec
	  (make-wasm-valtype-vec :initial-contents (map 'list #'copy-wasm-valtype params)))
	(results-vec
	  (make-wasm-valtype-vec :initial-contents (map 'list #'copy-wasm-valtype results))))
    (with-new-wasm-object wasm-functype
      (wasm-ffi:wasm-functype-new (own params-vec) (own results-vec)))))

;; TODO: Return list of KEYWORDs?
(defun functype-params (functype)
  (check-type functype wasm-functype)
  (wasm-vec-to-list (make-instance 'wasm-valtype-vec
				   :pointer (wasm-ffi:wasm-functype-params functype)
				   :owner (owner functype))))

;; TODO: Return list of KEYWORDs?
(defun functype-results (functype)
    (check-type functype wasm-functype)
    (wasm-vec-to-list (make-instance 'wasm-valtype-vec
				     :pointer (wasm-ffi:wasm-functype-results functype)
				     :owner (owner functype))))

(defmethod wasm-object-eq? ((functype-a wasm-functype) (functype-b wasm-functype))
  (let ((params-a (functype-params functype-a))
	(params-b (functype-params functype-b))
	(results-a (functype-results functype-a))
	(results-b (functype-results functype-b)))
    (and (= (length params-a) (length params-b))
	 (= (length results-a) (length results-b))
	 (every #'wasm-object-eq? params-a params-b)
	 (every #'wasm-object-eq? results-a results-b))))
