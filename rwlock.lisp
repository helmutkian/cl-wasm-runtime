(in-package #:cl-wasm-runtime)

(defstruct rwlock
  (read-lock (bt:make-lock))
  (write-lock (bt:make-lock))
  (retry-lock (bt:make-lock))
  (resource (bt:make-lock))
  (read-count 0)
  (write-count 0))

(defun read-lock-begin (rwlock)
  (bt:acquire-lock (rwlock-retry-lock rwlock))
  (bt:acquire-lock (rwlock-read-lock rwlock))
  (when (= 1 (incf (rwlock-read-count rwlock)))
    (bt:acquire-lock (rwlock-resource rwlock)))
  (bt:release-lock (rwlock-read-lock rwlock))
  (bt:release-lock (rwlock-retry-lock rwlock)))

(defun read-lock-end (rwlock)
  (bt:acquire-lock (rwlock-read-lock rwlock))
  (when (= 0 (decf (rwlock-read-count rwlock)))
    (bt:release-lock (rwlock-resource rwlock)))
  (bt:release-lock (rwlock-read-lock rwlock)))

(defmacro with-read-lock (rwlock &body body)
  (alexandria:with-gensyms (rwlock%)
    `(let ((,rwlock% ,rwlock))
       (read-lock-begin ,rwlock%)
       (unwind-protect
            (progn ,@body)
         (read-lock-end ,rwlock%)))))

(defun write-lock-begin (rwlock)
  (bt:acquire-lock (rwlock-write-lock rwlock))
  (when (= 1 (incf (rwlock-write-count rwlock)))
    (bt:acquire-lock (rwlock-retry-lock rwlock)))
  (bt:release-lock (rwlock-write-lock rwlock))
  (bt:acquire-lock (rwlock-resource rwlock)))

(defun write-lock-end (rwlock)
  (bt:release-lock (rwlock-resource rwlock))
  (bt:acquire-lock (rwlock-write-lock rwlock))
  (when (= 0 (decf (rwlock-write-count rwlock)))
    (bt:release-lock (rwlock-retry-lock rwlock)))
  (bt:release-lock (rwlock-write-lock rwlock)))

(defmacro with-write-lock (rwlock &body body)
  (alexandria:with-gensyms (rwlock%)
    `(let ((,rwlock% ,rwlock))
       (write-lock-begin ,rwlock%)
       (unwind-protect
            (progn ,@body)
         (write-lock-end ,rwlock%)))))
