(in-package #:acl2-mcp-bridge)

;; Threading utilities for multi-client support

(defmacro with-lock (lock &body body)
  "Acquire a lock for the duration of body."
  `(bt:with-lock-held (,lock) ,@body))

(defun run-in-main-thread (form &key block-p)
  "Execute a form in the main ACL2 thread.

   This is necessary for ACL2 operations that aren't thread-safe
   (memoization, hash-consing, stobj updates).

   If block-p is false, returns immediately without waiting."
  (declare (ignore form block-p))
  ;; Stub: real implementation needs thread communication channel
  nil)

(defun try-in-main-thread (form)
  "Like run-in-main-thread but fails immediately if main thread is busy."
  (declare (ignore form))
  nil)

(defun current-thread-name ()
  "Get human-readable thread name."
  (format nil "~A" (bt:thread-name (bt:current-thread))))
