
(in-package #:acl2-mcp-40ants)

;; Skeleton ACL2 interface; to be wired to real ACL2 process or in-image ACL2.

(defstruct acl2-session
  id
  process
  input-stream
  output-stream
  error-stream
  state)

(defparameter *acl2-sessions* (make-hash-table :test 'equal))

(defun ensure-acl2-session (id)
  (or (gethash id *acl2-sessions*)
      (setf (gethash id *acl2-sessions*)
            (make-acl2-session :id id))))

(defun acl2-eval (form &key (session-id "default"))
  (declare (ignore form))
  (let ((session (ensure-acl2-session session-id)))
    (declare (ignore session))
    "ACL2-EVAL not yet implemented"))
