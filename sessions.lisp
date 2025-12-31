
(in-package #:acl2-mcp-40ants)

(defstruct cl-session
  id
  package)

(defparameter *cl-sessions* (make-hash-table :test 'equal))

(defun ensure-cl-session (id)
  (or (gethash id *cl-sessions*)
      (setf (gethash id *cl-sessions*)
            (make-cl-session :id id :package :cl-user))))

(defun cl-eval (code &key (session-id "default"))
  (let* ((session (ensure-cl-session session-id))
         (pkg (cl-session-package session)))
    (let ((*package* (find-package pkg)))
      (multiple-value-list (eval (read-from-string code))))))
