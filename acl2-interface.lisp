
(in-package #:acl2-mcp-bridge)

;; ACL2 Interface - Direct evaluation in running ACL2
;;
;; This code runs INSIDE ACL2, so we have direct access to ACL2 symbols
;; and *the-live-state*. Similar to centaur/bridge.

(defun initialize-acl2-interface (&optional acl2-path)
  "No-op - we're already running inside ACL2."
  (declare (ignore acl2-path))
  t)

(defun acl2-eval (form-string &key session-id main-thread-p)
  "Evaluate a form in ACL2.

   FORM-STRING is a string containing an s-expression.
   If MAIN-THREAD-P is true, use bridge::in-main-thread for memoization safety.

   Returns (values result error-p error-message)"
  (declare (ignore session-id))
  (handler-case
      (let* ((form (read-from-string form-string))
             (wrapped-form `(let ((acl2::state acl2::*the-live-state*))
                              (declare (ignorable acl2::state))
                              ,form)))
        (if main-thread-p
            (values (eval `(bridge::in-main-thread ,wrapped-form)) nil nil)
            (values (eval wrapped-form) nil nil)))
    (error (e)
      (values nil t (format nil "~A" e)))))

(defun acl2-query (form-string &key session-id)
  "Query ACL2 for a computation result.
   Returns (values result error-p error-message)"
  (acl2-eval form-string :session-id session-id))

(defun acl2-event (form-string &key session-id)
  "Submit an ACL2 event (defun, defthm, etc).
   Returns (values success-p error-message output-string)"
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((form (read-from-string form-string))
               (*standard-output* output)
               (result (eval `(let ((acl2::state acl2::*the-live-state*))
                                (declare (ignorable acl2::state))
                                ,form))))
          (declare (ignore result))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-check-theorem (conjecture &key session-id hints)
  "Check if a conjecture is provable using thm."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((form (read-from-string conjecture))
               (thm-form (if hints
                             `(acl2::thm ,form :hints ,(read-from-string hints))
                             `(acl2::thm ,form)))
               (*standard-output* output))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   ,thm-form))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-admit (event &key session-id)
  "Admit an event to the ACL2 world."
  (acl2-event event :session-id session-id))

(defun acl2-verify-guards (function &key session-id)
  "Verify guards for a function."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((fn-sym (read-from-string function))
               (*standard-output* output))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   (acl2::verify-guards ,fn-sym)))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

;; Session management is simplified - we're in a single ACL2 process
;; Sessions are just for tracking MCP client state, not separate ACL2 instances

(defstruct acl2-session
  (id (string-downcase (princ-to-string (uuid:make-v4-uuid))) :type string)
  (created-at (local-time:now))
  (last-activity (local-time:now)))

(defparameter *acl2-sessions* (make-hash-table :test 'equal))

(defun start-acl2-session (&key session-id)
  "Register a new session (for MCP client tracking)."
  (let* ((id (or session-id (string-downcase (princ-to-string (uuid:make-v4-uuid)))))
         (session (make-acl2-session :id id)))
    (setf (gethash id *acl2-sessions*) session)
    id))

(defun get-acl2-session (id)
  (gethash id *acl2-sessions*))

(defun stop-acl2-session (session-id)
  "Remove a session."
  (remhash session-id *acl2-sessions*))

(defun list-acl2-sessions ()
  "List all registered sessions."
  (loop for session being the hash-values of *acl2-sessions*
        collect (list :id (acl2-session-id session)
                      :created (acl2-session-created-at session)
                      :last-activity (acl2-session-last-activity session))))
