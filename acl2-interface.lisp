
(in-package #:acl2-mcp-bridge)

;; ACL2 Session Structure
(defstruct acl2-session
  (id (string-downcase (princ-to-string (uuid:make-v4-uuid))) :type string)
  (acl2-process nil)
  (input-stream nil)
  (output-stream nil)
  (error-stream nil)
  (state nil)
  (created-at (local-time:now))
  (last-activity (local-time:now))
  (event-history '())
  (output-capture (make-string-output-stream))
  (lock (bt:make-lock "acl2-session")))

(defparameter *acl2-sessions* (make-hash-table :test 'equal))
(defparameter *session-counter* 0)

;; Path to ACL2 executable or wrapper; configure via initialize-acl2-interface.
(defparameter *acl2-executable* "acl2")

(defun initialize-acl2-interface (&optional acl2-path)
  "Configure the ACL2 interface executable path.

If ACL2-PATH is nil, fall back to $ACL2_PATH or the literal "acl2".
Returns the chosen path so callers can log or validate it."
  (setf *acl2-executable* (or acl2-path (uiop:getenv "ACL2_PATH") "acl2"))
  *acl2-executable*)

(defun create-acl2-session ()
  "Create a new ACL2 session."
  (let ((session (make-acl2-session)))
    (setf (gethash (acl2-session-id session) *acl2-sessions*) session)
    session))

(defun get-acl2-session (id)
  "Retrieve an ACL2 session by ID."
  (gethash id *acl2-sessions*))

(defun ensure-acl2-session (id)
  "Get or create an ACL2 session."
  (or (get-acl2-session id)
      (create-acl2-session)))

(defun acl2-eval (form &key (session-id nil) main-thread-p)
  "Evaluate a form in ACL2.

   If main-thread-p is true, the evaluation is serialized through the main 
   thread (required for memoization safety).

   Returns (values result error-p output-string)"
  (declare (ignore form session-id main-thread-p))
  (log:info "ACL2-EVAL stub: implement actual ACL2 integration")
  (values nil nil ""))

(defun acl2-event (form &key (session-id nil))
  "Submit an ACL2 event (defun, defthm, etc).

   Returns (values success-p error-message output-string)"
  (declare (ignore form session-id))
  (log:info "ACL2-EVENT stub: implement actual ACL2 integration")
  (values t nil ""))

(defun acl2-query (form &key (session-id nil))
  "Query ACL2 for a computation result (non-event).

   Returns (values result error-p output-string)"
  (declare (ignore form session-id))
  (log:info "ACL2-QUERY stub: implement actual ACL2 integration")
  (values nil nil ""))

(defun acl2-check-theorem (conjecture &key (session-id nil) hints)
  "Check if a conjecture is provable."
  (declare (ignore conjecture session-id hints))
  (values nil "Not implemented" ""))

(defun acl2-admit (event &key (session-id nil))
  "Admit an event to the ACL2 world."
  (declare (ignore event session-id))
  (values nil "Not implemented" ""))

(defun acl2-verify-guards (function &key (session-id nil))
  "Verify guards for a function."
  (declare (ignore function session-id))
  (values nil "Not implemented" ""))

(defun list-acl2-sessions ()
  "List all active ACL2 sessions."
  (loop for session being the hash-values of *acl2-sessions*
        collect (list :id (acl2-session-id session)
                      :created (acl2-session-created-at session)
                      :last-activity (acl2-session-last-activity session))))
