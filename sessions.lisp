
(in-package #:acl2-mcp-bridge)

;; Common Lisp Session Structure
(defstruct cl-session
  (id (string-downcase (princ-to-string (uuid:make-v4-uuid))) :type string)
  (package (find-package :cl-user))
  (created-at (local-time:now))
  (last-activity (local-time:now))
  (eval-history '())
  (lock (bt:make-lock "cl-session")))

(defparameter *cl-sessions* (make-hash-table :test 'equal))

(defun create-cl-session (&key id package)
  "Create and register a new Common Lisp session.

If ID is supplied and already exists, signal an error. PACKAGE may be a
package designator; defaults to :cl-user. Returns the session instance." 
  (let* ((id (or id (string-downcase (princ-to-string (uuid:make-v4-uuid)))))
         (existing (gethash id *cl-sessions*)))
    (when existing
      (error "CL session ~A already exists" id))
    (let ((session (make-cl-session :id id
                                    :package (or (and package (find-package package))
                                                 (find-package :cl-user))
                                    :created-at (local-time:now)
                                    :last-activity (local-time:now))))
      (setf (gethash (cl-session-id session) *cl-sessions*) session)
      session)))

(defun get-cl-session (id)
  "Retrieve a CL session by ID."
  (gethash id *cl-sessions*))

(defun ensure-cl-session (id)
  "Get or create a CL session."
  (or (get-cl-session id)
      (create-cl-session)))

(defun start-cl-session (&key session-id package)
  "Start a new CL session and return its ID.

If SESSION-ID is provided and exists, an error is signaled to avoid silently
reusing sessions. PACKAGE selects the initial package (default CL-USER)."
  (cl-session-id (create-cl-session :id session-id :package package)))

(defun stop-cl-session (session-id)
  "Stop and remove a CL session. Returns T if removed, NIL if not found."
  (when (gethash session-id *cl-sessions*)
    (remhash session-id *cl-sessions*)
    t))

(defun cl-eval (code &key (session-id "default"))
  "Evaluate Common Lisp code in a persistent session.

   Returns (values result-list error-p error-message)"
  (let* ((session (ensure-cl-session session-id))
         (pkg (cl-session-package session)))
    (handler-case
      (let ((*package* pkg))
        (let ((form (read-from-string code)))
          (multiple-value-list (eval form))))
      (error (e)
        (values nil t (format nil "~A" e))))))

(defun cl-load-file (path &key (session-id "default"))
  "Load a Common Lisp file in a session."
  (let ((session (ensure-cl-session session-id)))
    (handler-case
      (let ((*package* (cl-session-package session)))
        (load (truename path)))
      (error (e)
        (values nil t (format nil "~A" e))))))

(defun cl-define-function (name lambda-list body &key (session-id "default"))
  "Define a function in a session."
  (let ((session (ensure-cl-session session-id)))
    (handler-case
      (let ((*package* (cl-session-package session)))
        (eval `(defun ,name ,lambda-list ,@body)))
      (error (e)
        (values nil t (format nil "~A" e))))))

(defun list-cl-sessions ()
  "List all active Common Lisp sessions."
  (loop for session being the hash-values of *cl-sessions*
        collect (list :id (cl-session-id session)
                      :package (package-name (cl-session-package session))
            :created (cl-session-created-at session)
            :last-activity (cl-session-last-activity session))))
