
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Common Lisp Session Management
;;; 
;;; Simple in-process evaluation. The MCP server itself provides the isolation
;;; boundary - clients can spawn multiple server instances if they need 
;;; separate environments.
;;; ============================================================================

;; Common Lisp Session Structure
(defstruct cl-session
  (id (string-downcase (princ-to-string (uuid:make-v4-uuid))) :type string)
  (package-name "CL-USER" :type string)
  (created-at (local-time:now))
  (last-activity (local-time:now)))

(defparameter *cl-sessions* (make-hash-table :test 'equal))

(defun create-cl-session (&key id)
  "Create and register a new Common Lisp session.

If ID is supplied and already exists, signal an error. Returns the session instance."
  (let* ((id (or id (string-downcase (princ-to-string (uuid:make-v4-uuid)))))
         (existing (gethash id *cl-sessions*)))
    (when existing
      (error "CL session ~A already exists" id))
    (let ((session (make-cl-session :id id
                                    :created-at (local-time:now)
                                    :last-activity (local-time:now))))
      (setf (gethash id *cl-sessions*) session)
      session)))

(defun get-cl-session (id)
  "Retrieve a CL session by ID."
  (gethash id *cl-sessions*))

(defun ensure-cl-session (id)
  "Get or create a CL session."
  (or (get-cl-session id)
      (create-cl-session :id id)))

(defun start-cl-session (&key session-id)
  "Start a new CL session and return its ID.

If SESSION-ID is provided and exists, an error is signaled."
  (cl-session-id (create-cl-session :id session-id)))

(defun stop-cl-session (session-id)
  "Stop and remove a CL session. Returns T if removed, NIL if not found."
  (remhash session-id *cl-sessions*))

(defun cl-eval (code &key (session-id "default"))
  "Evaluate Common Lisp code in a session.

Returns (values result-list error-p error-message)."
  (let ((session (ensure-cl-session session-id)))
    (setf (cl-session-last-activity session) (local-time:now))
    (let ((*package* (find-package (cl-session-package-name session))))
      (handler-case
          (let* ((form (read-from-string code))
                 (results (multiple-value-list (eval form))))
            ;; Update session's package if it changed
            (setf (cl-session-package-name session) (package-name *package*))
            (values results nil nil))
        (error (e)
          (values nil t (princ-to-string e)))))))

(defun cl-load-file (path &key (session-id "default"))
  "Load a Common Lisp file in a session context.

Returns (values result error-p error-message)."
  (let ((session (ensure-cl-session session-id)))
    (setf (cl-session-last-activity session) (local-time:now))
    (let ((*package* (find-package (cl-session-package-name session))))
      (handler-case
          (progn
            (load path)
            (setf (cl-session-package-name session) (package-name *package*))
            (values t nil nil))
        (error (e)
          (values nil t (princ-to-string e)))))))

(defun cl-define-function (name lambda-list body &key (session-id "default"))
  "Define a function in a session.

Returns (values function-name error-p error-message)."
  (let ((session (ensure-cl-session session-id)))
    (setf (cl-session-last-activity session) (local-time:now))
    (let ((*package* (find-package (cl-session-package-name session))))
      (handler-case
          (progn
            (eval `(defun ,name ,lambda-list ,body))
            (values name nil nil))
        (error (e)
          (values nil t (princ-to-string e)))))))

(defun list-cl-sessions ()
  "List all active CL sessions."
  (let ((result '()))
    (maphash (lambda (id session)
               (push (list :id id
                          :package (cl-session-package-name session)
                          :created (local-time:format-timestring
                                   nil (cl-session-created-at session))
                          :last-activity (local-time:format-timestring
                                         nil (cl-session-last-activity session)))
                     result))
             *cl-sessions*)
    result))
