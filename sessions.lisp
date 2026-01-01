
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Per-Client CL Session Management
;;;
;;; Based on ACL2 Bridge: "When a client connects, it creates a new worker 
;;; thread to handle the client's requests."
;;;
;;; Each MCP client (identified by session ID) gets:
;;; - Own package context (can be in different packages)
;;; - Own user-defined symbol tracking (for reset-cl cleanup)
;;; - Own activity tracking
;;;
;;; NOTE: CL functions/macros are inherently global. Sessions track which 
;;; symbols each client defined so reset-cl can clean up that client's 
;;; definitions. For true isolation, use separate processes.
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Session State Structure
;;; ---------------------------------------------------------------------------

(defstruct cl-session
  "State for a CL evaluation session."
  (id "default" :type string)
  (eval-package (find-package "CL-USER"))
  (user-defined-symbols nil :type list)
  (created (get-universal-time) :type integer)
  (last-activity (get-universal-time) :type integer))

;;; ---------------------------------------------------------------------------
;;; Session Registry
;;; ---------------------------------------------------------------------------

(defvar *sessions* (make-hash-table :test 'equal)
  "Map of session-id -> cl-session.")

(defvar *sessions-lock* (bt:make-lock "sessions-lock")
  "Lock for thread-safe session access.")

(defvar *current-session* nil
  "The session for the current request (bound per-request by transport).")

;;; ---------------------------------------------------------------------------
;;; Session Management
;;; ---------------------------------------------------------------------------

(defun get-or-create-session (session-id)
  "Get existing session or create new one for SESSION-ID."
  (bt:with-lock-held (*sessions-lock*)
    (or (gethash session-id *sessions*)
        (let ((session (make-cl-session :id session-id)))
          (setf (gethash session-id *sessions*) session)
          (log:info "Created new CL session: ~A" session-id)
          session))))

(defun get-session (session-id)
  "Get session by ID, or nil if not found."
  (bt:with-lock-held (*sessions-lock*)
    (gethash session-id *sessions*)))

(defun destroy-session (session-id)
  "Destroy session and clean up its resources."
  (bt:with-lock-held (*sessions-lock*)
    (let ((session (gethash session-id *sessions*)))
      (when session
        ;; Clean up user-defined symbols
        (dolist (sym (cl-session-user-defined-symbols session))
          (ignore-errors
            (when (fboundp sym) (fmakunbound sym))
            (when (boundp sym) (makunbound sym))))
        (remhash session-id *sessions*)
        (log:info "Destroyed CL session: ~A" session-id)
        t))))

(defun list-all-sessions ()
  "List all active sessions."
  (bt:with-lock-held (*sessions-lock*)
    (loop for session being the hash-values of *sessions*
          collect (list :id (cl-session-id session)
                        :package (package-name (cl-session-eval-package session))
                        :symbols (length (cl-session-user-defined-symbols session))
                        :created (cl-session-created session)
                        :last-activity (cl-session-last-activity session)))))

;;; ---------------------------------------------------------------------------
;;; Execute in Session Context
;;; ---------------------------------------------------------------------------

(defmacro with-session ((session) &body body)
  "Execute BODY in SESSION's evaluation context."
  (let ((sess (gensym "SESSION")))
    `(let ((,sess ,session))
       (setf (cl-session-last-activity ,sess) (get-universal-time))
       (let ((*package* (cl-session-eval-package ,sess)))
         (unwind-protect
             (progn ,@body)
           ;; Save package back if changed (e.g., by IN-PACKAGE)
           (setf (cl-session-eval-package ,sess) *package*))))))

;;; ---------------------------------------------------------------------------
;;; CL Evaluation Functions (use *current-session* bound by transport)
;;; ---------------------------------------------------------------------------

(defun cl-eval (code &optional (session *current-session*))
  "Evaluate Common Lisp code in a session's context.
Returns (values result-list error-p error-message)."
  (unless session
    (return-from cl-eval (values nil t "No active session")))
  (with-session (session)
    (handler-case
        (let* ((form (read-from-string code))
               (results (multiple-value-list (eval form))))
          ;; Track defined symbols
          (when (and (consp form) 
                     (member (car form) '(defun defvar defparameter defconstant defmacro)))
            (pushnew (second form) (cl-session-user-defined-symbols session)))
          (values results nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-load-file (path &optional (session *current-session*))
  "Load a Common Lisp file in a session's context.
Returns (values result error-p error-message)."
  (unless session
    (return-from cl-load-file (values nil t "No active session")))
  (with-session (session)
    (handler-case
        (progn (load path) (values t nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-define-function (name lambda-list body &optional (session *current-session*))
  "Define a function in a session's context.
Returns (values function-name error-p error-message)."
  (unless session
    (return-from cl-define-function (values nil t "No active session")))
  (with-session (session)
    (handler-case
        (progn
          (eval `(defun ,name ,lambda-list ,body))
          (pushnew name (cl-session-user-defined-symbols session))
          (values name nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-get-package (&optional (session *current-session*))
  "Return current evaluation package name for a session.
Returns (values package-name error-p error-message)."
  (if session
      (values (package-name (cl-session-eval-package session)) nil nil)
      (values nil t "No active session")))

(defun cl-reset (&optional (session *current-session*))
  "Reset a session's evaluation context to initial state.
Returns (values message error-p error-message)."
  (unless session
    (return-from cl-reset (values nil t "No active session")))
  (handler-case
      (progn
        ;; Unbind all user-defined symbols
        (dolist (sym (cl-session-user-defined-symbols session))
          (when (fboundp sym) (fmakunbound sym))
          (when (boundp sym) (makunbound sym)))
        (setf (cl-session-user-defined-symbols session) nil)
        ;; Reset to CL-USER package
        (setf (cl-session-eval-package session) (find-package "CL-USER"))
        (values "Evaluation context reset to initial state" nil nil))
    (error (e)
      (values nil t (princ-to-string e)))))
