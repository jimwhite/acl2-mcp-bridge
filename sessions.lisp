
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Per-Client CL Session Management
;;;
;;; Based on ACL2 Bridge: "When a client connects, it creates a new worker 
;;; thread to handle the client's requests."
;;;
;;; Each MCP client (identified by session ID) gets:
;;; - Own private package (SESSION-<id>) for true symbol isolation
;;; - Own function/variable definitions (in their package)
;;; - Own activity tracking
;;;
;;; This provides real isolation: defun in session A creates A::foo,
;;; defun in session B creates B::foo - completely separate symbols.
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Session Package Management
;;; ---------------------------------------------------------------------------

(defun make-session-package-name (session-id)
  "Generate unique package name for a session."
  (format nil "SESSION-~A" session-id))

(defun create-session-package (session-id)
  "Create a fresh package for SESSION-ID that inherits CL and useful libs."
  (let ((pkg-name (make-session-package-name session-id)))
    ;; Delete if exists (shouldn't happen but be safe)
    (let ((existing (find-package pkg-name)))
      (when existing
        (delete-package existing)))
    ;; Create package that uses CL (core) and inherits useful symbols
    (make-package pkg-name :use '(:cl))))

(defun destroy-session-package (session-id)
  "Delete the package for SESSION-ID."
  (let* ((pkg-name (make-session-package-name session-id))
         (pkg (find-package pkg-name)))
    (when pkg
      ;; Unintern all symbols to allow clean deletion
      (do-symbols (sym pkg)
        (when (eq (symbol-package sym) pkg)
          (unintern sym pkg)))
      (delete-package pkg))))

;;; ---------------------------------------------------------------------------
;;; Session State Structure
;;; ---------------------------------------------------------------------------

(defstruct cl-session
  "State for a CL evaluation session."
  (id "default" :type string)
  (eval-package nil)  ; Session's private package
  (hons-space nil)    ; ACL2 hons space for this session (thread safety)
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
  "Get existing session or create new one for SESSION-ID.
Creates a private package and hons space for the session."
  (bt:with-lock-held (*sessions-lock*)
    (or (gethash session-id *sessions*)
        (let* ((pkg (create-session-package session-id))
               ;; Create a dedicated hons space for ACL2 thread safety
               ;; This is what centaur/bridge does for each worker thread
               (hs (acl2::hl-hspace-init))
               (session (make-cl-session :id session-id 
                                         :eval-package pkg
                                         :hons-space hs)))
          (setf (gethash session-id *sessions*) session)
          (log:info "Created new CL session: ~A (package ~A, hons-space ~A)" 
                    session-id (package-name pkg) hs)
          session))))

(defun get-session (session-id)
  "Get session by ID, or nil if not found."
  (bt:with-lock-held (*sessions-lock*)
    (gethash session-id *sessions*)))

(defun destroy-session (session-id)
  "Destroy session and clean up its resources (including its package)."
  (bt:with-lock-held (*sessions-lock*)
    (let ((session (gethash session-id *sessions*)))
      (when session
        ;; Delete the session's package (cleans up all symbols)
        (destroy-session-package session-id)
        (remhash session-id *sessions*)
        (log:info "Destroyed CL session: ~A" session-id)
        t))))

(defun list-all-sessions ()
  "List all active sessions."
  (bt:with-lock-held (*sessions-lock*)
    (loop for session being the hash-values of *sessions*
          collect (list :id (cl-session-id session)
                        :package (package-name (cl-session-eval-package session))
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
  "Evaluate Common Lisp code in a session's private package.
Returns (values result-list error-p error-message).
Definitions are automatically isolated to this session's package."
  (unless session
    (return-from cl-eval (values nil t "No active session")))
  (with-session (session)
    (handler-case
        (let* ((form (read-from-string code))
               (results (multiple-value-list (eval form))))
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
  "Define a function in a session's private package.
NAME, LAMBDA-LIST, and BODY should be strings - they are read in the session's package.
Returns (values function-name error-p error-message)."
  (unless session
    (return-from cl-define-function (values nil t "No active session")))
  (with-session (session)
    (handler-case
        (let ((name-sym (read-from-string name))
              (params (read-from-string lambda-list))
              (body-form (read-from-string body)))
          (eval `(defun ,name-sym ,params ,body-form))
          (values name-sym nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-get-package (&optional (session *current-session*))
  "Return current evaluation package name for a session.
Returns (values package-name error-p error-message)."
  (if session
      (values (package-name (cl-session-eval-package session)) nil nil)
      (values nil t "No active session")))

(defun cl-reset (&optional (session *current-session*))
  "Reset a session's evaluation context by recreating its package.
Returns (values message error-p error-message)."
  (unless session
    (return-from cl-reset (values nil t "No active session")))
  (handler-case
      (let ((session-id (cl-session-id session)))
        ;; Destroy and recreate the package
        (destroy-session-package session-id)
        (let ((new-pkg (create-session-package session-id)))
          (setf (cl-session-eval-package session) new-pkg))
        (values "Evaluation context reset to initial state" nil nil))
    (error (e)
      (values nil t (princ-to-string e)))))

(defun cl-query-package (&optional package-name (session *current-session*))
  "Introspect a package and return information about its contents.
Returns (values info-string error-p error-message)."
  (handler-case
      (let* ((pkg (if package-name
                      (or (find-package (string-upcase package-name))
                          (return-from cl-query-package 
                            (values nil t (format nil "Package ~A not found" package-name))))
                      (if session
                          (cl-session-eval-package session)
                          *package*)))
             (functions '())
             (macros '())
             (variables '())
             (constants '()))
        ;; Collect symbols
        (do-symbols (sym pkg)
          (when (eq (symbol-package sym) pkg)  ; Only symbols owned by this package
            (cond
              ((constantp sym) (push (symbol-name sym) constants))
              ((and (boundp sym) (not (fboundp sym))) (push (symbol-name sym) variables))
              ((macro-function sym) (push (symbol-name sym) macros))
              ((fboundp sym) (push (symbol-name sym) functions)))))
        (values (format nil "Package: ~A~%~
                            Uses: ~{~A~^, ~}~%~
                            Functions (~D): ~{~A~^, ~}~%~
                            Macros (~D): ~{~A~^, ~}~%~
                            Variables (~D): ~{~A~^, ~}~%~
                            Constants (~D): ~{~A~^, ~}"
                       (package-name pkg)
                       (mapcar #'package-name (package-use-list pkg))
                       (length functions) (sort functions #'string<)
                       (length macros) (sort macros #'string<)
                       (length variables) (sort variables #'string<)
                       (length constants) (sort constants #'string<))
                nil nil))
    (error (e)
      (values nil t (princ-to-string e)))))
