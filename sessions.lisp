
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Common Lisp Evaluation Context
;;; 
;;; Based on the ACL2 Bridge model: each MCP connection gets a worker thread 
;;; that provides a read-eval-print loop. The MCP protocol handles session 
;;; isolation at the transport level - clients needing separate environments 
;;; should spawn multiple server instances.
;;;
;;; Per ACL2 Bridge docs: "When a client connects, it creates a new worker 
;;; thread to handle the client's requests. The worker thread presents the 
;;; client with a kind of read-eval-print loop."
;;; ============================================================================

(defparameter *eval-package* (find-package "CL-USER")
  "Current package for evaluation context.")

(defparameter *user-defined-symbols* nil
  "List of symbols defined by user during this session (for reset).")

(defun cl-reset ()
  "Reset the evaluation context to initial state.

Unbinds user-defined functions/variables and resets package to CL-USER.
Returns (values message error-p error-message)."
  (handler-case
      (progn
        ;; Unbind all user-defined symbols
        (dolist (sym *user-defined-symbols*)
          (when (fboundp sym)
            (fmakunbound sym))
          (when (boundp sym)
            (makunbound sym)))
        (setf *user-defined-symbols* nil)
        ;; Reset to CL-USER package
        (setf *eval-package* (find-package "CL-USER"))
        (values "Evaluation context reset to initial state" nil nil))
    (error (e)
      (values nil t (princ-to-string e)))))

(defun track-defined-symbol (sym)
  "Track a symbol as user-defined for later cleanup."
  (pushnew sym *user-defined-symbols*))

(defun cl-eval (code)
  "Evaluate Common Lisp code in the server's evaluation context.

Returns (values result-list error-p error-message).
Tracks any defined symbols for potential reset."
  (let ((*package* *eval-package*))
    (handler-case
        (let* ((form (read-from-string code))
               (results (multiple-value-list (eval form))))
          ;; Track defined symbols
          (when (and (consp form) 
                     (member (car form) '(defun defvar defparameter defconstant defmacro)))
            (track-defined-symbol (second form)))
          ;; Update package if it changed (e.g., via IN-PACKAGE)
          (setf *eval-package* *package*)
          (values results nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-load-file (path)
  "Load a Common Lisp file in the evaluation context.

Returns (values result error-p error-message)."
  (let ((*package* *eval-package*))
    (handler-case
        (progn
          (load path)
          (setf *eval-package* *package*)
          (values t nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-define-function (name lambda-list body)
  "Define a function in the evaluation context.

Returns (values function-name error-p error-message).
The function is tracked for potential reset."
  (let ((*package* *eval-package*))
    (handler-case
        (progn
          (eval `(defun ,name ,lambda-list ,body))
          (track-defined-symbol name)
          (values name nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))

(defun cl-get-package ()
  "Return current evaluation package name.

Returns (values package-name error-p error-message)."
  (values (package-name *eval-package*) nil nil))
