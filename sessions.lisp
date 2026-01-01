
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Common Lisp Evaluation Context
;;; 
;;; Simple in-process evaluation. The MCP server itself provides the isolation
;;; boundary - clients can spawn multiple server instances if they need 
;;; separate environments. One MCP connection = one evaluation context.
;;; ============================================================================

(defparameter *eval-package* (find-package "CL-USER")
  "Current package for evaluation context.")

(defun cl-eval (code)
  "Evaluate Common Lisp code in the server's evaluation context.

Returns (values result-list error-p error-message)."
  (let ((*package* *eval-package*))
    (handler-case
        (let* ((form (read-from-string code))
               (results (multiple-value-list (eval form))))
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

Returns (values function-name error-p error-message)."
  (let ((*package* *eval-package*))
    (handler-case
        (progn
          (eval `(defun ,name ,lambda-list ,body))
          (values name nil nil))
      (error (e)
        (values nil t (princ-to-string e))))))
