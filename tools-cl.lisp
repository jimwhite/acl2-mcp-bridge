
(in-package #:acl2-mcp-40ants)

;; Common Lisp-specific MCP tools

(define-api (cl-api :title "Common Lisp MCP Tools"))

(define-tool (cl-api eval-cl) (code &optional session-id)
  (:summary "Evaluate Common Lisp code in a persistent session")
  (:param code string "Common Lisp form(s)")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (results error-p error-msg)
      (cl-eval code :session-id (or session-id "default"))
    (if error-p
      (list (make-instance 'text-content :text (format nil "ERROR: ~A" error-msg)))
      (list (make-instance 'text-content :text (format nil "~S" results))))))

(define-tool (cl-api load-file) (path &optional session-id)
  (:summary "Load a Common Lisp file in a session")
  (:param path string "Path to .lisp file")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (cl-load-file path :session-id (or session-id "default"))
    (if error-p
      (list (make-instance 'text-content :text (format nil "ERROR: ~A" error-msg)))
      (list (make-instance 'text-content :text "File loaded successfully")))))

(define-tool (cl-api define-function) (name lambda-list body &optional session-id)
  (:summary "Define a function in a session")
  (:param name string "Function name")
  (:param lambda-list string "Parameter list")
  (:param body string "Function body")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (cl-define-function (read-from-string name) 
                         (read-from-string lambda-list)
                         (read-from-string body)
                         :session-id (or session-id "default"))
    (if error-p
      (list (make-instance 'text-content :text (format nil "ERROR: ~A" error-msg)))
      (list (make-instance 'text-content :text (format nil "Defined ~A" name))))))

(define-tool (cl-api list-sessions) ()
  (:summary "List all active Common Lisp sessions")
  (:result (soft-list-of text-content))
  (let ((sessions (list-cl-sessions)))
    (list (make-instance 'text-content :text (format nil "~S" sessions)))))
