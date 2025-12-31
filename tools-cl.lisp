
(in-package #:acl2-mcp-40ants)

(define-api (cl-api :title "Common Lisp MCP Tools"))

(define-tool (cl-api eval-cl) (code &optional session-id)
  (:summary "Evaluate Common Lisp code in a persistent session")
  (:param code string "Common Lisp form to evaluate")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (let* ((sid (or session-id "default"))
         (values (cl-eval code :session-id sid)))
    (list (make-instance 'text-content :text (princ-to-string values)))))
