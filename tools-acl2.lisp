
(in-package #:acl2-mcp-40ants)

(define-api (acl2-api :title "ACL2 MCP Tools"))

(define-tool (acl2-api eval-acl2) (code &optional session-id)
  (:summary "Evaluate ACL2 code in a given session")
  (:param code string "ACL2 event or form to evaluate")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (let* ((sid (or session-id "default"))
         (result (acl2-eval code :session-id sid)))
    (list (make-instance 'text-content :text (princ-to-string result)))))
