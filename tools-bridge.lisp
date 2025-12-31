
(in-package #:acl2-mcp-40ants)

(define-api (bridge-api :title "ACL2/CL Bridge Tools"))

(define-tool (bridge-api acl2-to-cl) (code &optional session-id)
  (:summary "Evaluate ACL2 form and translate result into CL-readable form.")
  (:param code string "ACL2 form")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (let* ((sid (or session-id "default"))
         (acl2-result (acl2-eval code :session-id sid)))
    (list (make-instance 'text-content :text (princ-to-string acl2-result)))))
