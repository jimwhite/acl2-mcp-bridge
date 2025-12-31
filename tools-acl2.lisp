
(in-package #:acl2-mcp-bridge)

;; ACL2-specific MCP tools

(define-api (acl2-api :title "ACL2 MCP Tools"))

(define-tool (acl2-api check-theorem) (conjecture &optional session-id hints)
  (:summary "Check if a conjecture is provable in ACL2")
  (:param conjecture string "Theorem statement as s-expression")
  (:param session-id string "Session identifier" :optional t)
  (:param hints string "Proof hints (optional)" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-check-theorem conjecture :session-id (or session-id "default") :hints hints)
    (list (make-instance 'text-content 
                        :text (format nil "Success: ~A~%Error: ~A~%Output:~%~A"
                                     success error-msg output)))))

(define-tool (acl2-api admit) (event &optional session-id)
  (:summary "Admit an event (defun, defthm, etc) to ACL2")
  (:param event string "ACL2 event as s-expression")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-admit event :session-id (or session-id "default"))
    (list (make-instance 'text-content 
                        :text (format nil "Success: ~A~%Error: ~A~%Output:~%~A"
                                     success error-msg output)))))

(define-tool (acl2-api verify-guards) (function &optional session-id)
  (:summary "Verify guards for a function")
  (:param function string "Function name")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-verify-guards function :session-id (or session-id "default"))
    (list (make-instance 'text-content 
                        :text (format nil "Success: ~A~%Error: ~A~%Output:~%~A"
                                     success error-msg output)))))

(define-tool (acl2-api query-event) (form &optional session-id)
  (:summary "Query ACL2 for computation (non-event)")
  (:param form string "ACL2 form")
  (:param session-id string "Session identifier" :optional t)
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (acl2-query form :session-id (or session-id "default"))
    (list (make-instance 'text-content 
                        :text (format nil "Result: ~S~%Error: ~A" result error-msg)))))

(define-tool (acl2-api list-sessions) ()
  (:summary "List all active ACL2 sessions")
  (:result (soft-list-of text-content))
  (let ((sessions (list-acl2-sessions)))
    (list (make-instance 'text-content :text (format nil "~S" sessions)))))
