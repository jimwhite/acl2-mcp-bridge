
(in-package #:acl2-mcp-bridge)

;; Bridge tools for ACL2/CL interop

(define-api (bridge-api :title "ACL2/CL Bridge Tools"))

(define-tool (bridge-api acl2-to-cl) (code &optional session-id)
  (:summary "Evaluate ACL2 and translate result to CL-readable form")
  (:param code string "ACL2 form")
  (:param session-id string "Session identifier")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval code :session-id (or session-id "default") :main-thread-p t)
    (if error-p
      (list (make-instance 'text-content :text (format nil "ERROR: ~A" error-msg)))
      (list (make-instance 'text-content :text (format nil "~S" result))))))

(define-tool (bridge-api cl-to-acl2) (code)
  (:summary "Prepare CL value for ACL2 (serialization)")
  (:param code string "Common Lisp expression")
  (:result (soft-list-of text-content))
  ;; Uses *current-session* bound by transport
  (handler-case
    (let ((cl-value (eval (read-from-string code))))
      (list (make-instance 'text-content :text (format nil "~S" cl-value))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "ERROR: ~A" e))))))

(define-tool (bridge-api cross-eval) (acl2-code cl-code &optional session-id)
  (:summary "Evaluate both ACL2 and CL forms, return both results")
  (:param acl2-code string "ACL2 form")
  (:param cl-code string "Common Lisp form")
  (:param session-id string "Session identifier (for ACL2)")
  (:result (soft-list-of text-content))
  ;; CL uses *current-session* bound by transport, ACL2 uses explicit session-id
  (let* ((sid (or session-id "default"))
         (acl2-result (acl2-eval acl2-code :session-id sid :main-thread-p t))
         (cl-result (cl-eval cl-code)))  ; Uses *current-session*
    (list (make-instance 'text-content 
                        :text (format nil "ACL2: ~S~%CL: ~S" acl2-result cl-result)))))

