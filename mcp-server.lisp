(in-package #:acl2-mcp-bridge)

;; Define a single unified API with only the currently implemented CL tools
(define-api (acl2-mcp-tools :title "ACL2 MCP Bridge Tools"))

(define-tool (acl2-mcp-tools eval-cl) (code &optional package-name)
  (:summary "Evaluate Common Lisp code")
  (:param code string "Lisp code to evaluate")
  (:param package-name string "Package context (default CL-USER)")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((pkg (or (and package-name (find-package (string-upcase package-name))) :cl-user))
             (*package* (find-package pkg)))
        (multiple-value-bind (result)
            (eval (read-from-string code))
          (list (make-instance 'text-content 
                              :text (format nil "~S" result)))))
    (error (e)
      (list (make-instance 'text-content 
                          :text (format nil "Error: ~A" e))))))

(define-tool (acl2-mcp-tools list-sessions) ()
  (:summary "List all active sessions")
  (:result (soft-list-of text-content))
  (let ((acl2-sessions (list-acl2-sessions))
        (cl-sessions (list-cl-sessions)))
    (list (make-instance 'text-content 
                        :text (format nil "ACL2 Sessions: ~S~%CL Sessions: ~S" 
                                     acl2-sessions cl-sessions)))))


(defun start-mcp-server (&key (transport :stdio) host port)
  "Start the MCP server using 40ants-mcp's start-server.

This properly exposes tools via MCP's tools/list and tools/call methods."
  (declare (ignore host))
  (log:info "Starting MCP server with transport ~A" transport)
  (40ants-mcp/server/definition:start-server 
   acl2-mcp-tools
   :transport transport
   :port port))

;; Note: 40ants-mcp has no public stop API; stopping requires terminating the process.
