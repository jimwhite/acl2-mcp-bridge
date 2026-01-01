(in-package #:acl2-mcp-bridge)

;; Define a single unified API with only the currently implemented CL tools
(define-api (acl2-mcp-tools :title "ACL2 MCP Bridge Tools"))

(define-tool (acl2-mcp-tools eval-cl) (code &optional session-id)
  (:summary "Evaluate Common Lisp code in a persistent session")
  (:param code string "Lisp code to evaluate")
  (:param session-id string "Session identifier (default: 'default')")
  (:result (soft-list-of text-content))
  (multiple-value-bind (results error-p error-msg)
      (cl-eval code :session-id (or session-id "default"))
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content 
                            :text (if (= 1 (length results))
                                      (format nil "~S" (first results))
                                      (format nil "~{~S~^~%~}" results)))))))

(define-tool (acl2-mcp-tools start-session) ()
  (:summary "Start a new Common Lisp session and return its id")
  (:result (soft-list-of text-content))
  (let ((id (start-cl-session)))
    (list (make-instance 'text-content :text id))))

(define-tool (acl2-mcp-tools stop-session) (session-id)
  (:summary "Stop and remove a Common Lisp session")
  (:param session-id string "Session identifier to stop")
  (:result (soft-list-of text-content))
  (if (stop-cl-session session-id)
      (list (make-instance 'text-content :text (format nil "Stopped ~A" session-id)))
      (list (make-instance 'text-content :text (format nil "Session ~A not found" session-id)))))

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
