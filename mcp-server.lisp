(in-package #:acl2-mcp-bridge)

;; Define a single unified API with only the currently implemented CL tools
;; The MCP server itself provides session isolation - clients spawn multiple
;; server instances if they need separate environments.
(define-api (acl2-mcp-tools :title "ACL2 MCP Bridge Tools"))

(define-tool (acl2-mcp-tools eval-cl) (code)
  (:summary "Evaluate Common Lisp code")
  (:param code string "Lisp code to evaluate")
  (:result (soft-list-of text-content))
  (multiple-value-bind (results error-p error-msg)
      (cl-eval code)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content 
                            :text (cond
                                   ((null results) "NIL")
                                   ((null (cdr results)) (format nil "~S" (car results)))
                                   (t (format nil "~{~S~^~%~}" results))))))))


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
