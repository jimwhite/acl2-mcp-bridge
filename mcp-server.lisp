(in-package #:acl2-mcp-bridge)

(defun start-mcp-server (&key (transport :stdio) host port)
  "Start the MCP server via 40ants-mcp and return the server instance.

HOST is only applied for HTTP transports; PORT is forwarded when provided."
  (let* ((apis (list #'acl2-api #'cl-api #'bridge-api))
         (args (append (list :transport transport)
                       (when port (list :port port))
                       (when (and host (eq transport :http)) (list :host host)))))
    (apply #'40ants-mcp/server/definition:start-server (cons apis args))))

;; Note: 40ants-mcp has no public stop API; stopping requires terminating the process.
