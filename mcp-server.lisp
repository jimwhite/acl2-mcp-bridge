(in-package #:acl2-mcp-bridge)

(defvar *acl2-mcp-server* nil
  "Global MCP server instance")

(defun start-server (&key (acl2-path nil) (transport :stdio) (host "127.0.0.1") (port 3000))
  "Start the ACL2 MCP Bridge server
   
   ACLD2-PATH: Optional path to ACL2 executable
   TRANSPORT:  :stdio (default) or :http
   HOST/PORT:  For HTTP transport"
  
  (let ((acl2-path (or acl2-path (uiop:getenv "ACL2_PATH") "acl2")))
    
    ;; Initialize ACL2 interface
    (initialize-acl2-interface acl2-path)
    
    ;; Create 40ants-mcp server
    (setf *acl2-mcp-server*
          (openrpc-server:define-api (acl2-bridge-tools :title "ACL2 MCP Bridge")
            :description "Unified interface to ACL2 + Common Lisp via MCP"))
    
    ;; Register all tools
    (register-acl2-tools)
    (register-cl-tools)
    (register-bridge-tools)
    (register-resources)
    (register-prompts)
    
    ;; Start server with appropriate transport
    (ecase transport
      (:stdio
       (40ants-mcp/server/definition:start-server *acl2-mcp-server*))
      (:http
       (40ants-mcp/transports/http:start-http-server *acl2-mcp-server*
                                                       :host host
                                                       :port port)))
    
    (format t "~%ACL2 MCP Bridge started successfully~%")))

(defun stop-server ()
  "Stop the MCP server"
  (when *acl2-mcp-server*
    (cleanup-sessions)
    (setf *acl2-mcp-server* nil)))
