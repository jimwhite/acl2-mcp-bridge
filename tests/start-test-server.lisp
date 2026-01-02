;; MCP Test Server Startup Script
;; Used by run-mcp-client-tests.sh
;;
;; Drop to raw Lisp first
:q

;; Disable debugger for non-interactive use
(sb-ext:disable-debugger)

;; Load quicklisp for dependencies
(load "~/quicklisp/setup.lisp")

;; Add project paths to ASDF  
(push #p"/workspaces/acl2-mcp-bridge/" asdf:*central-registry*)
(push #p"/workspaces/acl2-mcp-bridge/vendor/40ants-mcp/" asdf:*central-registry*)

;; Load the system
(asdf:load-system :acl2-mcp-bridge)
(format t "~%System loaded successfully~%")

;; Start the MCP HTTP server (protocol :mcp, not :bridge)
;; This enters the main-thread-loop which blocks forever
(funcall (find-symbol "START-SERVER" (find-package "ACL2-MCP-BRIDGE")) 
         :protocol :mcp :port 8080)

;; Note: START-SERVER with :main-thread-loop t (default) blocks forever
;; processing ACL2 work from HTTP threads. The server is ready when
;; "Entering main thread loop" appears in the log.
