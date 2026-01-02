;;; Start the ACL2 MCP HTTP server
;;; Usage: acl2 < start-mcp-server.lisp

;; Exit ACL2 to raw Lisp
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
(funcall (find-symbol "START-SERVER" (find-package "ACL2-MCP-BRIDGE")) 
         :protocol :mcp :transport :http :port 8085)
(format t "~%MCP Server started on http://127.0.0.1:8085/mcp~%")
(format t "~%Press Ctrl+C to stop.~%")
(force-output)

;; Keep running - the HTTP server runs in threads  
(loop (sleep 3600))
