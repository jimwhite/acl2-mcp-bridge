#!/bin/bash
# Script to start the ACL2 Bridge server

cd /workspaces/acl2-mcp-bridge

$ACL2 <<'LISP_EOF'
:q
(load "~/quicklisp/setup.lisp")
(push "/workspaces/acl2-mcp-bridge/" asdf:*central-registry*)
(push "/workspaces/acl2-mcp-bridge/vendor/40ants-mcp/" asdf:*central-registry*)
(asdf:load-system "acl2-mcp-bridge")
(format t "~%Starting Bridge Server on port 55433...~%")
(acl2-mcp-bridge:start-bridge-server)
(format t "Bridge server started. Press Ctrl+C to stop.~%")
(loop (sleep 1))
LISP_EOF
