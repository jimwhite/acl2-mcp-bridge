
(asdf:defsystem "acl2-mcp-bridge"
  :description "ACL2 Bridge + MCP server built on 40ants-mcp"
  :author "Generated"
  :license "MIT"
  :depends-on ("40ants-mcp" 
               "uiop"
               "usocket"
               "bordeaux-threads"
               "local-time"
               "cl-ppcre"
               "yason"
               "log4cl"
               "uuid")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "acl2-interface")
               (:file "sessions")
               (:file "session-transport")
               (:file "threading-utils")
               (:file "bridge-protocol")
               (:file "message-format")
               (:file "tools-acl2")
               (:file "tools-cl")
               (:file "tools-bridge")
               (:file "mcp-server")
               (:file "main")))

(asdf:defsystem "acl2-mcp-bridge/tests"
  :description "Tests for acl2-mcp-bridge"
  :author "Generated"
  :license "MIT"
  :depends-on ("acl2-mcp-bridge" "fiveam")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "startup-tests")
                             (:file "message-format-tests")
                             (:file "readme-basic-server-tests")
                             (:file "run-tests"))))
  :perform (test-op (op c)
             (uiop:symbol-call '#:acl2-mcp-bridge/tests '#:run-tests)))
