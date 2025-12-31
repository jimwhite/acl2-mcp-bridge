
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
               "log4cl")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "acl2-interface")
               (:file "sessions")
               (:file "threading-utils")
               (:file "bridge-protocol")
               (:file "message-format")
               (:file "mcp-tools")
               (:file "mcp-server")
               (:file "tools-acl2")
               (:file "tools-cl")
               (:file "tools-bridge")
               (:file "main")))
