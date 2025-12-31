
(asdf:defsystem "acl2-mcp-40ants"
  :description "ACL2 + Common Lisp MCP server built on 40ants-mcp"
  :author "Generated"
  :license "MIT"
  :depends-on ("40ants-mcp" "uiop")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "acl2-interface")
               (:file "sessions")
               (:file "tools-acl2")
               (:file "tools-cl")
               (:file "tools-bridge")
               (:file "main")))
