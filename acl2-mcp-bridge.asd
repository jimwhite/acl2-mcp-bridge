
(asdf:defsystem "acl2-mcp-bridge"
  :description "ACL2 Bridge + MCP server built on 40ants-mcp"
  :author "Generated"
  :license "MIT"
  ;; Use local vendor/40ants-mcp instead of Quicklisp version
  :defsystem-depends-on ()
  :depends-on ("40ants-mcp" 
               "uiop"
               "usocket"
               "bordeaux-threads"
               "trivial-gray-streams"
               "local-time"
               "cl-ppcre"
               "yason"
               "log4cl"
               "uuid")
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "bridge-sbcl")
               (:file "sessions")
               (:file "acl2-interface")
               (:file "session-transport")
               (:file "threading-utils")
               (:file "bridge-protocol")
               (:file "message-format")
               (:file "mcp-server")      ; Defines acl2-mcp-tools API
               (:file "tools-acl2")      ; ACL2 tools (uses acl2-mcp-tools)
               (:file "tools-cl")        ; (currently empty/placeholder)
               (:file "tools-bridge")    ; Bridge tools (uses bridge-api)
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
