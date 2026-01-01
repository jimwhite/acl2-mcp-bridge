
(defpackage #:acl2-mcp-bridge
  (:use #:cl)
  (:import-from #:40ants-mcp/content/text #:text-content)
  (:import-from #:40ants-mcp/tools #:define-tool)
  (:import-from #:openrpc-server #:define-api)
  (:import-from #:openrpc-server/method #:method-thunk)
  (:import-from #:40ants-mcp/stdio-transport #:stdio-transport)
  (:import-from #:40ants-mcp/http-transport #:http-transport)
  (:import-from #:40ants-mcp/transport/base #:start-loop)
  (:import-from #:40ants-mcp/server/definition #:handle-message)
  (:import-from #:jsonrpc #:make-server #:expose)
  (:export
   ;; Server management
   #:start-server
   #:start-both
   #:stop-server
   #:*bridge-server*
   #:*mcp-server*
   
   ;; Session-aware transport
   #:session-http-transport
   
   ;; Session management
   #:get-or-create-session
   #:get-session
   #:destroy-session
   #:list-all-sessions
   #:*current-session*
   #:*sessions*
   #:cl-session
   #:cl-session-p
   #:cl-session-id
   #:cl-session-eval-package
   
   ;; CL evaluation
   #:cl-eval
   #:cl-load-file
   #:cl-define-function
   #:cl-get-package
   #:cl-reset
   
   ;; ACL2 interface
   #:start-acl2-session
   #:stop-acl2-session
   #:get-acl2-session
   #:*acl2-sessions*
   #:initialize-acl2-interface
   #:*acl2-executable*))

(in-package #:acl2-mcp-bridge)

