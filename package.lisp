
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
   #:start-server
   #:start-both
   #:stop-server
    #:start-cl-session
    #:stop-cl-session
    #:get-cl-session
    #:start-acl2-session
    #:stop-acl2-session
    #:get-acl2-session
    #:initialize-acl2-interface
    #:*acl2-executable*
   #:*bridge-server*
   #:*mcp-server*
   #:*acl2-sessions*
   #:*cl-sessions*))

(in-package #:acl2-mcp-bridge)
