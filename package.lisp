
(defpackage #:acl2-mcp-bridge
  (:use #:cl)
  (:import-from #:40ants-mcp/content/text #:text-content)
  (:import-from #:40ants-mcp/tools #:define-tool)
  (:import-from #:openrpc-server #:define-api)
  (:export
   #:start-server
   #:start-both
   #:stop-server
    #:initialize-acl2-interface
    #:*acl2-executable*
   #:*bridge-server*
   #:*mcp-server*
   #:*acl2-sessions*
   #:*cl-sessions*))

(in-package #:acl2-mcp-bridge)
