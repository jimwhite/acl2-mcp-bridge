
(defpackage #:acl2-mcp-40ants
  (:use #:cl)
  (:import-from #:40ants-mcp/content/text #:text-content)
  (:import-from #:40ants-mcp/tools #:define-tool)
  (:import-from #:openrpc-server #:define-api)
  (:export
   #:*server*
   #:start-server
   #:stop-server))

(in-package #:acl2-mcp-40ants)
