
(in-package #:acl2-mcp-40ants)

;; ACL2 Bridge Configuration
(defparameter *bridge-port* 13721)
(defparameter *bridge-socket-path* nil)  ;; Unix domain socket if set
(defparameter *bridge-listen-address* "127.0.0.1")

;; MCP Configuration
(defparameter *mcp-transport* :stdio)
(defparameter *mcp-port* 8085)

;; Threading Configuration
(defparameter *main-thread* (bt:current-thread))
(defparameter *acl2-main-lock* (bt:make-lock "acl2-main"))

;; Logging Configuration
(log:config :daily (format nil "~A/acl2-mcp.log" (uiop:temporary-directory)))
