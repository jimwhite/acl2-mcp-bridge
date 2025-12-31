
(in-package #:acl2-mcp-40ants)

(defparameter *server* nil)

(defun start-server (&key (transport *default-transport*) port)
  (setf *server*
        (40ants-mcp/server/definition:start-server
         (list #'acl2-api #'cl-api #'bridge-api)
         :transport transport
         :port (or port *default-port*)))
  *server*)

(defun stop-server ()
  (when *server*
    (40ants-mcp/server/definition:stop-server *server*)
    (setf *server* nil)))
