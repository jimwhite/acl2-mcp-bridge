
(in-package #:acl2-mcp-bridge)


(defparameter *bridge-server* nil)
(defparameter *mcp-server* nil)

(defun start-server (&key (protocol :bridge) (transport :stdio) port host acl2-path)
  "Start the server with specified protocol.

   :protocol :bridge   - ACL2 Bridge protocol (TCP, port 13721)
   :protocol :mcp      - Model Context Protocol

   :transport :stdio   - Standard input/output (for MCP)
   :transport :http    - HTTP server (for MCP, requires :port)

   :port               - Port number (for Bridge or MCP HTTP)
   :host               - Host binding (HTTP transport only)
   :acl2-path          - Optional ACL2 executable path"
  (case protocol
    (:bridge
     (setf *bridge-server*
       (start-bridge-server :port (or port *bridge-port*)))
     (log:info "ACL2 Bridge server started on port ~A" (or port *bridge-port*))
     *bridge-server*)
    (:mcp
     (initialize-acl2-interface acl2-path)
     (log:info "MCP server starting via 40ants-mcp (~A transport)" transport)
     (setf *mcp-server*
       (start-mcp-server :transport transport :host host :port port))
     *mcp-server*)
    (otherwise
     (error "Unknown protocol: ~A" protocol))))

(defun start-both (&key (bridge-port *bridge-port*) (mcp-transport :stdio) host acl2-path)
  "Start both Bridge and MCP servers (multi-protocol support).

Returns two values: bridge server and MCP server."
  (let ((bridge (start-server :protocol :bridge :port bridge-port))
        (mcp (start-server :protocol :mcp :transport mcp-transport :host host :acl2-path acl2-path)))
    (log:info "Both Bridge and MCP servers running")
    (values bridge mcp)))

(defun stop-server (&key (protocol :all))
  "Stop the server(s).

   :protocol :bridge - Stop only Bridge
   :protocol :mcp    - Stop only MCP
   :protocol :all    - Stop both"
  (case protocol
    (:bridge
     (when *bridge-server*
       (usocket:socket-close *bridge-server*)
       (setf *bridge-server* nil)))
    (:mcp
     (when *mcp-server*
       (stop-mcp-server *mcp-server*)
       (setf *mcp-server* nil)))
    (:all
     (stop-server :protocol :bridge)
     (stop-server :protocol :mcp))))

(defun status ()
  "Print server status."
  (format t "Bridge Server: ~A~%" (if *bridge-server* "RUNNING" "STOPPED"))
  (format t "MCP Server: ~A~%" (if *mcp-server* "RUNNING" "STOPPED"))
  (format t "ACL2 Sessions: ~D~%" (hash-table-count *acl2-sessions*))
  (format t "CL Sessions: ~D~%" (hash-table-count *cl-sessions*)))
