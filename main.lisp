
(in-package #:acl2-mcp-bridge)


(defparameter *bridge-server* nil)
(defparameter *mcp-server* nil)

(defun start-server (&key (protocol :bridge) (transport :http) port host socket-path acl2-path
                          (main-thread-loop t))
  "Start the server with specified protocol.

   :protocol :bridge   - ACL2 Bridge protocol (TCP, default port 55433)
   :protocol :mcp      - Model Context Protocol

   :transport :http    - HTTP server (for MCP)
   
   Note: stdio transport is NOT supported because ACL2's startup banners
   and prompts would corrupt the JSON-RPC stream. Use the Python stdio
   wrapper (acl2-mcp-stdio.py) instead.

   :port               - Port number (for Bridge or MCP HTTP over TCP)
   :socket-path        - Unix socket path (for MCP HTTP over Unix socket)
   :host               - Host binding (HTTP transport only)
   :acl2-path          - Ignored (we run inside ACL2)
   :main-thread-loop   - If T (default), enter main thread loop after starting server.
                         This blocks forever but allows safe ACL2 state operations.
                         Set to NIL for testing or if you need control to return."
  (declare (ignore acl2-path))
  (case protocol
    (:bridge
     (setf *bridge-server*
       (start-bridge-server :port (or port *bridge-port*)))
     (log:info "ACL2 Bridge server started on port ~A" (or port *bridge-port*))
     ;; Bridge protocol already handles main thread via its own start-fn
     *bridge-server*)
    (:mcp
     (when (eq transport :stdio)
       (error "stdio transport not supported - use acl2-mcp-stdio.py wrapper instead"))
     (initialize-acl2-interface)
     (log:info "MCP server starting via 40ants-mcp (~A transport)" transport)
     (setf *mcp-server*
       (start-mcp-server :transport transport :host host :port port :socket-path socket-path))
     ;; Enter main thread loop so ACL2 operations can be delegated safely
     (when main-thread-loop
       (log:info "Entering main thread loop for ACL2 operation delegation")
       (bridge::main-thread-loop))
     *mcp-server*)
    (otherwise
     (error "Unknown protocol: ~A" protocol))))

(defun start-both (&key (bridge-port *bridge-port*) (mcp-port 8085) host)
  "Start both Bridge and MCP servers (multi-protocol support).

Returns two values: bridge server and MCP server."
  (let ((bridge (start-server :protocol :bridge :port bridge-port :main-thread-loop nil))
        (mcp (start-server :protocol :mcp :transport :http :host host :port mcp-port :main-thread-loop nil)))
    (log:info "Both Bridge and MCP servers running")
    ;; Enter main thread loop
    (bridge::main-thread-loop)
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
       ;; 40ants-mcp does not expose a stop API; require process restart.
       (error "MCP stop not supported; restart the process to stop MCP")))
    (:all
     (stop-server :protocol :bridge)
     (stop-server :protocol :mcp))))

(defun status ()
  "Print server status."
  (format t "Bridge Server: ~A~%" (if *bridge-server* "RUNNING" "STOPPED"))
  (format t "MCP Server: ~A~%" (if *mcp-server* "RUNNING" "STOPPED"))
  (format t "ACL2 Sessions: ~D~%" (hash-table-count *acl2-sessions*))
  (format t "CL Sessions: ~D~%" (hash-table-count *sessions*)))
