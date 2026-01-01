
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Session-Aware HTTP Transport
;;;
;;; Wraps 40ants-mcp's HTTP transport to provide per-client session isolation.
;;; Each MCP client gets its own CL session based on session ID in requests.
;;;
;;; Per ACL2 Bridge model: "When a client connects, it creates a new worker 
;;; thread to handle the client's requests."
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Session-Aware HTTP Transport Class
;;; ---------------------------------------------------------------------------

(defclass session-http-transport (40ants-mcp/http-transport:http-transport)
  ()
  (:documentation "HTTP transport that creates per-client sessions.
Each unique session ID gets its own isolated CL evaluation context."))

;;; ---------------------------------------------------------------------------
;;; Session ID Extraction from MCP Requests
;;; ---------------------------------------------------------------------------

(defun extract-session-id-from-json (json-string)
  "Extract session ID from MCP JSON-RPC request.
MCP protocol uses _meta.sessionId for session identification.
Returns session ID string or generates one from client info."
  (handler-case
      (let* ((request (yason:parse json-string))
             ;; Try _meta.sessionId first (MCP standard)
             (meta (gethash "_meta" request))
             (session-id (when (hash-table-p meta)
                          (gethash "sessionId" meta))))
        (or session-id
            ;; Fall back to request id as pseudo-session for simple clients
            (let ((id (gethash "id" request)))
              (when id (format nil "request-~A" id)))
            ;; Ultimate fallback
            "default"))
    (error (e)
      (declare (ignore e))
      "default")))

;;; ---------------------------------------------------------------------------
;;; Custom Request Handler with Session Binding
;;; ---------------------------------------------------------------------------

(defun make-session-aware-handler (original-handler)
  "Wrap ORIGINAL-HANDLER to bind *current-session* based on request session ID."
  (lambda (body-string)
    (let* ((session-id (extract-session-id-from-json body-string))
           (*current-session* (get-or-create-session session-id)))
      (log:debug "Processing request for session ~A" session-id)
      (funcall original-handler body-string))))

;;; ---------------------------------------------------------------------------
;;; Override start-loop to inject session handling
;;; ---------------------------------------------------------------------------

(defmethod 40ants-mcp/transport/base:start-loop 
    ((transport session-http-transport) message-handler)
  "Start HTTP server with session-aware request handling."
  (log:info "Starting session-aware HTTP transport on port ~A" 
            (40ants-mcp/http-transport:transport-port transport))
  
  ;; Wrap the message handler with session binding
  (let ((session-aware-handler (make-session-aware-handler message-handler)))
    (setf (40ants-mcp/http-transport:transport-message-handler transport) 
          session-aware-handler)
    
    ;; Start the server (from parent class logic)
    ;; Run in thread to not block the caller
    (setf (40ants-mcp/http-transport:transport-server transport)
          (clack:clackup (40ants-mcp/http-transport:transport-lack-app transport)
                         :server :hunchentoot
                         :port (40ants-mcp/http-transport:transport-port transport)
                         :use-thread t))))
