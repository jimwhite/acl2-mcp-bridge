
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Session-Aware HTTP Transport (MCP Spec Compliant)
;;;
;;; Implements MCP session management per:
;;; https://modelcontextprotocol.io/specification/2025-11-25/basic/transports#session-management
;;;
;;; Key behaviors:
;;; - Server assigns session ID at initialization via MCP-Session-Id header
;;; - Client includes MCP-Session-Id in all subsequent requests  
;;; - HTTP DELETE terminates session
;;; - Each session gets isolated CL package
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Session-Aware HTTP Transport Class
;;; ---------------------------------------------------------------------------

(defclass session-http-transport (40ants-mcp/http-transport:http-transport)
  ()
  (:documentation "HTTP transport with MCP-compliant session management.
Each unique session ID gets its own isolated CL evaluation context."))

;;; ---------------------------------------------------------------------------
;;; Session ID Generation
;;; ---------------------------------------------------------------------------

(defun generate-session-id ()
  "Generate a cryptographically-suitable session ID."
  ;; Use UUID-style format: random hex with dashes
  (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (random (expt 16 8))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 12))))

;;; ---------------------------------------------------------------------------
;;; HTTP Header Session Extraction
;;; ---------------------------------------------------------------------------

(defun get-session-id-from-headers (env)
  "Extract MCP-Session-Id from request headers (Lack/Clack ENV)."
  (let ((headers (getf env :headers)))
    (when headers
      (gethash "mcp-session-id" headers))))

(defun is-initialize-request-p (body-string)
  "Check if BODY-STRING is an MCP initialize request."
  (handler-case
      (let* ((json (yason:parse body-string))
             (method (gethash "method" json)))
        (string= method "initialize"))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Custom Lack App with Session Management
;;; ---------------------------------------------------------------------------

(defun make-session-lack-app (transport message-handler)
  "Create a Lack app that handles MCP sessions per spec."
  (lambda (env)
    (let* ((request-method (getf env :request-method))
           (content-length (getf env :content-length))
           (body-stream (getf env :raw-body))
           (session-id (get-session-id-from-headers env)))
      
      ;; Handle DELETE = session termination
      (when (eq request-method :delete)
        (if session-id
            (progn
              (destroy-session session-id)
              (return-from make-session-lack-app
                '(200 (:content-type "application/json") ("{}"))))
            (return-from make-session-lack-app
              '(400 (:content-type "application/json") 
                ("{\"error\":\"Missing MCP-Session-Id\"}")))))
      
      ;; For POST, read body and process
      (when (eq request-method :post)
        (let* ((body-bytes (when (and body-stream content-length)
                            (let ((buf (make-array content-length 
                                                   :element-type '(unsigned-byte 8))))
                              (read-sequence buf body-stream)
                              buf)))
               (body-string (when body-bytes
                             (babel:octets-to-string body-bytes :encoding :utf-8)))
               (is-init (and body-string (is-initialize-request-p body-string)))
               ;; For initialize: generate new session ID
               ;; Otherwise: use provided session ID or reject
               (effective-session-id 
                 (cond
                   (is-init (generate-session-id))
                   (session-id session-id)
                   (t nil))))
          
          ;; Reject non-init requests without session ID
          (when (and (not is-init) (not effective-session-id))
            (return-from make-session-lack-app
              '(400 (:content-type "application/json")
                ("{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Missing MCP-Session-Id header\"}}"))))
          
          ;; Check if session exists (for non-init requests)
          (when (and (not is-init) 
                     (not (get-session effective-session-id)))
            (return-from make-session-lack-app
              '(404 (:content-type "application/json")
                ("{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Session not found\"}}"))))
          
          ;; Process request with session binding
          (let* ((*current-session* (get-or-create-session effective-session-id))
                 (response (funcall message-handler body-string))
                 (response-headers 
                   (if is-init
                       ;; Include session ID in init response
                       (list :content-type "application/json"
                             :mcp-session-id effective-session-id)
                       (list :content-type "application/json"))))
            (return-from make-session-lack-app
              (list 200 response-headers (list response))))))
      
      ;; GET for SSE stream (not yet implemented)
      (when (eq request-method :get)
        (return-from make-session-lack-app
          '(405 (:content-type "application/json")
            ("{\"error\":\"SSE not implemented\"}"))))
      
      ;; Unknown method
      '(405 (:content-type "application/json")
        ("{\"error\":\"Method not allowed\"}")))))

;;; ---------------------------------------------------------------------------
;;; Override start-loop to use our session-aware app
;;; ---------------------------------------------------------------------------

(defmethod 40ants-mcp/transport/base:start-loop 
    ((transport session-http-transport) message-handler)
  "Start HTTP server with MCP-compliant session management."
  (log:info "Starting session-aware HTTP transport on port ~A" 
            (40ants-mcp/http-transport:transport-port transport))
  
  (let ((session-app (make-session-lack-app transport message-handler)))
    ;; Override the lack-app with our session-aware version
    (setf (40ants-mcp/http-transport:transport-server transport)
          (clack:clackup session-app
                         :server :hunchentoot
                         :port (40ants-mcp/http-transport:transport-port transport)
                         :use-thread t))))
