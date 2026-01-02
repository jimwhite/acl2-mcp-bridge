
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
  ((socket-path :initarg :socket-path :initform nil :accessor transport-socket-path
                :documentation "Unix socket path for stdio wrapper mode, or NIL for TCP.")
   (server-socket :initform nil :accessor transport-server-socket
                  :documentation "Unix socket server instance.")
   (running :initform nil :accessor transport-running
            :documentation "Running flag for Unix socket server."))
  (:documentation "HTTP transport with MCP-compliant session management.
Each unique session ID gets its own isolated CL evaluation context.
Supports both TCP (via Clack) and Unix socket modes."))

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

(defun get-connection-id (env)
  "Get a unique connection identifier from remote addr:port.
For Streamable HTTP, each connection maintains its own session."
  (let ((addr (getf env :remote-addr))
        (port (getf env :remote-port)))
    (when (and addr port)
      (format nil "conn-~A:~A" addr port))))

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
  (declare (ignore transport))
  (lambda (env)
    (block lack-app
      (let* ((request-method (getf env :request-method))
             (content-length (getf env :content-length))
             (body-stream (getf env :raw-body))
             (session-id (get-session-id-from-headers env))
             (connection-id (get-connection-id env)))
        
        ;; Handle DELETE = session termination
        (when (eq request-method :delete)
          (let ((id-to-delete (or session-id connection-id)))
            (if id-to-delete
                (progn
                  (destroy-session id-to-delete)
                  (return-from lack-app
                    '(200 (:content-type "application/json") ("{}"))))
                (return-from lack-app
                  '(400 (:content-type "application/json") 
                    ("{\"error\":\"Cannot identify session\"}"))))))
        
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
                 ;; Session ID priority:
                 ;; 1. Explicit MCP-Session-Id header (spec-compliant clients)
                 ;; 2. Connection-based ID (VS Code Streamable HTTP)
                 ;; 3. Generate new ID for initialize requests
                 (effective-session-id 
                   (cond
                     (session-id session-id)  ; Explicit header takes priority
                     (connection-id connection-id)  ; Use connection for VS Code
                     (is-init (generate-session-id))  ; New session on init
                     (t nil))))
            
            ;; Only reject if we truly can't identify the session
            (when (not effective-session-id)
              (return-from lack-app
                '(400 (:content-type "application/json")
                  ("{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Cannot identify session\"}}"))))
            
            ;; Check if explicit session ID exists (for non-init requests with header)
            (when (and (not is-init) 
                       session-id  ; Only check for explicit header sessions
                       (not (get-session session-id)))
              (return-from lack-app
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
              (return-from lack-app
                (list 200 response-headers (list response))))))
        
        ;; GET for SSE stream (not yet implemented)
        (when (eq request-method :get)
          (return-from lack-app
            '(405 (:content-type "application/json")
              ("{\"error\":\"SSE not implemented\"}"))))
        
        ;; Unknown method
        '(405 (:content-type "application/json")
          ("{\"error\":\"Method not allowed\"}"))))))

;;; ---------------------------------------------------------------------------
;;; Override start-loop to use our session-aware app
;;; ---------------------------------------------------------------------------

(defmethod 40ants-mcp/transport/base:start-loop 
    ((transport session-http-transport) message-handler)
  "Start HTTP server with MCP-compliant session management."
  (let ((port (40ants-mcp/http-transport:transport-port transport))
        (socket-path (transport-socket-path transport)))
    (if socket-path
        ;; Unix socket mode - use our custom HTTP server
        (start-unix-socket-http-server socket-path message-handler transport)
        ;; TCP mode - use clack/hunchentoot
        (progn
          (log:info "Starting session-aware HTTP transport on port ~A" port)
          (let ((session-app (make-session-lack-app transport message-handler)))
            (setf (40ants-mcp/http-transport:transport-server transport)
                  (clack:clackup session-app
                                 :server :hunchentoot
                                 :port port
                                 :use-thread t)))))))

;;; ---------------------------------------------------------------------------
;;; Unix Socket HTTP Server (for stdio wrapper)
;;; ---------------------------------------------------------------------------

(defclass unix-socket-http-transport (session-http-transport)
  ((server-socket :initform nil :accessor transport-server-socket)
   (running :initform nil :accessor transport-running))
  (:documentation "HTTP transport over Unix domain socket.
Inherits socket-path from session-http-transport."))

(defun parse-http-request (stream)
  "Parse HTTP request from stream. Returns (method path headers body)."
  (let* ((request-line (read-line stream nil nil))
         (parts (when request-line (uiop:split-string request-line :separator " ")))
         (method (first parts))
         (path (second parts))
         (headers (make-hash-table :test 'equal))
         (content-length 0))
    ;; Read headers
    (loop for line = (read-line stream nil nil)
          while (and line (> (length line) 1))  ; Stop at empty line
          do (let ((colon-pos (position #\: line)))
               (when colon-pos
                 (let ((key (string-downcase (subseq line 0 colon-pos)))
                       (value (string-trim '(#\Space #\Return) 
                                          (subseq line (1+ colon-pos)))))
                   (setf (gethash key headers) value)
                   (when (string= key "content-length")
                     (setf content-length (parse-integer value :junk-allowed t)))))))
    ;; Read body
    (let ((body (when (> content-length 0)
                  (let ((buf (make-string content-length)))
                    (read-sequence buf stream)
                    buf))))
      (values method path headers body))))

(defun write-http-response (stream status headers body)
  "Write HTTP response to stream."
  (format stream "HTTP/1.1 ~A ~A~C~C" 
          status 
          (case status (200 "OK") (400 "Bad Request") (404 "Not Found") (405 "Method Not Allowed") (t "Error"))
          #\Return #\Newline)
  ;; Write headers
  (loop for (key value) on headers by #'cddr
        do (format stream "~A: ~A~C~C" key value #\Return #\Newline))
  (format stream "Content-Length: ~A~C~C" (length body) #\Return #\Newline)
  (format stream "~C~C" #\Return #\Newline)
  (write-string body stream)
  (force-output stream))

(defun handle-unix-http-connection (client-stream message-handler)
  "Handle a single HTTP connection on Unix socket."
  (handler-case
      (multiple-value-bind (method path headers body)
          (parse-http-request client-stream)
        (declare (ignore path))
        (cond
          ;; POST - handle JSON-RPC message
          ((string-equal method "POST")
           (let* ((session-id (or (gethash "mcp-session-id" headers)
                                  (format nil "unix-~A" (random 1000000))))
                  (is-init (is-initialize-request-p body))
                  (*current-session* (get-or-create-session session-id)))
             ;; Process message
             (let ((response (funcall message-handler body)))
               (write-http-response client-stream 200
                                    (if is-init
                                        (list "Content-Type" "application/json"
                                              "MCP-Session-Id" session-id)
                                        (list "Content-Type" "application/json"))
                                    response))))
          ;; DELETE - end session
          ((string-equal method "DELETE")
           (let ((session-id (gethash "mcp-session-id" headers)))
             (when session-id
               (destroy-session session-id))
             (write-http-response client-stream 200
                                  '("Content-Type" "application/json")
                                  "{\"status\":\"session terminated\"}")))
          ;; Other methods
          (t
           (write-http-response client-stream 405
                                '("Content-Type" "application/json")
                                "{\"error\":\"Method not allowed\"}"))))
    (error (e)
      (log:error "Error handling connection: ~A" e)
      (ignore-errors
        (write-http-response client-stream 500
                             '("Content-Type" "application/json")
                             (format nil "{\"error\":\"~A\"}" e))))))

(defun start-unix-socket-http-server (socket-path message-handler transport)
  "Start HTTP server on Unix domain socket."
  (log:info "Starting MCP HTTP server on Unix socket: ~A" socket-path)
  
  ;; Remove existing socket file
  (when (probe-file socket-path)
    (log:info "Removing existing socket file")
    (delete-file socket-path))
  
  ;; Create socket using our bridge primitives
  (log:info "Creating Unix socket...")
  (let ((server-socket (bridge::ccl-make-socket-unix socket-path)))
    (log:info "Unix socket created: ~A, file exists: ~A" server-socket (probe-file socket-path))
    (setf (transport-server-socket transport) server-socket)
    (setf (transport-running transport) t)
    
    ;; Accept loop in background thread
    (bt:make-thread
     (lambda ()
       (unwind-protect
           (loop while (transport-running transport)
                 do (handler-case
                        (let ((client-stream (bridge::ccl-accept-connection server-socket)))
                          (unwind-protect
                              (handle-unix-http-connection client-stream message-handler)
                            (close client-stream)))
                      (error (e)
                        (unless (transport-running transport)
                          (return))
                        (log:error "Accept error: ~A" e))))
         ;; Cleanup
         (ignore-errors (sb-bsd-sockets:socket-close server-socket))
         (ignore-errors (delete-file socket-path))))
     :name "MCP Unix Socket Server")))

;;; ---------------------------------------------------------------------------
;;; Unix Socket JSON-RPC Server (raw protocol, same as stdio)
;;; ---------------------------------------------------------------------------

(defun read-jsonrpc-message (stream)
  "Read a JSON-RPC message from stream.
   Supports both Content-Length framed messages and raw JSON (newline-delimited)."
  (format *error-output* "~&[read-jsonrpc] starting read...~%")
  (force-output *error-output*)
  
  ;; Peek at first character to determine format
  (let ((first-char (peek-char nil stream nil nil)))
    (format *error-output* "~&[read-jsonrpc] first char: ~S~%" first-char)
    (force-output *error-output*)
    
    (cond
      ;; Raw JSON - starts with {
      ((eql first-char #\{)
       (format *error-output* "~&[read-jsonrpc] detected raw JSON mode~%")
       (force-output *error-output*)
       ;; Read until we have balanced braces
       (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
             (brace-count 0)
             (in-string nil)
             (escape-next nil))
         (loop for ch = (read-char stream nil nil)
               while ch
               do (vector-push-extend ch result)
                  (cond
                    (escape-next (setf escape-next nil))
                    ((eql ch #\\) (setf escape-next t))
                    ((eql ch #\") (setf in-string (not in-string)))
                    ((and (not in-string) (eql ch #\{)) (incf brace-count))
                    ((and (not in-string) (eql ch #\})) (decf brace-count)))
               until (and (not in-string) (zerop brace-count)))
         ;; Consume trailing whitespace (newlines between JSON messages)
         (loop for next-char = (peek-char nil stream nil nil)
               while (and next-char (member next-char '(#\Newline #\Return #\Space #\Tab)))
               do (read-char stream nil nil))
         (format *error-output* "~&[read-jsonrpc] read ~D chars of JSON~%" (length result))
         (force-output *error-output*)
         (coerce result 'string)))
      
      ;; Content-Length framed
      ((and first-char (alpha-char-p first-char))
       (format *error-output* "~&[read-jsonrpc] detected Content-Length mode~%")
       (force-output *error-output*)
       (let ((content-length nil))
         ;; Read headers until blank line
         (loop for line = (read-line stream nil nil)
               while (and line 
                          (> (length line) 0) 
                          (not (every (lambda (c) (member c '(#\Return #\Newline #\Space))) line)))
               do (let ((trimmed (string-trim '(#\Return #\Newline #\Space) line)))
                    (when (and (> (length trimmed) 15)
                               (string-equal "content-length:" (subseq trimmed 0 15)))
                      (setf content-length 
                            (parse-integer (subseq trimmed 15) :junk-allowed t)))))
         (format *error-output* "~&[read-jsonrpc] content-length: ~A~%" content-length)
         (force-output *error-output*)
         (when content-length
           (let ((buf (make-string content-length)))
             (read-sequence buf stream)
             buf))))
      
      ;; EOF or error
      (t
       (format *error-output* "~&[read-jsonrpc] EOF or no data~%")
       (force-output *error-output*)
       nil))))

(defun write-jsonrpc-message (stream message &key (format :newline))
  "Write a JSON-RPC message to stream.
   :format :newline - Simple newline-delimited (for stdio)
   :format :content-length - Content-Length framed (for HTTP body)"
  (case format
    (:newline
     ;; Simple newline-delimited JSON (what MCP stdio uses)
     (write-string message stream)
     (write-char #\Newline stream)
     (force-output stream))
    (:content-length
     ;; Content-Length framed (for HTTP)
     (format stream "Content-Length: ~A~C~C~C~C~A" 
             (length message) #\Return #\Newline #\Return #\Newline message)
     (force-output stream))))

(defun handle-unix-jsonrpc-connection (client-stream message-handler)
  "Handle a JSON-RPC connection on Unix socket (same protocol as stdio)."
  (format *error-output* "~&[handle-connection] starting~%")
  (force-output *error-output*)
  (let ((*current-session* (get-or-create-session "unix-socket")))
    (loop
      (handler-case
          (progn
            (format *error-output* "~&[handle-connection] reading message...~%")
            (force-output *error-output*)
            (let ((message (read-jsonrpc-message client-stream)))
              (format *error-output* "~&[handle-connection] got: ~A~%" 
                      (if message (subseq message 0 (min 80 (length message))) "NIL"))
              (force-output *error-output*)
              (unless message
                (return))  ; EOF
              (let ((response (funcall message-handler message)))
                (format *error-output* "~&[handle-connection] response: ~A~%"
                        (if response (subseq response 0 (min 80 (length response))) "NIL"))
                (force-output *error-output*)
                (when response
                  (write-jsonrpc-message client-stream response)))))
        (end-of-file () (return))
        (error (e)
          (format *error-output* "~&[handle-connection] ERROR: ~A~%" e)
          (force-output *error-output*)
          (return))))))

(defun start-unix-socket-jsonrpc-server (socket-path message-handler)
  "Start JSON-RPC server on Unix domain socket (same protocol as stdio).
   Blocks and handles connections directly - no threading needed."
  (format *error-output* "~&[start-server] Starting on ~A~%" socket-path)
  (force-output *error-output*)
  
  ;; Remove existing socket file
  (when (probe-file socket-path)
    (delete-file socket-path))
  
  ;; Create socket
  (let ((server-socket (bridge::ccl-make-socket-unix socket-path)))
    (format *error-output* "~&[start-server] Socket ready, accepting...~%")
    (force-output *error-output*)
    
    ;; Accept and handle connection directly (single client for stdio wrapper)
    (unwind-protect
        (let ((client-stream (bridge::ccl-accept-connection server-socket)))
          (format *error-output* "~&[start-server] Client connected!~%")
          (force-output *error-output*)
          (unwind-protect
              (handle-unix-jsonrpc-connection client-stream message-handler)
            (close client-stream)))
      ;; Cleanup
      (ignore-errors (sb-bsd-sockets:socket-close server-socket))
      (ignore-errors (delete-file socket-path)))))
