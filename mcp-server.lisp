(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; MCP Server with CL Tools
;;;
;;; Based on the ACL2 Bridge model: each MCP connection gets a worker thread 
;;; that provides a read-eval-print loop. Per ACL2 Bridge docs: "When a client 
;;; connects, it creates a new worker thread to handle the client's requests."
;;;
;;; The MCP server provides session isolation at the transport level.
;;; Clients needing separate environments spawn multiple server instances.
;;; ============================================================================

(define-api (acl2-mcp-tools :title "ACL2 MCP Bridge Tools"))

;;; ---------------------------------------------------------------------------
;;; Common Lisp REPL Tools
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools eval-cl) (code)
  (:summary "Evaluate Common Lisp code")
  (:description "Evaluate one or more Common Lisp forms. Results are returned as 
printed representations. Multiple values are separated by newlines. The evaluation
context persists across calls (defined functions, variables, current package).")
  (:param code string "Lisp code to evaluate")
  (:result (soft-list-of text-content))
  (multiple-value-bind (results error-p error-msg)
      (cl-eval code)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content 
                            :text (cond
                                   ((null results) "NIL")
                                   ((null (cdr results)) (format nil "~S" (car results)))
                                   (t (format nil "~{~S~^~%~}" results))))))))

(define-tool (acl2-mcp-tools load-file) (path)
  (:summary "Load a Common Lisp source file")
  (:description "Load and evaluate all forms in a Lisp source file. The file's 
definitions persist in the evaluation context.")
  (:param path string "Path to the .lisp file to load")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (cl-load-file path)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content :text (format nil "Loaded: ~A" path))))))

(define-tool (acl2-mcp-tools define-function) (name lambda-list body)
  (:summary "Define a Common Lisp function")
  (:description "Define a new function in the evaluation context. Equivalent to 
(defun name lambda-list body). The function persists and can be called later.")
  (:param name string "Function name (as a string)")
  (:param lambda-list string "Parameter list, e.g. \"(x y)\" or \"(n &optional (base 10))\"")
  (:param body string "Function body expression")
  (:result (soft-list-of text-content))
  ;; Validate inputs before attempting to read
  (cond
    ((or (null name) (string= name ""))
     (list (make-instance 'text-content :text "Error: Function name cannot be empty")))
    ((or (null lambda-list) (string= lambda-list ""))
     (list (make-instance 'text-content :text "Error: Lambda list cannot be empty (use \"()\" for no parameters)")))
    ((or (null body) (string= body ""))
     (list (make-instance 'text-content :text "Error: Function body cannot be empty")))
    (t
     (multiple-value-bind (result error-p error-msg)
         (cl-define-function name lambda-list body)  ; Pass strings, read in session context
       (if error-p
           (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
           (list (make-instance 'text-content :text (format nil "Defined: ~A" result))))))))

(define-tool (acl2-mcp-tools get-package) ()
  (:summary "Get current evaluation package")
  (:description "Returns the name of the current package used for symbol resolution
during evaluation.")
  (:result (soft-list-of text-content))
  (multiple-value-bind (pkg-name error-p error-msg)
      (cl-get-package)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content :text pkg-name)))))

(define-tool (acl2-mcp-tools reset-cl) ()
  (:summary "Reset Common Lisp evaluation context")
  (:description "Reset the evaluation context to initial state: unbind all 
user-defined functions and variables, reset package to CL-USER. Use this to 
start fresh without restarting the server.")
  (:result (soft-list-of text-content))
  (multiple-value-bind (msg error-p error-msg)
      (cl-reset)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content :text msg)))))

(define-tool (acl2-mcp-tools query-cl-package) (&key package-name)
  (:summary "Introspect a Common Lisp package")
  (:description "List symbols, functions, and variables in a package. Without arguments,
introspects the current session package. Useful for exploring available definitions.")
  (:param package-name string "Package name to introspect (optional, defaults to current)")
  (:result (soft-list-of text-content))
  (multiple-value-bind (info error-p error-msg)
      (cl-query-package package-name)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content :text info)))))

;; Note: Session termination is handled via HTTP DELETE per MCP spec, not as a tool.
;; See: https://modelcontextprotocol.io/specification/2025-11-25/basic/transports#session-management


(defun start-mcp-server (&key (transport :stdio) host port socket-path)
  "Start the MCP server with per-client session support.

Uses session-http-transport for HTTP to provide isolated sessions per client.
Each client (identified by MCP session ID) gets its own CL evaluation context.

:transport :http        - HTTP transport (TCP)
:transport :unix-socket - Raw JSON-RPC over Unix socket (for stdio wrapper)
:port                   - TCP port (for HTTP over TCP)
:socket-path            - Unix socket path"
  (declare (ignore host))
  (log:info "Starting MCP server with transport ~A" transport)
  
  (case transport
    (:http
     ;; Use our custom session-aware transport
     ;; We access internal symbols because 40ants-mcp doesn't export them
     (let* ((init-fn (symbol-function 
                      (find-symbol "INITIALIZE-RPC-SERVER" 
                                   (find-package "40ANTS-MCP/SERVER/DEFINITION"))))
            (handle-fn (symbol-function 
                        (find-symbol "HANDLE-MESSAGE" 
                                     (find-package "40ANTS-MCP/SERVER/DEFINITION"))))
            (rpc-server (funcall init-fn acl2-mcp-tools))
            (transport-obj (make-instance 'session-http-transport :port port)))
       (start-loop transport-obj
                   (lambda (message)
                     (funcall handle-fn rpc-server message)))))
    (:unix-socket
     ;; Raw JSON-RPC over Unix socket (same protocol as stdio)
     (unless socket-path
       (error "socket-path required for :unix-socket transport"))
     (let* ((init-fn (symbol-function 
                      (find-symbol "INITIALIZE-RPC-SERVER" 
                                   (find-package "40ANTS-MCP/SERVER/DEFINITION"))))
            (handle-fn (symbol-function 
                        (find-symbol "HANDLE-MESSAGE" 
                                     (find-package "40ANTS-MCP/SERVER/DEFINITION"))))
            (rpc-server (funcall init-fn acl2-mcp-tools)))
       (start-unix-socket-jsonrpc-server socket-path
                                         (lambda (message)
                                           (funcall handle-fn rpc-server message)))))
    (:stdio
     ;; Use standard 40ants-mcp for stdio (not recommended for ACL2)
     (40ants-mcp/server/definition:start-server 
      acl2-mcp-tools
      :transport :stdio))
    (t
     (error "Unknown transport: ~A" transport))))

;; Note: 40ants-mcp has no public stop API; stopping requires terminating the process.
