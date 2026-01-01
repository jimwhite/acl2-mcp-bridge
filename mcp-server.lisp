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
         (cl-define-function (read-from-string name)
                             (read-from-string lambda-list)
                             (read-from-string body))
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


(defun start-mcp-server (&key (transport :stdio) host port)
  "Start the MCP server using 40ants-mcp's start-server.

This properly exposes tools via MCP's tools/list and tools/call methods."
  (declare (ignore host))
  (log:info "Starting MCP server with transport ~A" transport)
  (40ants-mcp/server/definition:start-server 
   acl2-mcp-tools
   :transport transport
   :port port))

;; Note: 40ants-mcp has no public stop API; stopping requires terminating the process.
