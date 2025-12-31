(in-package #:acl2-mcp-bridge)

(defun register-acl2-tools ()
  "Register ACL2-specific MCP tools"
  
  ;; Check Theorem
  (40ants-mcp/tools:define-tool (acl2-bridge-tools check-theorem) 
      (file-path theorem-name &key session-id timeout)
    (:summary "Check if a specific theorem is valid")
    (:param file-path string "Path to ACL2 file")
    (:param theorem-name string "Name of theorem to check")
    (:param session-id (or null string) "Session ID for persistence")
    (:param timeout integer "Timeout in seconds (default 60)")
    (:result (soft-list-of text-content)
     (handler-case
       (let* ((timeout (or timeout 60))
              (result (check-theorem-impl file-path theorem-name session-id timeout)))
         (list (text-content
                (if (getf result :success)
                    (format nil "Theorem ~A verified successfully~%~A"
                           theorem-name
                           (getf result :output))
                    (format nil "Theorem ~A FAILED~%~A~%~A"
                           theorem-name
                           (getf result :output)
                           (getf result :error))))))
       (error (e)
        (list (text-content
               (format nil "Error checking theorem: ~A" e)))))))
  
  ;; Admit Event
  (40ants-mcp/tools:define-tool (acl2-bridge-tools admit)
      (code &key session-id timeout)
    (:summary "Admit an ACL2 event (function/theorem)")
    (:param code string "ACL2 code to admit")
    (:param session-id (or null string) "Session ID for persistence")
    (:param timeout integer "Timeout in seconds (default 30)")
    (:result (soft-list-of text-content)
     (handler-case
       (let* ((timeout (or timeout 30))
              (result (admit-impl code session-id timeout)))
         (list (text-content
                (if (getf result :success)
                    (format nil "Event admitted successfully~%~A"
                           (getf result :output))
                    (format nil "Admission FAILED~%~A"
                           (getf result :output)))))
       (error (e)
        (list (text-content
               (format nil "Error admitting event: ~A" e)))))))
  
  ;; Query Event
  (40ants-mcp/tools:define-tool (acl2-bridge-tools query-event)
      (name &key file-path session-id timeout)
    (:summary "Query an ACL2 event for its definition and properties")
    (:param name string "Event name to query")
    (:param file-path (or null string) "Optional file path")
    (:param session-id (or null string) "Session ID")
    (:param timeout integer "Timeout (default 30)")
    (:result (soft-list-of text-content)
     (handler-case
       (let* ((timeout (or timeout 30))
              (result (query-event-impl name file-path session-id timeout)))
         (list (text-content (getf result :definition))))
       (error (e)
        (list (text-content (format nil "Query failed: ~A" e)))))))
  
  ;; Session Management
  (40ants-mcp/tools:define-tool (acl2-bridge-tools start-session)
      (&key description)
    (:summary "Start a new persistent ACL2 session")
    (:param description (or null string) "Session description")
    (:result (soft-list-of text-content)
     (let ((session-id (create-session description)))
       (list (text-content
              (format nil "Session ~A created" session-id))))))
  
  (40ants-mcp/tools:define-tool (acl2-bridge-tools list-sessions)
      ()
    (:summary "List all active sessions")
    (:result (soft-list-of text-content)
     (let ((sessions (list-active-sessions)))
       (list (text-content
              (format nil "Active sessions:~%~{~A~%~}" sessions)))))))

(defun register-cl-tools ()
  "Register Common Lisp specific tools"
  
  (40ants-mcp/tools:define-tool (acl2-bridge-tools eval-cl)
      (code &key package session-id)
    (:summary "Evaluate Common Lisp code")
    (:param code string "Lisp code to evaluate")
    (:param package (or null string) "Package context (default :cl-user)")
    (:param session-id (or null string) "CL session ID")
    (:result (soft-list-of text-content)
     (handler-case
       (let* ((pkg (or (and package (find-package package)) :cl-user))
              (result (eval-cl-impl code pkg session-id)))
         (list (text-content
                (format nil "Result:~%~S~%~%Output:~%~A"
                       (getf result :value)
                       (getf result :output)))))
       (error (e)
        (list (text-content
               (format nil "CL Evaluation Error: ~A~%~A"
                      e
                      (with-output-to-string (s)
                        (print-backtrace e s))))))))))

(defun register-bridge-tools ()
  "Register cross-language bridge tools"
  
  (40ants-mcp/tools:define-tool (acl2-bridge-tools bridge-acl2-to-cl)
      (data &key acl2-session-id cl-session-id)
    (:summary "Transfer data from ACL2 to Common Lisp")
    (:param data string "ACL2 expression to transfer")
    (:param acl2-session-id string "Source ACL2 session")
    (:param cl-session-id string "Target CL session")
    (:result (soft-list-of text-content)
     (handler-case
       (let ((result (bridge-acl2-to-cl-impl data acl2-session-id cl-session-id)))
         (list (text-content result)))
       (error (e)
        (list (text-content (format nil "Bridge error: ~A" e)))))))
  
  (40ants-mcp/tools:define-tool (acl2-bridge-tools bridge-cl-to-acl2)
      (data &key cl-session-id acl2-session-id)
    (:summary "Transfer data from Common Lisp to ACL2")
    (:param data string "CL expression to transfer")
    (:param cl-session-id string "Source CL session")
    (:param acl2-session-id string "Target ACL2 session")
    (:result (soft-list-of text-content)
     (handler-case
       (let ((result (bridge-cl-to-acl2-impl data cl-session-id acl2-session-id)))
         (list (text-content result)))
       (error (e)
        (list (text-content (format nil "Bridge error: ~A" e))))))))
