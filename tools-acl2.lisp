
(in-package #:acl2-mcp-bridge)

;; ACL2-specific MCP tools

(define-api (acl2-api :title "ACL2 MCP Tools"))

;;; ---------------------------------------------------------------------------
;;; Core Theorem Proving Tools
;;; ---------------------------------------------------------------------------

(define-tool (acl2-api check-theorem) (conjecture &optional hints)
  (:summary "Check if a conjecture is provable in ACL2")
  (:description "Test whether a theorem is provable using ACL2's thm command.
This does NOT admit the theorem to the world - use prove-theorem for that.
Use this for quick provability checks.")
  (:param conjecture string "Theorem statement as s-expression, e.g. \"(equal (+ x y) (+ y x))\"")
  (:param hints string "Proof hints (optional), e.g. \"((\\\"Goal\\\" :induct t))\"")
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-check-theorem conjecture :hints hints)
    (list (make-instance 'text-content 
                        :text (format nil "Provable: ~A~@[~%Error: ~A~]~@[~%Output:~%~A~]"
                                     success error-msg (when (plusp (length output)) output))))))

(define-tool (acl2-api prove-theorem) (conjecture &optional name hints rule-classes)
  (:summary "Prove and admit a theorem to ACL2")
  (:description "Prove a theorem and permanently add it to the ACL2 world using defthm.
The theorem can then be used as a lemma in future proofs.
Unlike check-theorem, this modifies the ACL2 logical world.")
  (:param conjecture string "Theorem statement as s-expression")
  (:param name string "Theorem name (optional, auto-generated if omitted)")
  (:param hints string "Proof hints (optional)")
  (:param rule-classes string "Rule classes (optional), e.g. \":rewrite\"")
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-prove-theorem conjecture :hints hints :rule-classes rule-classes)
    (list (make-instance 'text-content 
                        :text (format nil "Admitted: ~A~@[~%Error: ~A~]~@[~%Output:~%~A~]"
                                     success error-msg (when (plusp (length output)) output))))))

(define-tool (acl2-api admit) (event)
  (:summary "Admit an event (defun, defthm, etc) to ACL2")
  (:description "Submit an ACL2 event to be admitted to the logical world.
Supports defun, defthm, defmacro, encapsulate, include-book, and other ACL2 events.")
  (:param event string "ACL2 event as s-expression, e.g. \"(defun double (x) (* 2 x))\"")
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-admit event)
    (list (make-instance 'text-content 
                        :text (format nil "Admitted: ~A~@[~%Error: ~A~]~@[~%Output:~%~A~]"
                                     success error-msg (when (plusp (length output)) output))))))

(define-tool (acl2-api verify-guards) (function)
  (:summary "Verify guards for a function")
  (:description "Verify that a function's guard conditions are satisfied.
Guards are ACL2's type-like conditions that enable efficient execution.")
  (:param function string "Function name to verify guards for")
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-verify-guards function)
    (list (make-instance 'text-content 
                        :text (format nil "Guards verified: ~A~@[~%Error: ~A~]~@[~%Output:~%~A~]"
                                     success error-msg (when (plusp (length output)) output))))))

;;; ---------------------------------------------------------------------------
;;; Query and History Tools
;;; ---------------------------------------------------------------------------

(define-tool (acl2-api query-event) (name)
  (:summary "Query information about a named ACL2 event")
  (:description "Retrieve definition, guards, formals, and other properties of a 
function, theorem, or other named event from the ACL2 world.")
  (:param name string "Event name to query")
  (:result (soft-list-of text-content))
  (multiple-value-bind (info error-p error-msg)
      (acl2-get-event-info name)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content 
                            :text (format nil "~{~A: ~S~^~%~}" 
                                         (loop for (k . v) in info
                                               collect k collect v)))))))

(define-tool (acl2-api get-event-history) (&optional limit)
  (:summary "Retrieve proof/event history")
  (:description "Get a list of recent events (defun, defthm, etc) from the ACL2 world.
Useful for understanding the current logical context.")
  (:param limit string "Maximum events to return (optional, default 50)")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (acl2-get-event-history :limit (when limit (parse-integer limit :junk-allowed t)))
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content :text (format nil "~S" result))))))

(define-tool (acl2-api undo-to-point) (target)
  (:summary "Revert to earlier state")
  (:description "Undo ACL2 events back to a previous point. Use \"last\" to undo 
just the most recent event, or provide a command number.")
  (:param target string "\"last\" or a command number to undo to")
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-undo-to-point (if (string-equal target "last") 
                              :last 
                              (parse-integer target :junk-allowed t)))
    (list (make-instance 'text-content 
                        :text (format nil "Undone: ~A~@[~%Error: ~A~]~@[~%Output:~%~A~]"
                                     success error-msg (when (plusp (length output)) output))))))

;;; ---------------------------------------------------------------------------
;;; Book Management
;;; ---------------------------------------------------------------------------

(define-tool (acl2-api check-book) (book-path)
  (:summary "Validate/certify an ACL2 book")
  (:description "Certify an ACL2 book file. This checks all definitions and proofs 
in the book for correctness.")
  (:param book-path string "Path to the book (without .lisp extension)")
  (:result (soft-list-of text-content))
  (multiple-value-bind (success error-msg output)
      (acl2-check-book book-path)
    (list (make-instance 'text-content 
                        :text (format nil "Certified: ~A~@[~%Error: ~A~]~@[~%Output:~%~A~]"
                                     success error-msg (when (plusp (length output)) output))))))

;;; ---------------------------------------------------------------------------
;;; Session Management (for multi-client scenarios)
;;; ---------------------------------------------------------------------------

(define-tool (acl2-api list-sessions) ()
  (:summary "List all active ACL2 sessions")
  (:result (soft-list-of text-content))
  (let ((sessions (list-acl2-sessions)))
    (list (make-instance 'text-content :text (format nil "~S" sessions)))))

(define-tool (acl2-api start-session) ()
  (:summary "Start a new ACL2 session and return its id")
  (:result (soft-list-of text-content))
  (let ((id (start-acl2-session)))
    (list (make-instance 'text-content :text (format nil "~A" id)))))

(define-tool (acl2-api stop-session) (session-id)
  (:summary "Stop and remove an ACL2 session")
  (:param session-id string "Session identifier to stop")
  (:result (soft-list-of text-content))
  (if (stop-acl2-session session-id)
      (list (make-instance 'text-content :text (format nil "Stopped ~A" session-id)))
      (list (make-instance 'text-content :text (format nil "Session ~A not found" session-id)))))
