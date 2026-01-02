
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; ACL2 MCP Tools
;;;
;;; These tools provide ACL2 theorem proving capabilities via MCP.
;;; They use the same API (acl2-mcp-tools) as the CL tools in mcp-server.lisp.
;;;
;;; Session management is handled per-connection by the transport layer -
;;; each MCP client gets its own isolated evaluation context automatically.
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; ACL2 Evaluation
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-evaluate) (code)
  (:summary "Evaluate ACL2 expressions or define functions")
  (:description "Evaluate ACL2 expressions or define functions (defun). Use this for:
1) Defining functions: (defun factorial (n) (if (zp n) 1 (* n (factorial (- n 1)))))
2) Computing values: (+ 1 2 3) or (append '(a b) '(c d))
3) Testing expressions before proving theorems about them
Results persist in the ACL2 logical world for this session.")
  (:param code string "ACL2 code to evaluate")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval code)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (list (make-instance 'text-content :text (format nil "~S" result))))))

;;; ---------------------------------------------------------------------------
;;; Theorem Proving Tools
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-prove) (code)
  (:summary "Submit an ACL2 theorem (defthm) for proof")
  (:description "Prove a theorem and permanently add it to the ACL2 world.
The theorem can then be used as a lemma in future proofs.
Example: (defthm append-nil (implies (true-listp x) (equal (append x nil) x)))
Returns detailed ACL2 proof output showing the proof steps.")
  (:param code string "ACL2 defthm form to prove")
  (:result (soft-list-of text-content))
  ;; Use acl2-eval which works
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval code)
    (if error-p
        (list (make-instance 'text-content 
                            :text (format nil "Proof FAILED: ~A" error-msg)))
        (list (make-instance 'text-content 
                            :text (format nil "Theorem proved and admitted: ~S" result))))))

(define-tool (acl2-mcp-tools acl2-check-provable) (conjecture &key hints)
  (:summary "Check if a conjecture is provable without admitting it")
  (:description "Test whether a theorem is provable using ACL2's thm command.
This does NOT add the theorem to the world - use acl2-prove for that.
Use this for quick provability checks before committing to a theorem name.")
  (:param conjecture string "Theorem statement, e.g. \"(equal (+ x y) (+ y x))\"")
  (:param hints string "Proof hints (optional), e.g. \"((\\\"Goal\\\" :induct t))\"")
  (:result (soft-list-of text-content))
  ;; Build thm form and eval it directly
  (let* ((thm-code (if (and hints (plusp (length hints)))
                       (format nil "(acl2::thm ~A :hints ~A)" conjecture hints)
                       (format nil "(acl2::thm ~A)" conjecture))))
    (multiple-value-bind (result error-p error-msg)
        (acl2-eval thm-code)
      (declare (ignore result))
      (list (make-instance 'text-content 
                          :text (if error-p
                                    (format nil "Provable: NIL~%Error: ~A" error-msg)
                                    "Provable: T"))))))

;;; ---------------------------------------------------------------------------
;;; Event Admission
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-admit) (code)
  (:summary "Admit an ACL2 event (defun, defthm, defmacro, etc)")
  (:description "Submit an ACL2 event to be admitted to the logical world.
Supports defun, defthm, defmacro, encapsulate, mutual-recursion, and other events.
Use this for general event submission. For theorems specifically, acl2-prove 
provides more detailed output.")
  (:param code string "ACL2 event, e.g. \"(defun double (x) (* 2 x))\"")
  (:result (soft-list-of text-content))
  ;; Use acl2-eval which works - it returns the result of the form
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval code)
    (if error-p
        (list (make-instance 'text-content 
                            :text (format nil "FAILED: ~A" error-msg)))
        (list (make-instance 'text-content 
                            :text (format nil "Admitted: ~S" result))))))

;;; ---------------------------------------------------------------------------
;;; Syntax Checking
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-check-syntax) (code)
  (:summary "Check ACL2 code for syntax errors")
  (:description "Quickly check ACL2 code for syntax errors without full execution.
Use this before acl2-admit or acl2-prove to catch basic errors like unbalanced
parentheses, undefined packages, or malformed expressions.")
  (:param code string "ACL2 code to check")
  (:result (soft-list-of text-content))
  (handler-case
      (progn
        (read-from-string code)
        (list (make-instance 'text-content :text "Syntax OK: No errors detected")))
    (end-of-file (e)
      (list (make-instance 'text-content 
                          :text (format nil "Syntax Error: Unexpected end of input (unbalanced parentheses?)~%~A" e))))
    (reader-error (e)
      (list (make-instance 'text-content 
                          :text (format nil "Syntax Error: ~A" e))))
    (error (e)
      (list (make-instance 'text-content 
                          :text (format nil "Syntax Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; Guard Verification
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-verify-guards) (function-name)
  (:summary "Verify guards for a function")
  (:description "Verify that a function's guards are satisfied, enabling efficient 
execution in raw Common Lisp. Guards are conditions that ensure a function is 
called with valid inputs. Common workflow:
1) Define function with acl2-admit
2) Verify guards with this tool")
  (:param function-name string "Name of the function to verify guards for")
  (:result (soft-list-of text-content))
  (let ((code (format nil "(acl2::verify-guards ~A)" function-name)))
    (multiple-value-bind (result error-p error-msg)
        (acl2-eval code)
      (declare (ignore result))
      (list (make-instance 'text-content 
                          :text (if error-p
                                    (format nil "Guards FAILED: ~A" error-msg)
                                    (format nil "Guards verified for ~A" function-name)))))))

;;; ---------------------------------------------------------------------------
;;; Query Tools
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-query-event) (name)
  (:summary "Look up definition and properties of an ACL2 event")
  (:description "Query information about an ACL2 function, theorem, or macro.
Returns the definition, formals, guards, and other properties.
Works with built-in ACL2 functions (e.g., 'append', 'len') or user-defined ones.")
  (:param name string "Name of function/theorem to query, e.g. \"append\" or \"my-function\"")
  (:result (soft-list-of text-content))
  ;; Use getpropc to safely query properties (like acl2-get-event-info does)
  (let* ((formals-code (format nil "(acl2::getpropc '~A 'acl2::formals nil (acl2::w acl2::state))" name))
         (body-code (format nil "(acl2::getpropc '~A 'acl2::unnormalized-body nil (acl2::w acl2::state))" name))
         (guard-code (format nil "(acl2::getpropc '~A 'acl2::guard nil (acl2::w acl2::state))" name)))
    (multiple-value-bind (formals f-err) (acl2-eval formals-code)
      (multiple-value-bind (body b-err) (acl2-eval body-code)
        (multiple-value-bind (guard g-err) (acl2-eval guard-code)
          (let ((parts nil))
            (push (format nil "~A" name) parts)
            (if f-err
                (push "Formals: (unknown)" parts)
                (push (format nil "Formals: ~S" formals) parts))
            (unless b-err
              (push (format nil "Body: ~S" body) parts))
            (unless g-err
              (push (format nil "Guard: ~S" guard) parts))
            (when (and f-err b-err g-err)
              (push "(No definition found - may be a primitive or macro)" parts))
            (list (make-instance 'text-content 
                                :text (format nil "~{~A~^~%~}" (nreverse parts))))))))))

;;; ---------------------------------------------------------------------------
;;; Book Management
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-include-book) (book-path &key dir)
  (:summary "Load a certified ACL2 book")
  (:description "Load a certified ACL2 book to use its definitions and theorems.
Use this to import existing ACL2 libraries before proving new theorems.
Common books: \"std/lists/top\", \"arithmetic-5/top\", \"std/util/define\"
IMPORTANT: Provide path WITHOUT .lisp extension.")
  (:param book-path string "Path to book without .lisp extension, e.g. \"std/lists/top\"")
  (:param dir string "Directory keyword: :system for ACL2 books, or path string")
  (:result (soft-list-of text-content))
  (let ((code (if (and dir (plusp (length dir)))
                  (format nil "(include-book ~S :dir ~A)" book-path 
                          (if (string-equal dir ":system") ":system" dir))
                  (format nil "(include-book ~S)" book-path))))
    (multiple-value-bind (result error-p error-msg)
        (acl2-eval code)
      (declare (ignore result))
      (list (make-instance 'text-content 
                          :text (if error-p
                                    (format nil "FAILED to load ~A: ~A" book-path error-msg)
                                    (format nil "Loaded ~A" book-path)))))))

(define-tool (acl2-mcp-tools acl2-certify-book) (book-path)
  (:summary "Certify an ACL2 book")
  (:description "Certify an ACL2 book (a .lisp file containing definitions and theorems).
This verifies all proofs and creates a .cert certificate file.
IMPORTANT: Provide path WITHOUT the .lisp extension.")
  (:param book-path string "Path to book without .lisp extension")
  (:result (soft-list-of text-content))
  (let ((code (format nil "(certify-book ~S)" book-path)))
    (multiple-value-bind (result error-p error-msg)
        (acl2-eval code)
      (declare (ignore result))
      (list (make-instance 'text-content 
                          :text (if error-p
                                    (format nil "Certification FAILED: ~A" error-msg)
                                    (format nil "Certified ~A" book-path)))))))

;;; ---------------------------------------------------------------------------
;;; Undo/History
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools acl2-undo) (&key (count "1"))
  (:summary "Undo recent ACL2 events")
  (:description "Undo the most recent ACL2 events. Use this to backtrack and try 
alternative approaches. By default undoes just the last event.")
  (:param count string "Number of events to undo (default: 1)")
  (:result (soft-list-of text-content))
  (declare (ignore count))  ; TODO: implement multi-undo using count
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval "(acl2::ubt! :here)")
    (declare (ignore result))
    (list (make-instance 'text-content 
                        :text (if error-p
                                  (format nil "Undo FAILED: ~A" error-msg)
                                  "Undone")))))
