
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Bridge Tools for ACL2/CL Interop
;;;
;;; These tools enable data and code exchange between ACL2 and Common Lisp.
;;; Since we run inside ACL2, we have direct access to both environments.
;;; ============================================================================

(define-api (bridge-api :title "ACL2/CL Bridge Tools"))

;;; ---------------------------------------------------------------------------
;;; Cross-Language Evaluation
;;; ---------------------------------------------------------------------------

(define-tool (bridge-api bridge-acl2-to-cl) (code)
  (:summary "Send ACL2 data to CL")
  (:description "Evaluate an ACL2 expression and make the result available in 
the Common Lisp environment. The result is stored in *acl2-result* and returned.
ACL2 lists become CL lists, symbols become CL symbols, etc.")
  (:param code string "ACL2 form to evaluate")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (acl2-eval code :main-thread-p t)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (progn
          ;; Store in a session-accessible variable
          (when *current-session*
            (with-session (*current-session*)
              (eval `(defparameter ,(intern "*ACL2-RESULT*") ',result))))
          (list (make-instance 'text-content 
                              :text (format nil "Result: ~S~%~
                                                Stored in *acl2-result*" result)))))))

(define-tool (bridge-api bridge-cl-to-acl2) (code)
  (:summary "Send CL data to ACL2")
  (:description "Evaluate a Common Lisp expression and make the result available
for use in ACL2. Useful for preprocessing data in CL before passing to ACL2.
Result is returned as a quoted form suitable for ACL2.")
  (:param code string "Common Lisp expression")
  (:result (soft-list-of text-content))
  (multiple-value-bind (results error-p error-msg)
      (cl-eval code)
    (if error-p
        (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
        (let ((result (car results)))
          (list (make-instance 'text-content 
                              :text (format nil "Result: ~S~%~
                                                ACL2 form: '~S" result result)))))))

(define-tool (bridge-api acl2-cl-eval) (acl2-code cl-code)
  (:summary "Cross-language evaluation")
  (:description "Evaluate expressions in both ACL2 and CL, returning both results.
Useful for comparing behavior or coordinating computation across both environments.")
  (:param acl2-code string "ACL2 form to evaluate")
  (:param cl-code string "Common Lisp form to evaluate")
  (:result (soft-list-of text-content))
  (let ((acl2-result nil) (acl2-error nil)
        (cl-result nil) (cl-error nil))
    ;; Evaluate ACL2
    (multiple-value-bind (result error-p error-msg)
        (acl2-eval acl2-code :main-thread-p t)
      (if error-p
          (setf acl2-error error-msg)
          (setf acl2-result result)))
    ;; Evaluate CL
    (multiple-value-bind (results error-p error-msg)
        (cl-eval cl-code)
      (if error-p
          (setf cl-error error-msg)
          (setf cl-result (car results))))
    (list (make-instance 'text-content 
                        :text (format nil "ACL2: ~:[~S~;Error: ~A~]~%~
                                          CL: ~:[~S~;Error: ~A~]"
                                     acl2-error (or acl2-error acl2-result)
                                     cl-error (or cl-error cl-result))))))

;;; ---------------------------------------------------------------------------
;;; Dependency Analysis
;;; ---------------------------------------------------------------------------

(define-tool (bridge-api get-dependencies) (name)
  (:summary "Analyze theorem dependencies")
  (:description "Get the functions and lemmas that a theorem or function depends on.
Useful for understanding proof structure and identifying necessary lemmas.")
  (:param name string "Name of theorem or function to analyze")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((sym (read-from-string name))
             (deps '()))
        ;; Try to get dependencies from ACL2 world
        (multiple-value-bind (result error-p error-msg)
            (acl2-eval (format nil "(acl2::immediate-subterm-lst '~A (w state))" sym)
                       :main-thread-p t)
          (declare (ignore error-p error-msg))
          (when result (setf deps result)))
        ;; Also check for used-hints if it's a theorem
        (multiple-value-bind (hints error-p error-msg)
            (acl2-eval (format nil "(acl2::getpropc '~A 'acl2::hints nil (w state))" sym)
                       :main-thread-p t)
          (declare (ignore error-p error-msg))
          (when hints 
            (push (cons :hints hints) deps)))
        (list (make-instance 'text-content 
                            :text (format nil "Dependencies for ~A:~%~S" name deps))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; Lemma Extraction  
;;; ---------------------------------------------------------------------------

(define-tool (bridge-api extract-lemmas) (theorem-name)
  (:summary "Get supporting lemmas")
  (:description "Extract the lemmas used in proving a theorem. This analyzes the
proof and returns the rewrite rules and lemmas that were applied.")
  (:param theorem-name string "Name of the theorem")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((sym (read-from-string theorem-name)))
        ;; Get the runic history which tells us what rules were used
        (multiple-value-bind (result error-p error-msg)
            (acl2-eval (format nil 
                              "(let ((world (w state)))
                                 (acl2::getpropc '~A 'acl2::runic-mapping-pairs nil world))"
                              sym)
                       :main-thread-p t)
          (if error-p
              (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
              (list (make-instance 'text-content 
                                  :text (format nil "Lemmas used by ~A:~%~S" theorem-name result))))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; Proof Suggestions (placeholder for AI-assisted suggestions)
;;; ---------------------------------------------------------------------------

(define-tool (bridge-api suggest-proofs) (conjecture)
  (:summary "AI-assisted proof suggestions")  
  (:description "Analyze a conjecture and suggest possible proof strategies.
This provides hints about induction schemes, useful lemmas, and proof approaches.")
  (:param conjecture string "The conjecture to analyze")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((form (read-from-string conjecture))
             (suggestions '()))
        ;; Analyze the form structure
        (cond
          ;; If it contains recursive functions, suggest induction
          ((and (listp form) (some #'symbolp form))
           (push "Consider induction on a recursive argument" suggestions))
          ;; If it's an equality, suggest rewrite rules
          ((and (listp form) (eq (car form) 'equal))
           (push "Try :expand hints on function calls" suggestions)
           (push "Look for applicable rewrite rules" suggestions))
          ;; Generic suggestions
          (t
           (push "Try (thm ...) first to test provability" suggestions)
           (push "Consider breaking into lemmas" suggestions)))
        ;; Try to find related theorems
        (list (make-instance 'text-content 
                            :text (format nil "Suggestions for: ~A~%~{• ~A~%~}"
                                         conjecture suggestions))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; Dependency Graph
;;; ---------------------------------------------------------------------------

(define-tool (bridge-api dependency-graph) (name &optional depth)
  (:summary "Visualize proof dependencies")
  (:description "Generate a dependency graph showing what a theorem or function
depends on. Returns a text representation of the dependency tree.")
  (:param name string "Name of theorem or function")
  (:param depth string "Maximum depth to traverse (default: 3)")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((sym (read-from-string name))
             (max-depth (or (when depth (parse-integer depth :junk-allowed t)) 3))
             (visited (make-hash-table :test 'eq))
             (output (make-string-output-stream)))
        (labels ((print-deps (s d indent)
                   (when (and (< d max-depth) (not (gethash s visited)))
                     (setf (gethash s visited) t)
                     (format output "~A~A~%" 
                             (make-string (* 2 indent) :initial-element #\Space)
                             s)
                     ;; Try to get callees
                     (multiple-value-bind (callees error-p)
                         (acl2-eval (format nil 
                                           "(acl2::getpropc '~A 'acl2::recursivep nil (w state))"
                                           s)
                                    :main-thread-p t)
                       (declare (ignore error-p))
                       (when (and callees (listp callees))
                         (dolist (c callees)
                           (print-deps c (1+ d) (1+ indent))))))))
          (print-deps sym 0 0))
        (list (make-instance 'text-content 
                            :text (format nil "Dependency graph for ~A:~%~A"
                                         name (get-output-stream-string output)))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; Trace Execution
;;; ---------------------------------------------------------------------------

(define-tool (bridge-api trace-execution) (function-name &optional action)
  (:summary "Debug theorem proving")
  (:description "Trace function calls during theorem proving for debugging.
ACTION can be 'trace' to start tracing, 'untrace' to stop, or omit for status.")
  (:param function-name string "Function name to trace")
  (:param action string "Action: 'trace', 'untrace', or omit for status")
  (:result (soft-list-of text-content))
  (handler-case
      (let ((sym (read-from-string function-name)))
        (cond
          ((or (null action) (string-equal action "status"))
           (list (make-instance 'text-content 
                               :text (format nil "Trace status: ~A is ~:[not ~;~]being traced"
                                            sym (member sym (trace))))))
          ((string-equal action "trace")
           (eval `(trace ,sym))
           (list (make-instance 'text-content :text (format nil "Now tracing ~A" sym))))
          ((string-equal action "untrace")
           (eval `(untrace ,sym))
           (list (make-instance 'text-content :text (format nil "Stopped tracing ~A" sym))))
          (t
           (list (make-instance 'text-content :text "Unknown action. Use 'trace', 'untrace', or omit.")))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))