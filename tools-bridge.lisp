
(in-package #:acl2-mcp-bridge)

;;; ============================================================================
;;; Bridge-Compatible MCP Tools
;;;
;;; These tools expose ACL2 Bridge protocol features via MCP.
;;; Based on centaur/bridge functionality:
;;; - Output capture (STDOUT messages in Bridge)
;;; - Multiple values (LISP_MV/JSON_MV command types)
;;; - Main thread execution (bridge::in-main-thread)
;;;
;;; Reference: $ACL2_SYSTEM_BOOKS/centaur/bridge/top.lisp
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; eval_with_output - Capture printed output like Bridge does
;;; ---------------------------------------------------------------------------
;;; Bridge sends STDOUT messages for CW/FMT output. This tool captures all
;;; printed output and returns it alongside the result.

(define-tool (acl2-mcp-tools eval-with-output) (code &key (environment "acl2"))
  (:summary "Evaluate code and capture all printed output")
  (:description "Evaluate code in ACL2 or CL and capture everything printed to 
standard output (via CW, FMT, FORMAT, PRINC, etc.). Returns both the result 
and the captured output. This mimics Bridge's STDOUT message capture.

Use this when you need to see ACL2's proof commentary or debugging output.")
  (:param code string "Code to evaluate")
  (:param environment string "Environment: 'acl2' (default) or 'cl'")
  (:result (soft-list-of text-content))
  (let ((output-stream (make-string-output-stream))
        (result nil)
        (error-p nil)
        (error-msg nil))
    (handler-case
        (let* ((*standard-output* output-stream)
               (*trace-output* output-stream)
               (*error-output* output-stream))
          (if (string-equal environment "cl")
              ;; CL evaluation
              (multiple-value-bind (results err-p err-msg)
                  (cl-eval code)
                (setf result (car results)
                      error-p err-p
                      error-msg err-msg))
              ;; ACL2 evaluation with output capture
              (let* ((form (read-from-string code))
                     ;; Create a channel for ACL2 output capture
                     (channel (gensym "BRIDGE-OUTPUT")))
                (setf (get channel 'acl2::*open-output-channel-type-key*) :character)
                (setf (get channel 'acl2::*open-output-channel-key*) output-stream)
                (unwind-protect
                    (progv
                        (list (acl2::global-symbol 'acl2::proofs-co)
                              (acl2::global-symbol 'acl2::standard-co))
                        (list channel channel)
                      (setf result
                            (eval `(let ((acl2::state acl2::*the-live-state*)
                                         (*standard-co* ',channel))
                                     (declare (ignorable acl2::state))
                                     ,form))))
                  ;; Cleanup
                  (setf (get channel 'acl2::*open-output-channel-key*) nil)
                  (setf (get channel 'acl2::*open-output-channel-type-key*) nil)))))
      (error (e)
        (setf error-p t
              error-msg (format nil "~A" e))))
    (let ((output (get-output-stream-string output-stream)))
      (if error-p
          (list (make-instance 'text-content 
                              :text (format nil "Error: ~A~@[~%Output:~%~A~]" 
                                           error-msg 
                                           (when (plusp (length output)) output))))
          (list (make-instance 'text-content 
                              :text (format nil "Result: ~S~@[~%~%Output:~%~A~]" 
                                           result
                                           (when (plusp (length output)) output))))))))

;;; ---------------------------------------------------------------------------
;;; eval_multiple_values - Return all values like Bridge's LISP_MV/JSON_MV
;;; ---------------------------------------------------------------------------
;;; Bridge supports LISP_MV and JSON_MV command types that return all values.
;;; Standard MCP tools only return the first value.

(define-tool (acl2-mcp-tools eval-multiple-values) (code &key (environment "acl2"))
  (:summary "Evaluate code and return all values")
  (:description "Evaluate code and return ALL return values, not just the first.
This is equivalent to Bridge's LISP_MV or JSON_MV command types.

Many ACL2 functions return multiple values (e.g., state as second value).
This tool lets you see all of them.")
  (:param code string "Code to evaluate")
  (:param environment string "Environment: 'acl2' (default) or 'cl'")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((form (read-from-string code))
             (values-list
               (if (string-equal environment "cl")
                   ;; CL: use session package
                   (with-session (*current-session*)
                     (multiple-value-list (eval form)))
                   ;; ACL2: bind state
                   (multiple-value-list
                    (eval `(let ((acl2::state acl2::*the-live-state*))
                             (declare (ignorable acl2::state))
                             ,form))))))
        (list (make-instance 'text-content 
                            :text (format nil "~D value~:P returned:~%~{  ~D: ~S~%~}"
                                         (length values-list)
                                         (loop for v in values-list
                                               for i from 0
                                               collect i collect v)))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; eval_main_thread - Execute in main thread for memoization/hons safety
;;; ---------------------------------------------------------------------------
;;; Bridge provides bridge::in-main-thread for this. Required when:
;;; - Using memoized functions (memoization is not thread-safe)
;;; - Doing heavy hons operations (each thread has its own hons space)
;;;
;;; Note: in-main-thread is CCL-only in the original Bridge code.
;;; On SBCL we provide a compatibility wrapper.

(define-tool (acl2-mcp-tools eval-main-thread) (code)
  (:summary "Evaluate in main thread for memoization/hons safety")
  (:description "Execute code in the main Lisp thread. Required when:

1. Using MEMOIZED functions - ACL2's memoization is not thread-safe
2. Heavy HONS operations - each thread has its own hons space

This is equivalent to wrapping code in (bridge::in-main-thread ...).

Example: For memoized fibonacci:
  (eval-main-thread \"(fib 40)\")

Without main thread execution, memoized functions may cause errors or
return incorrect results when called from worker threads.")
  (:param code string "ACL2 code to evaluate in main thread")
  (:result (soft-list-of text-content))
  (handler-case
      (let ((form (read-from-string code)))
        ;; Try to use bridge::in-main-thread if available
        (let ((result
                (if (and (find-package "BRIDGE")
                         (fboundp (find-symbol "IN-MAIN-THREAD" "BRIDGE")))
                    ;; Use Bridge's in-main-thread
                    (eval `(bridge::in-main-thread
                            (let ((acl2::state acl2::*the-live-state*))
                              (declare (ignorable acl2::state))
                              ,form)))
                    ;; Fallback: just eval (SBCL single-threaded hunchentoot handler)
                    ;; Log warning about potential issues
                    (progn
                      (log:warn "bridge::in-main-thread not available, using direct eval")
                      (eval `(let ((acl2::state acl2::*the-live-state*))
                               (declare (ignorable acl2::state))
                               ,form))))))
          (list (make-instance 'text-content :text (format nil "~S" result)))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

;;; ---------------------------------------------------------------------------
;;; Analysis Tools (moved from old bridge tools)
;;; ---------------------------------------------------------------------------

(define-tool (acl2-mcp-tools get-dependencies) (name)
  (:summary "Analyze theorem/function dependencies")
  (:description "Get the functions and lemmas that a theorem or function depends on.
Useful for understanding proof structure and identifying necessary lemmas.")
  (:param name string "Name of theorem or function to analyze")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((sym-str (string-upcase name))
             (info '()))
        ;; Get recursivep (what functions it calls)
        (multiple-value-bind (recursivep err-p)
            (acl2-eval (format nil "(acl2::getpropc '~A 'acl2::recursivep nil (w state))" sym-str))
          (unless err-p
            (when recursivep (push (cons :calls recursivep) info))))
        ;; Get constraint (for constrained functions)
        (multiple-value-bind (constraint err-p)
            (acl2-eval (format nil "(acl2::getpropc '~A 'acl2::constraint-lst nil (w state))" sym-str))
          (unless err-p
            (when constraint (push (cons :constraints constraint) info))))
        ;; Get classes (rule classes for theorems)
        (multiple-value-bind (classes err-p)
            (acl2-eval (format nil "(acl2::getpropc '~A 'acl2::classes nil (w state))" sym-str))
          (unless err-p
            (when classes (push (cons :rule-classes classes) info))))
        (if info
            (list (make-instance 'text-content 
                                :text (format nil "Dependencies for ~A:~%~{~A: ~S~%~}"
                                             name
                                             (loop for (k . v) in info
                                                   collect k collect v))))
            (list (make-instance 'text-content 
                                :text (format nil "No dependency info found for ~A" name)))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

(define-tool (acl2-mcp-tools extract-lemmas) (theorem-name)
  (:summary "Get lemmas used in a proof")
  (:description "Extract the lemmas and rules used in proving a theorem. 
Analyzes the proof and returns the rewrite rules and lemmas that were applied.")
  (:param theorem-name string "Name of the theorem")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((sym-str (string-upcase theorem-name)))
        ;; Get runic-mapping-pairs which tells us what rules were used
        (multiple-value-bind (result error-p error-msg)
            (acl2-eval (format nil 
                              "(acl2::getpropc '~A 'acl2::runic-mapping-pairs nil (w state))"
                              sym-str))
          (if error-p
              (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
              (if result
                  (list (make-instance 'text-content 
                                      :text (format nil "Rules for ~A:~%~S" theorem-name result)))
                  (list (make-instance 'text-content 
                                      :text (format nil "No rule info found for ~A (may not be a theorem)" 
                                                   theorem-name)))))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

(define-tool (acl2-mcp-tools suggest-proofs) (conjecture)
  (:summary "Suggest proof strategies")  
  (:description "Analyze a conjecture and suggest possible proof strategies.
Provides hints about induction schemes, useful lemmas, and proof approaches.")
  (:param conjecture string "The conjecture to analyze")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((form (read-from-string conjecture))
             (suggestions '()))
        ;; Analyze the form structure
        (when (listp form)
          (let ((head (car form)))
            ;; Check for common patterns
            (cond
              ((member head '(equal =))
               (push "• Try :expand hints on function calls in the equality" suggestions)
               (push "• Look for applicable rewrite rules" suggestions))
              ((member head '(implies if))
               (push "• The hypothesis may provide useful case splitting" suggestions)
               (push "• Consider strengthening the hypothesis if proof fails" suggestions))
              ((eq head 'and)
               (push "• May be easier to prove conjuncts separately as lemmas" suggestions))
              ((eq head 'or)
               (push "• Consider proving by cases" suggestions)))
            ;; Check for recursive function calls
            (when (some #'symbolp (cdr form))
              (push "• If functions are recursive, try :induct hint" suggestions))
            ;; Generic suggestions
            (push "• Use (thm ...) first to test provability without naming" suggestions)
            (push "• Add :hints ((\"Goal\" :in-theory (enable ...))) if needed" suggestions)))
        (list (make-instance 'text-content 
                            :text (format nil "Suggestions for: ~A~%~%~{~A~%~}"
                                         conjecture (nreverse suggestions)))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Parse error: ~A" e))))))

(define-tool (acl2-mcp-tools dependency-graph) (name &key (depth "3"))
  (:summary "Visualize proof dependencies")
  (:description "Generate a text dependency graph showing what a theorem or 
function depends on. Returns a tree representation.")
  (:param name string "Name of theorem or function")
  (:param depth string "Maximum depth to traverse (default: 3)")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((sym-str (string-upcase name))
             (max-depth (or (parse-integer depth :junk-allowed t) 3))
             (visited (make-hash-table :test 'equal))
             (output (make-string-output-stream)))
        (labels ((print-deps (s d indent)
                   (when (and (< d max-depth) (not (gethash s visited)))
                     (setf (gethash s visited) t)
                     (format output "~A~A~%" 
                             (make-string (* 2 indent) :initial-element #\Space)
                             s)
                     ;; Get callees
                     (multiple-value-bind (callees err-p)
                         (acl2-eval (format nil 
                                           "(acl2::getpropc '~A 'acl2::recursivep nil (w state))"
                                           s))
                       (unless err-p
                         (when (and callees (listp callees))
                           (dolist (c callees)
                             (print-deps (string c) (1+ d) (1+ indent)))))))))
          (print-deps sym-str 0 0))
        (let ((result (get-output-stream-string output)))
          (list (make-instance 'text-content 
                              :text (if (plusp (length result))
                                        (format nil "Dependency graph for ~A:~%~A" name result)
                                        (format nil "~A has no recorded dependencies" name))))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))

(define-tool (acl2-mcp-tools trace-execution) (function-name &key (action "status"))
  (:summary "Trace function calls for debugging")
  (:description "Trace or untrace function calls for debugging.
ACTION: 'trace' to start, 'untrace' to stop, 'status' to check (default).")
  (:param function-name string "Function name to trace")
  (:param action string "Action: 'trace', 'untrace', or 'status'")
  (:result (soft-list-of text-content))
  (handler-case
      (let ((sym (read-from-string (string-upcase function-name))))
        (cond
          ((string-equal action "status")
           (list (make-instance 'text-content 
                               :text (format nil "~A is ~:[not ~;~]being traced"
                                            sym (member sym (trace))))))
          ((string-equal action "trace")
           (eval `(trace ,sym))
           (list (make-instance 'text-content :text (format nil "Now tracing ~A" sym))))
          ((string-equal action "untrace")
           (eval `(untrace ,sym))
           (list (make-instance 'text-content :text (format nil "Stopped tracing ~A" sym))))
          (t
           (list (make-instance 'text-content 
                               :text "Unknown action. Use 'trace', 'untrace', or 'status'.")))))
    (error (e)
      (list (make-instance 'text-content :text (format nil "Error: ~A" e))))))
