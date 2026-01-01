
(in-package #:acl2-mcp-bridge)

;; ACL2 Interface - Direct evaluation in running ACL2
;;
;; This code runs INSIDE ACL2, so we have direct access to ACL2 symbols
;; and *the-live-state*. Similar to centaur/bridge.

(defun initialize-acl2-interface (&optional acl2-path)
  "No-op - we're already running inside ACL2."
  (declare (ignore acl2-path))
  t)

(defun acl2-eval (form-string &key session-id main-thread-p)
  "Evaluate a form in ACL2.

   FORM-STRING is a string containing an s-expression.
   If MAIN-THREAD-P is true, use bridge::in-main-thread for memoization safety.

   Returns (values result error-p error-message)"
  (declare (ignore session-id))
  (handler-case
      (let* ((form (read-from-string form-string))
             (wrapped-form `(let ((acl2::state acl2::*the-live-state*))
                              (declare (ignorable acl2::state))
                              ,form)))
        (if main-thread-p
            (values (eval `(bridge::in-main-thread ,wrapped-form)) nil nil)
            (values (eval wrapped-form) nil nil)))
    (error (e)
      (values nil t (format nil "~A" e)))))

(defun acl2-query (form-string &key session-id)
  "Query ACL2 for a computation result.
   Returns (values result error-p error-message)"
  (acl2-eval form-string :session-id session-id))

(defun acl2-event (form-string &key session-id)
  "Submit an ACL2 event (defun, defthm, etc).
   Returns (values success-p error-message output-string)"
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((form (read-from-string form-string))
               (*standard-output* output)
               (result (eval `(let ((acl2::state acl2::*the-live-state*))
                                (declare (ignorable acl2::state))
                                ,form))))
          (declare (ignore result))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-check-theorem (conjecture &key session-id hints)
  "Check if a conjecture is provable using thm."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((form (read-from-string conjecture))
               (thm-form (if hints
                             `(acl2::thm ,form :hints ,(read-from-string hints))
                             `(acl2::thm ,form)))
               (*standard-output* output))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   ,thm-form))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-admit (event &key session-id)
  "Admit an event to the ACL2 world."
  (acl2-event event :session-id session-id))

(defun acl2-verify-guards (function &key session-id)
  "Verify guards for a function."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((fn-sym (read-from-string function))
               (*standard-output* output))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   (acl2::verify-guards ,fn-sym)))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-prove-theorem (conjecture &key session-id hints rule-classes)
  "Prove and admit a theorem using defthm.
   Unlike check-theorem (which uses thm), this permanently admits the theorem."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let* ((form (read-from-string conjecture))
               ;; Generate a theorem name from the form
               (thm-name (intern (format nil "THM-~A" (get-universal-time)) :acl2))
               (defthm-form `(acl2::defthm ,thm-name ,form
                               ,@(when hints `(:hints ,(read-from-string hints)))
                               ,@(when rule-classes `(:rule-classes ,(read-from-string rule-classes)))))
               (*standard-output* output))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   ,defthm-form))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-check-book (book-path &key session-id)
  "Check/certify an ACL2 book.
   BOOK-PATH is the path without the .lisp extension."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let ((*standard-output* output))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   (acl2::certify-book ,book-path)))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-get-event-history (&key session-id name type limit)
  "Retrieve proof/event history from ACL2 world.
   NAME - filter by event name (optional)
   TYPE - filter by event type: defun, defthm, etc (optional)
   LIMIT - max events to return (optional, default 50)"
  (declare (ignore session-id))
  (handler-case
      (let* ((world (symbol-value 'acl2::*the-live-state*))
             (limit (or limit 50))
             (events '()))
        ;; Get command landmarks from the world
        (eval `(let ((acl2::state acl2::*the-live-state*))
                 (declare (ignorable acl2::state))
                 (acl2::er-progn
                  (acl2::assign acl2::temp-result 
                                (acl2::access acl2::state-global-let* acl2::state))
                  (acl2::value nil))))
        ;; For now, return what we can access - this is a simplified implementation
        ;; A full implementation would walk (w state) to get the history
        (values (list :events events :count (length events)) nil nil))
    (error (e)
      (values nil t (format nil "~A" e)))))

(defun acl2-undo-to-point (command-number &key session-id)
  "Undo ACL2 world to a previous command number.
   COMMAND-NUMBER - the command number to undo back to, or :last to undo just the last command."
  (declare (ignore session-id))
  (let ((output (make-string-output-stream)))
    (handler-case
        (let ((*standard-output* output)
              (ubt-form (if (eq command-number :last)
                            '(acl2::ubt! :here)  ; Undo last
                            `(acl2::ubt ,command-number))))
          (eval `(let ((acl2::state acl2::*the-live-state*))
                   (declare (ignorable acl2::state))
                   ,ubt-form))
          (values t nil (get-output-stream-string output)))
      (error (e)
        (values nil (format nil "~A" e) (get-output-stream-string output))))))

(defun acl2-get-event-info (name &key session-id)
  "Get information about a named event (function, theorem, etc).
   Returns definition, type, guards, etc."
  (declare (ignore session-id))
  (handler-case
      (let* ((sym (read-from-string name))
             (info '()))
        (eval `(let ((acl2::state acl2::*the-live-state*))
                 (declare (ignorable acl2::state))
                 ;; Try to get function body
                 (when (acl2::getpropc ',sym 'acl2::unnormalized-body nil (acl2::w acl2::state))
                   (push (cons :body (acl2::getpropc ',sym 'acl2::unnormalized-body nil (acl2::w acl2::state))) 
                         ',info))
                 ;; Try to get formals
                 (when (acl2::getpropc ',sym 'acl2::formals nil (acl2::w acl2::state))
                   (push (cons :formals (acl2::getpropc ',sym 'acl2::formals nil (acl2::w acl2::state)))
                         ',info))
                 ;; Try to get guard
                 (when (acl2::getpropc ',sym 'acl2::guard nil (acl2::w acl2::state))
                   (push (cons :guard (acl2::getpropc ',sym 'acl2::guard nil (acl2::w acl2::state)))
                         ',info))))
        (values info nil nil))
    (error (e)
      (values nil t (format nil "~A" e)))))

;; Session management is simplified - we're in a single ACL2 process
;; Sessions are just for tracking MCP client state, not separate ACL2 instances

(defstruct acl2-session
  (id (string-downcase (princ-to-string (uuid:make-v4-uuid))) :type string)
  (created-at (local-time:now))
  (last-activity (local-time:now)))

(defparameter *acl2-sessions* (make-hash-table :test 'equal))

(defun start-acl2-session (&key session-id)
  "Register a new session (for MCP client tracking)."
  (let* ((id (or session-id (string-downcase (princ-to-string (uuid:make-v4-uuid)))))
         (session (make-acl2-session :id id)))
    (setf (gethash id *acl2-sessions*) session)
    id))

(defun get-acl2-session (id)
  (gethash id *acl2-sessions*))

(defun stop-acl2-session (session-id)
  "Remove a session."
  (remhash session-id *acl2-sessions*))

(defun list-acl2-sessions ()
  "List all registered sessions."
  (loop for session being the hash-values of *acl2-sessions*
        collect (list :id (acl2-session-id session)
                      :created (acl2-session-created-at session)
                      :last-activity (acl2-session-last-activity session))))
