
(in-package #:acl2-mcp-bridge)

;; Common Lisp-specific MCP tools

(define-api (cl-api :title "Common Lisp MCP Tools"))

(define-tool (cl-api eval-cl) (code)
  (:summary "Evaluate Common Lisp code")
  (:param code string "Common Lisp form(s)")
  (:result (soft-list-of text-content))
  (multiple-value-bind (results error-p error-msg)
      (cl-eval code)
    (if error-p
      (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
      ;; Format results: single value as itself, multiple values separated by newlines
      (list (make-instance 'text-content 
              :text (cond
                     ((null results) "NIL")
                     ((null (cdr results)) (format nil "~S" (car results)))
                     (t (format nil "~{~S~^~%~}" results))))))))

(define-tool (cl-api load-file) (path)
  (:summary "Load a Common Lisp file")
  (:param path string "Path to .lisp file")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (cl-load-file path)
    (if error-p
      (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
      (list (make-instance 'text-content :text "File loaded successfully")))))

(define-tool (cl-api define-function) (name lambda-list body)
  (:summary "Define a function")
  (:param name string "Function name")
  (:param lambda-list string "Parameter list")
  (:param body string "Function body")
  (:result (soft-list-of text-content))
  (multiple-value-bind (result error-p error-msg)
      (cl-define-function (read-from-string name) 
                         (read-from-string lambda-list)
                         (read-from-string body))
    (if error-p
      (list (make-instance 'text-content :text (format nil "Error: ~A" error-msg)))
      (list (make-instance 'text-content :text (format nil "Defined ~A" name))))))
