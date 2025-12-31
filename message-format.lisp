(in-package #:acl2-mcp-bridge)

;; ACL2 Bridge Message Format
;; Format: TYPE len\nCONTENTS
;; Types: HELLO, READY, RETURN, ERROR

(defun send-bridge-message (stream type content)
  "Send a Bridge protocol message."
  (let ((len (length content)))
    (format stream "~A ~D~%" type len)
    (write-string content stream)
    (force-output stream)))

(defun send-bridge-return (stream content)
  "Send a successful return value."
  (send-bridge-message stream "RETURN" content))

(defun send-bridge-error (stream message)
  "Send an error message."
  (send-bridge-message stream "ERROR" message))

(defun read-bridge-message (stream)
  "Read a Bridge protocol message.

   Returns (cons type content)."
  (let* ((header-line (read-line stream))
         (parts (cl-ppcre:split "\\s+" header-line))
         (type (car parts))
         (len (parse-integer (cadr parts)))
         (content (make-string len)))
    (read-sequence content stream)
    (cons type content)))
