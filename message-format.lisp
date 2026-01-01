(in-package #:acl2-mcp-bridge)

;; ACL2 Bridge Message Format
;; Format: TYPE len\nCONTENTS
;; Types: HELLO, READY, RETURN, ERROR

(defun send-bridge-message (stream type content)
  "Send a Bridge protocol message."
  (let ((len (length content)))
    (format stream "~A ~D~%~A~%" type len content)
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
  (let* ((header-line (read-line stream nil nil)))
    (unless header-line
      (error "Unexpected end of stream while reading Bridge header"))
    (let* ((parts (cl-ppcre:split "\\s+" header-line))
           (type (car parts))
           (len-part (cadr parts)))
      (unless (and type len-part)
        (error "Invalid Bridge header: ~A" header-line))
      (let* ((len (parse-integer len-part))
             (content (make-string len)))
        (read-sequence content stream)
        ;; Consume trailing newline if present to align with bridge spec.
        (let ((terminator (peek-char nil stream nil nil)))
          (when (and terminator (char= terminator #\Newline))
            (read-char stream)))
        (cons type content)))))
