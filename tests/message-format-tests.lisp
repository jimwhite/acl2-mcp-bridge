(in-package #:acl2-mcp-bridge/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite message-format-suite))

(in-suite message-format-suite)

(test send-and-read-roundtrip
  (let* ((out (make-string-output-stream))
      (_ (acl2-mcp-bridge::send-bridge-message out "HELLO" "world"))
         (payload (get-output-stream-string out))
         (in (make-string-input-stream payload))
      (msg (acl2-mcp-bridge::read-bridge-message in)))
    (is (char= #\Newline (char payload (1- (length payload)))))
    (is (equal "HELLO" (car msg)))
    (is (equal "world" (cdr msg)))))

(test send-return-roundtrip
  (let* ((out (make-string-output-stream))
      (_ (acl2-mcp-bridge::send-bridge-return out "42"))
         (payload (get-output-stream-string out))
         (in (make-string-input-stream payload))
      (msg (acl2-mcp-bridge::read-bridge-message in)))
    (is (char= #\Newline (char payload (1- (length payload)))))
    (is (equal "RETURN" (car msg)))
    (is (equal "42" (cdr msg)))))

(test error-when-header-invalid
  (let* ((payload "BADHEADER\n")
         (in (make-string-input-stream payload)))
    (signals error (acl2-mcp-bridge::read-bridge-message in))))
