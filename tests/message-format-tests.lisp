(in-package #:acl2-mcp-bridge/tests)

(defsuite message-format-suite)

(in-suite message-format-suite)

(test send-and-read-roundtrip
  (let* ((out (make-string-output-stream))
         (_ (send-bridge-message out "HELLO" "world"))
         (payload (get-output-stream-string out))
         (in (make-string-input-stream payload))
         (msg (read-bridge-message in)))
    (is (equal "HELLO" (car msg)))
    (is (equal "world" (cdr msg)))))

(test send-return-roundtrip
  (let* ((out (make-string-output-stream))
         (_ (send-bridge-return out "42"))
         (payload (get-output-stream-string out))
         (in (make-string-input-stream payload))
         (msg (read-bridge-message in)))
    (is (equal "RETURN" (car msg)))
    (is (equal "42" (cdr msg)))))

(test error-when-header-invalid
  (let* ((payload "BADHEADER\n")
         (in (make-string-input-stream payload)))
    (signals error (read-bridge-message in))))
