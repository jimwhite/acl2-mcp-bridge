(in-package #:acl2-mcp-bridge/tests)

(defun run-tests ()
  (fiveam:run! 'startup-suite)
  (fiveam:run! 'message-format-suite))
