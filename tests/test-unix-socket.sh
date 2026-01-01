#!/usr/bin/env bash
# Test Unix domain socket support in SBCL
# This verifies SBCL has the capability; actual bridge testing happens in ACL2

set -e

echo "=== Testing SBCL Unix Domain Socket Support ==="

# Test: Basic Unix socket creation
echo -e "\n--- Test: Create and use Unix socket ---"
sbcl --noinform --non-interactive \
  --eval '(require :sb-bsd-sockets)' \
  --eval '
(let* ((path "/tmp/test-bridge-basic.sock")
       (socket (make-instance (quote sb-bsd-sockets:local-socket) :type :stream)))
  (when (probe-file path) (delete-file path))
  (sb-bsd-sockets:socket-bind socket path)
  (sb-bsd-sockets:socket-listen socket 5)
  (format t "SUCCESS: Unix socket created at ~A~%" path)
  (sb-bsd-sockets:socket-close socket)
  (when (probe-file path) (delete-file path)))
' --quit

echo -e "\n=== Unix socket test passed! ==="
