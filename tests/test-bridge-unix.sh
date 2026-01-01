#!/bin/bash
# Integration test for Bridge protocol over Unix sockets
# Starts server, runs Python client tests, cleans up

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SOCKET_PATH="/tmp/acl2-bridge-test.sock"

cd "$PROJECT_DIR"

# Clean up
cleanup() {
    echo "=== Cleaning up ==="
    pkill -9 -f "sbcl.*acl2" 2>/dev/null || true
    rm -f "$SOCKET_PATH"
    # Keep logs for debugging: rm -f /tmp/bridge-unix-server.log /tmp/start-bridge-unix.lisp
}
trap cleanup EXIT

cleanup

echo "=== Starting ACL2 Bridge Server (Unix Socket) ==="
cat > /tmp/start-bridge-unix.lisp << EOF
; Exit ACL2 loop first
:q

; Now in raw Lisp - set up debugger hook
(setf *debugger-hook* 
      (lambda (c h) 
        (declare (ignore h))
        (format *error-output* "~&Error: ~A~%" c)
        (sb-ext:exit :code 1)))

; Load quicklisp
(load "~/quicklisp/setup.lisp")

; Load dependencies
(ql:quickload '(:bordeaux-threads :usocket :trivial-gray-streams) :silent t)

; Create BRIDGE package if it doesn't exist
(unless (find-package "BRIDGE")
  (defpackage "BRIDGE"
    (:use "COMMON-LISP")
    (:export "START-FN" "STOP" "IN-MAIN-THREAD" "TRY-IN-MAIN-THREAD")))

; Load the bridge
(load "bridge-sbcl.lisp")

; Start with *no-main-thread* since we're testing simple commands
(setq bridge::*no-main-thread* t)

; Start on Unix socket
(format t "~%Starting Bridge on Unix socket: $SOCKET_PATH~%")
(let ((sock (bridge::ccl-make-socket-unix "$SOCKET_PATH")))
  (bridge::ccl-process-run-function '(:name "bridge-listener")
                                     'bridge::listener-thread sock))
(format t "Bridge listener started on Unix socket.~%")
(sleep 1000)  ; Keep alive
EOF

acl2 < /tmp/start-bridge-unix.lisp > /tmp/bridge-unix-server.log 2>&1 &
SERVER_PID=$!
echo "Server PID: $SERVER_PID"

# Wait for socket file to exist
echo "Waiting for Unix socket..."
for i in {1..30}; do
    if [ -S "$SOCKET_PATH" ]; then
        echo "Socket file exists: $SOCKET_PATH"
        break
    fi
    if ! kill -0 $SERVER_PID 2>/dev/null; then
        echo "Server process died!"
        cat /tmp/bridge-unix-server.log
        exit 1
    fi
    sleep 1
done

if [ ! -S "$SOCKET_PATH" ]; then
    echo "Timeout waiting for socket"
    cat /tmp/bridge-unix-server.log
    exit 1
fi

# Activate Python environment
if [ -z "$VIRTUAL_ENV" ]; then
    source "$PROJECT_DIR/.venv/bin/activate"
fi

echo ""
echo "=== Running Python Bridge Client Tests ==="

python3 << 'PYTEST'
import socket
import sys

def read_message(sock):
    """Read a Bridge protocol message."""
    header = b""
    while not header.endswith(b"\n"):
        c = sock.recv(1)
        if not c:
            return None, None
        header += c
    header = header.decode().strip()
    parts = header.split(" ", 1)
    msg_type = parts[0]
    length = int(parts[1]) if len(parts) > 1 else 0
    
    content = b""
    while len(content) < length:
        chunk = sock.recv(length - len(content))
        if not chunk:
            break
        content += chunk
    
    sock.recv(1)  # trailing newline
    return msg_type, content.decode()

def send_command(sock, cmd_type, content):
    msg = f"{cmd_type} {len(content)}\n{content}\n"
    sock.sendall(msg.encode())

socket_path = "/tmp/acl2-bridge-test.sock"
print(f"Connecting to Unix socket: {socket_path}")

sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect(socket_path)
sock.settimeout(10)

# Should receive HELLO
msg_type, content = read_message(sock)
assert msg_type == "ACL2_BRIDGE_HELLO", f"Expected HELLO, got {msg_type}"
print("Connected!")

# Should receive READY
msg_type, content = read_message(sock)
assert msg_type == "READY", f"Expected READY, got {msg_type}"

# Test 1: Simple arithmetic
print("Test 1: (+ 1 2)...", end=" ")
send_command(sock, "LISP", "(+ 1 2)")

result = None
while True:
    msg_type, content = read_message(sock)
    if msg_type == "READY":
        break
    if msg_type == "RETURN":
        result = content
assert result and "3" in result, f"Expected 3, got {result}"
print("PASS")

# Test 2: Multiplication
print("Test 2: (* 6 7)...", end=" ")
send_command(sock, "LISP", "(* 6 7)")

result = None
while True:
    msg_type, content = read_message(sock)
    if msg_type == "READY":
        break
    if msg_type == "RETURN":
        result = content
assert result and "42" in result, f"Expected 42, got {result}"
print("PASS")

sock.close()
print("\nAll tests passed!")
PYTEST

echo ""
echo "=== Bridge Unix Socket Test Complete ==="
