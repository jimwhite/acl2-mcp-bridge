#!/bin/bash
# Full integration test for Bridge protocol
# Starts server, runs tests, cleans up

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_DIR"

# Clean up any existing processes
echo "=== Cleaning up old processes ==="
pkill -9 -f "sbcl.*acl2" 2>/dev/null || true
sleep 2

# Start ACL2 with the bridge in background
echo "=== Starting ACL2 Bridge Server ==="
cat > /tmp/start-bridge.lisp << 'EOF'
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

; Start just the listener (not blocking main-thread-loop)
(format t "~%Starting Bridge on port 55433...~%")
(let ((sock (bridge::ccl-make-socket-tcp 55433)))
  (bridge::ccl-process-run-function '(:name "bridge-listener")
                                     'bridge::listener-thread sock))
(format t "Bridge listener started.~%")
(sleep 1000)  ; Keep alive
EOF

cd "$PROJECT_DIR"
acl2 < /tmp/start-bridge.lisp > /tmp/bridge-server.log 2>&1 &
SERVER_PID=$!

echo "Server PID: $SERVER_PID"

# Wait for server to be ready
echo "Waiting for server to start..."
# Use VIRTUAL_ENV if set, otherwise activate .venv
if [ -z "$VIRTUAL_ENV" ]; then
    source "$PROJECT_DIR/.venv/bin/activate"
fi
for i in {1..30}; do
    if python3 -c "import socket; s=socket.socket(); s.settimeout(1); s.connect(('127.0.0.1', 55433)); s.close()" 2>/dev/null; then
        echo "Server is ready on port 55433"
        break
    fi
    if ! kill -0 $SERVER_PID 2>/dev/null; then
        echo "Server process died!"
        cat /tmp/bridge-server.log
        exit 1
    fi
    sleep 1
done

if ! python3 -c "import socket; s=socket.socket(); s.settimeout(1); s.connect(('127.0.0.1', 55433)); s.close()" 2>/dev/null; then
    echo "Server failed to start within 30 seconds"
    cat /tmp/bridge-server.log
    kill $SERVER_PID 2>/dev/null || true
    exit 1
fi

# Run Python test
echo ""
echo "=== Running Python Bridge Test ==="

# Simple socket test first
python3 << 'PYTHON_TEST'
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
    """Send a Bridge protocol command."""
    msg = f"{cmd_type} {len(content)}\n{content}\n"
    sock.sendall(msg.encode())

print("Connecting to Bridge server on port 55433...")
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("127.0.0.1", 55433))
sock.settimeout(10)

# Should receive HELLO
msg_type, content = read_message(sock)
assert msg_type == "ACL2_BRIDGE_HELLO", f"Expected HELLO, got {msg_type}"
print("Connected!")

# Should receive READY
msg_type, content = read_message(sock)
assert msg_type == "READY", f"Expected READY, got {msg_type}"

# Test 1: (+ 1 2)
print("Test 1: (+ 1 2)...", end=" ")
send_command(sock, "LISP", "(+ 1 2)")

result = None
while True:
    msg_type, content = read_message(sock)
    if msg_type == "RETURN":
        result = content
    if msg_type in ("READY", "ERROR"):
        break

assert result and result.strip() == "3", f"Expected 3, got {result}"
print("PASS")

# Test 2: (* 6 7)
print("Test 2: (* 6 7)...", end=" ")
send_command(sock, "LISP", "(* 6 7)")

result = None
while True:
    msg_type, content = read_message(sock)
    if msg_type == "RETURN":
        result = content
    if msg_type in ("READY", "ERROR"):
        break

assert result and result.strip() == "42", f"Expected 42, got {result}"
print("PASS")

sock.close()
print("\nAll tests passed!")
PYTHON_TEST

TEST_RESULT=$?

# Cleanup
echo ""
echo "=== Cleaning up ==="
kill $SERVER_PID 2>/dev/null || true
pkill -9 -f "sbcl.*acl2-mcp-bridge" 2>/dev/null || true

echo ""
echo "=== Server log (last 50 lines) ==="
tail -50 /tmp/bridge-server.log

exit $TEST_RESULT
