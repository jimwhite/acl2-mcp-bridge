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

def run_command(sock, cmd_type, content):
    """Send command and return (result, error) tuple."""
    send_command(sock, cmd_type, content)
    result = None
    error = None
    while True:
        msg_type, msg_content = read_message(sock)
        if msg_type == "RETURN":
            result = msg_content
        elif msg_type == "ERROR":
            error = msg_content
        elif msg_type == "READY":
            break
    return result, error

socket_path = "/tmp/acl2-bridge-test.sock"
print(f"Connecting to Unix socket: {socket_path}")

sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect(socket_path)
sock.settimeout(10)

# Handshake
msg_type, content = read_message(sock)
assert msg_type == "ACL2_BRIDGE_HELLO", f"Expected HELLO, got {msg_type}"
msg_type, content = read_message(sock)
assert msg_type == "READY", f"Expected READY, got {msg_type}"
print("Connected!")

passed = 0
failed = 0

def test(name, cmd_type, cmd, check):
    global passed, failed
    print(f"Test: {name}...", end=" ")
    result, error = run_command(sock, cmd_type, cmd)
    try:
        check(result, error)
        print("PASS")
        passed += 1
    except AssertionError as e:
        print(f"FAIL: {e}")
        failed += 1

# === LISP command type (single value) ===
test("LISP: simple arithmetic",
     "LISP", "(+ 1 2)",
     lambda r, e: r and r.strip() == "3")

test("LISP: multiplication",
     "LISP", "(* 6 7)",
     lambda r, e: r and r.strip() == "42")

test("LISP: list construction",
     "LISP", "(cons 'a '(b c))",
     lambda r, e: r and "A" in r.upper())

test("LISP: string result",
     "LISP", '"hello world"',
     lambda r, e: r and "hello" in r.lower())

test("LISP: nil result",
     "LISP", "nil",
     lambda r, e: r and r.strip().upper() == "NIL")

test("LISP: t result",
     "LISP", "t",
     lambda r, e: r and r.strip().upper() == "T")

# === LISP_MV command type (multiple values) ===
test("LISP_MV: floor with two values",
     "LISP_MV", "(floor 17 5)",
     lambda r, e: r and "3" in r and "2" in r)

test("LISP_MV: truncate",
     "LISP_MV", "(truncate 10 3)",
     lambda r, e: r and "3" in r and "1" in r)

test("LISP_MV: values form",
     "LISP_MV", "(values 1 2 3)",
     lambda r, e: r and "1" in r and "2" in r and "3" in r)

test("LISP_MV: single value (should still work)",
     "LISP_MV", "(+ 1 2)",
     lambda r, e: r and "3" in r)

# === JSON command type (single value) ===
test("JSON: number",
     "JSON", "42",
     lambda r, e: r and r.strip() == "42")

test("JSON: list as array",
     "JSON", "'(1 2 3)",
     lambda r, e: r and "[" in r and "1" in r)

test("JSON: string",
     "JSON", '"test"',
     lambda r, e: r and "test" in r)

# === JSON_MV command type (multiple values) ===
test("JSON_MV: floor",
     "JSON_MV", "(floor 17 5)",
     lambda r, e: r and "3" in r and "2" in r)

test("JSON_MV: values",
     "JSON_MV", "(values 'a 'b 'c)",
     lambda r, e: r and "A" in r.upper())

# === Error handling ===
test("ERROR: undefined function",
     "LISP", "(undefined-function-xyz)",
     lambda r, e: e is not None or (r is None))

test("ERROR: syntax error",
     "LISP", "(+ 1",
     lambda r, e: e is not None or (r is None))

sock.close()

print(f"\n{'='*40}")
print(f"Results: {passed} passed, {failed} failed")
if failed > 0:
    sys.exit(1)
print("All tests passed!")
PYTEST

echo ""
echo "=== Bridge Unix Socket Test Complete ==="
