#!/bin/bash
# Integration test for Bridge protocol
# Starts server, runs Python client tests, cleans up
#
# Usage: ./test-bridge.sh [unix|tcp]
#   unix - Use Unix domain socket (default)
#   tcp  - Use TCP socket on port 55433

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TRANSPORT="${1:-unix}"
SOCKET_PATH="/tmp/acl2-bridge-test.sock"
TCP_PORT=55433

cd "$PROJECT_DIR"

# Validate transport argument
if [[ "$TRANSPORT" != "unix" && "$TRANSPORT" != "tcp" ]]; then
    echo "Usage: $0 [unix|tcp]"
    echo "  unix - Use Unix domain socket (default)"
    echo "  tcp  - Use TCP socket on port $TCP_PORT"
    exit 1
fi

echo "=== Bridge Protocol Integration Test (transport: $TRANSPORT) ==="

# Clean up
cleanup() {
    echo ""
    echo "=== Cleaning up ==="
    pkill -9 -f "sbcl.*acl2" 2>/dev/null || true
    rm -f "$SOCKET_PATH"
}
trap cleanup EXIT

cleanup
sleep 1

# Generate server startup script based on transport
echo "=== Starting ACL2 Bridge Server ==="

if [[ "$TRANSPORT" == "unix" ]]; then
    SOCKET_CODE="(bridge::ccl-make-socket-unix \"$SOCKET_PATH\")"
    START_MSG="Starting Bridge on Unix socket: $SOCKET_PATH"
else
    SOCKET_CODE="(bridge::ccl-make-socket-tcp $TCP_PORT)"
    START_MSG="Starting Bridge on TCP port: $TCP_PORT"
fi

cat > /tmp/start-bridge-test.lisp << EOF
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

; Start listener
(format t "~%$START_MSG~%")
(let ((sock $SOCKET_CODE))
  (bridge::ccl-process-run-function '(:name "bridge-listener")
                                     'bridge::listener-thread sock))
(format t "Bridge listener started.~%")
(sleep 1000)  ; Keep alive
EOF

acl2 < /tmp/start-bridge-test.lisp > /tmp/bridge-test-server.log 2>&1 &
SERVER_PID=$!
echo "Server PID: $SERVER_PID"

# Wait for server to be ready
echo "Waiting for server to start..."

if [[ "$TRANSPORT" == "unix" ]]; then
    # Wait for socket file
    for i in {1..30}; do
        if [ -S "$SOCKET_PATH" ]; then
            echo "Socket file exists: $SOCKET_PATH"
            break
        fi
        if ! kill -0 $SERVER_PID 2>/dev/null; then
            echo "Server process died!"
            cat /tmp/bridge-test-server.log
            exit 1
        fi
        sleep 1
    done
    
    if [ ! -S "$SOCKET_PATH" ]; then
        echo "Timeout waiting for socket"
        cat /tmp/bridge-test-server.log
        exit 1
    fi
else
    # Wait for TCP port
    for i in {1..30}; do
        if python3 -c "import socket; s=socket.socket(); s.settimeout(1); s.connect(('127.0.0.1', $TCP_PORT)); s.close()" 2>/dev/null; then
            echo "Server is ready on port $TCP_PORT"
            break
        fi
        if ! kill -0 $SERVER_PID 2>/dev/null; then
            echo "Server process died!"
            cat /tmp/bridge-test-server.log
            exit 1
        fi
        sleep 1
    done
    
    if ! python3 -c "import socket; s=socket.socket(); s.settimeout(1); s.connect(('127.0.0.1', $TCP_PORT)); s.close()" 2>/dev/null; then
        echo "Server failed to start within 30 seconds"
        cat /tmp/bridge-test-server.log
        exit 1
    fi
fi

# Activate Python environment if needed
if [ -z "$VIRTUAL_ENV" ]; then
    source "$PROJECT_DIR/.venv/bin/activate"
fi

echo ""
echo "=== Running Bridge Protocol Tests ==="

# Run Python tests
if [[ "$TRANSPORT" == "unix" ]]; then
    python3 "$SCRIPT_DIR/bridge-protocol-tests.py" --unix "$SOCKET_PATH"
else
    python3 "$SCRIPT_DIR/bridge-protocol-tests.py" --tcp "127.0.0.1:$TCP_PORT"
fi

TEST_RESULT=$?

echo ""
echo "=== Server log (last 30 lines) ==="
tail -30 /tmp/bridge-test-server.log

exit $TEST_RESULT
