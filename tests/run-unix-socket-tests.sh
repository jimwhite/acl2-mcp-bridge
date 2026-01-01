#!/bin/bash
# Unix Socket MCP Test Driver
# Starts the MCP server on a Unix socket, waits for it, then runs Python tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Configuration
SOCK_PATH="${MCP_SOCKET:-/tmp/acl2-mcp-test.sock}"
TIMEOUT=60
VERBOSE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE="-v"
            shift
            ;;
        -s|--socket)
            SOCK_PATH="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [-v|--verbose] [-s|--socket PATH]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose      Enable verbose output"
            echo "  -s, --socket PATH  Unix socket path (default: /tmp/acl2-mcp-test.sock)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=============================================="
echo "Unix Socket MCP Test Suite"
echo "=============================================="
echo "Socket:  $SOCK_PATH"
echo "Project: $PROJECT_DIR"
echo ""

# Clean up any previous server
cleanup() {
    echo ""
    echo "Cleaning up..."
    if [[ -n "$SERVER_PID" ]]; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    rm -f /tmp/mcp-unix-test-server.lisp /tmp/mcp-unix-test-server.log
    rm -f "$SOCK_PATH"
}
trap cleanup EXIT

# Remove any existing socket
rm -f "$SOCK_PATH"

# Create server startup script
cat > /tmp/mcp-unix-test-server.lisp << ENDOFSCRIPT
;; MCP Unix Socket Test Server Startup Script
:q

(sb-ext:disable-debugger)
(load "~/quicklisp/setup.lisp")
(push #p"/workspaces/acl2-mcp-bridge/" asdf:*central-registry*)
(push #p"/workspaces/acl2-mcp-bridge/vendor/40ants-mcp/" asdf:*central-registry*)
(asdf:load-system :acl2-mcp-bridge)
(format t "~%System loaded successfully~%")

;; Start the MCP server on Unix socket
(funcall (find-symbol "START-SERVER" (find-package "ACL2-MCP-BRIDGE")) 
         :protocol :mcp :transport :http :socket-path "$SOCK_PATH")
(format t "~%MCP Server started on Unix socket: $SOCK_PATH~%")
(format t "~%Server ready for connections~%")
(force-output)

(loop (sleep 3600))
ENDOFSCRIPT

echo "Starting MCP server on Unix socket..."
acl2 < /tmp/mcp-unix-test-server.lisp > /tmp/mcp-unix-test-server.log 2>&1 &
SERVER_PID=$!
echo "Server PID: $SERVER_PID"

# Wait for socket file to exist
echo "Waiting for server to start..."
WAIT_COUNT=0
while [[ ! -S "$SOCK_PATH" ]]; do
    sleep 1
    WAIT_COUNT=$((WAIT_COUNT + 1))
    
    # Check if server process died
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo ""
        echo "ERROR: Server process died!"
        echo "--- Server log ---"
        cat /tmp/mcp-unix-test-server.log
        exit 1
    fi
    
    if [[ $WAIT_COUNT -ge $TIMEOUT ]]; then
        echo ""
        echo "ERROR: Server failed to start within ${TIMEOUT}s"
        echo "--- Server log ---"
        cat /tmp/mcp-unix-test-server.log
        exit 1
    fi
    
    if [[ $((WAIT_COUNT % 5)) -eq 0 ]]; then
        echo "  Still waiting... (${WAIT_COUNT}s)"
    fi
done

echo "Socket file created!"
sleep 1  # Give server a moment to be ready for connections

# Run the Python Unix socket tests
echo ""
echo "Running Unix socket tests..."
echo ""

cd "$SCRIPT_DIR"
"$PROJECT_DIR/.venv/bin/python" test-unix-socket.py $VERBOSE
TEST_EXIT=$?

echo ""
if [[ $TEST_EXIT -eq 0 ]]; then
    echo "All tests passed!"
else
    echo "Some tests failed (exit code: $TEST_EXIT)"
fi

exit $TEST_EXIT
