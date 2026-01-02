#!/bin/bash
# MCP Client Test Driver
# Starts the MCP server in ACL2, waits for it, then runs Python client tests
# Supports both HTTP and stdio (via mcp-proxy-tool) transports

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Configuration
PORT="${MCP_PORT:-8080}"
URL="http://localhost:$PORT/mcp"
TIMEOUT=60
VERBOSE=""
TRANSPORT="http"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE="-v"
            shift
            ;;
        -p|--port)
            PORT="$2"
            URL="http://localhost:$PORT/mcp"
            shift 2
            ;;
        -t|--transport)
            TRANSPORT="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [-v|--verbose] [-p|--port PORT] [-t|--transport http|stdio]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose          Enable verbose output"
            echo "  -p, --port PORT        Server port (default: 8080)"
            echo "  -t, --transport TYPE   Transport: 'http' or 'stdio' (default: http)"
            echo ""
            echo "The stdio transport uses mcp-proxy-tool to proxy HTTP to stdio."
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=============================================="
echo "MCP Client Test Suite"
echo "=============================================="
echo "Transport:  $TRANSPORT"
echo "Server URL: $URL"
echo "Project:    $PROJECT_DIR"
echo ""

# Clean up any previous server
cleanup() {
    echo ""
    echo "Cleaning up..."
    if [[ -n "$SERVER_PID" ]]; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    rm -f /tmp/mcp-test-server.lisp /tmp/mcp-test-server.log
}
trap cleanup EXIT

# Create server startup script
cat > /tmp/mcp-test-server.lisp << 'ENDOFSCRIPT'
;; MCP Test Server Startup Script
;; Drop to raw Lisp first
:q

;; Disable debugger for non-interactive use
(sb-ext:disable-debugger)

;; Load quicklisp for dependencies
(load "~/quicklisp/setup.lisp")

;; Add project paths to ASDF  
(push #p"/workspaces/acl2-mcp-bridge/" asdf:*central-registry*)
(push #p"/workspaces/acl2-mcp-bridge/vendor/40ants-mcp/" asdf:*central-registry*)

;; Load the system
(asdf:load-system :acl2-mcp-bridge)
(format t "~%System loaded successfully~%")

;; Start the MCP HTTP server (protocol :mcp, not :bridge)
(funcall (find-symbol "START-SERVER" (find-package "ACL2-MCP-BRIDGE")) 
         :protocol :mcp :port 8080)
(format t "~%MCP Server started on port 8080~%")
(format t "~%Server ready for connections~%")
(force-output)

;; Keep running - the HTTP server runs in threads  
(loop (sleep 3600))
ENDOFSCRIPT

echo "Starting MCP server..."

# Start ACL2 and feed it the script (which drops to raw Lisp with :q)
echo "Using ACL2..."
acl2 < /tmp/mcp-test-server.lisp > /tmp/mcp-test-server.log 2>&1 &
SERVER_PID=$!

echo "Server PID: $SERVER_PID"

# Wait for server to be ready
echo "Waiting for server to start..."
WAIT_COUNT=0
while ! curl -s -o /dev/null -w "%{http_code}" "$URL" 2>/dev/null | grep -q "200\|400\|405"; do
    sleep 1
    WAIT_COUNT=$((WAIT_COUNT + 1))
    
    # Check if server process died
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo ""
        echo "ERROR: Server process died!"
        echo "--- Server log ---"
        cat /tmp/mcp-test-server.log
        exit 1
    fi
    
    if [[ $WAIT_COUNT -ge $TIMEOUT ]]; then
        echo ""
        echo "ERROR: Server failed to start within ${TIMEOUT}s"
        echo "--- Server log ---"
        cat /tmp/mcp-test-server.log
        exit 1
    fi
    
    # Show progress
    if [[ $((WAIT_COUNT % 5)) -eq 0 ]]; then
        echo "  Still waiting... (${WAIT_COUNT}s)"
    fi
done

echo "Server is ready!"
echo ""

# Run the Python client tests
echo "Running MCP client tests..."
echo ""

cd "$SCRIPT_DIR"

if [[ "$TRANSPORT" == "stdio" ]]; then
    # Check that mcp-proxy-tool is available
    if ! command -v mcp-proxy-tool &> /dev/null; then
        echo "ERROR: mcp-proxy-tool not found. Install it from:"
        echo "  https://github.com/awakecoding/mcp-proxy-tool"
        exit 1
    fi
    
    STDIO_CMD="mcp-proxy-tool -u $URL"
    echo "Using mcp-proxy-tool for stdio transport"
    "$PROJECT_DIR/.venv/bin/python" mcp-client-tests.py --transport stdio --stdio-command "$STDIO_CMD" $VERBOSE
else
    "$PROJECT_DIR/.venv/bin/python" mcp-client-tests.py --url "$URL" $VERBOSE
fi
TEST_EXIT=$?

echo ""
if [[ $TEST_EXIT -eq 0 ]]; then
    echo "All tests passed!"
else
    echo "Some tests failed (exit code: $TEST_EXIT)"
fi

exit $TEST_EXIT
