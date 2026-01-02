#!/bin/bash
# Integration tests for MCP HTTP transport (pure bash, using ACL2)
# Run with: ./tests/run-http-integration-tests.sh

set -e
cd "$(dirname "$0")/.."

PORT=${MCP_PORT:-8086}
URL="http://127.0.0.1:${PORT}/mcp"
PASSED=0
FAILED=0
SERVER_PID=""

cleanup() {
    if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

log() { echo "[test] $*"; }
pass() { log "PASS: $1"; ((PASSED++)) || true; }
fail() { log "FAIL: $1"; ((FAILED++)) || true; }

# Start HTTP server
start_server() {
    log "Starting ACL2 MCP HTTP server on port $PORT..."
    
    export MCP_PORT="$PORT"
    
    # Start ACL2 with the MCP bridge in HTTP mode
    # Use quicklisp setup which is pre-loaded
    # The server runs in a background thread, so we loop to keep it alive
    # NOTE: :protocol :mcp selects MCP (not Bridge), :transport :http uses HTTP
    acl2 > /tmp/acl2-mcp-http-test-$$.log 2>&1 <<LISP_EOF &
:q
(load "~/quicklisp/setup.lisp")
(push "/workspaces/acl2-mcp-bridge/" asdf:*central-registry*)
(push "/workspaces/acl2-mcp-bridge/vendor/40ants-mcp/" asdf:*central-registry*)
(asdf:load-system :acl2-mcp-bridge :verbose nil)
(acl2-mcp-bridge::start-server :protocol :mcp :transport :http :port $PORT)
(loop (sleep 60))
LISP_EOF
    SERVER_PID=$!
    
    # Wait for HTTP server to respond
    log "Waiting for HTTP server..."
    local waited=0
    while [ $waited -lt 45 ]; do
        sleep 1
        ((waited++)) || true
        
        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
            log "Server died during startup. Log:"
            cat /tmp/acl2-mcp-http-test-$$.log 2>/dev/null | tail -50
            return 1
        fi
        
        # Try to connect
        if curl -s -o /dev/null -w '' "$URL" -X POST \
           -H "Content-Type: application/json" \
           -d '{"jsonrpc":"2.0","id":0,"method":"ping"}' 2>/dev/null; then
            log "HTTP server ready on port $PORT"
            return 0
        fi
    done
    
    log "Server not responding after 45s. Log:"
    cat /tmp/acl2-mcp-http-test-$$.log 2>/dev/null | tail -50
    return 1
}

# Send JSON-RPC request via curl
send_request() {
    local body="$1"
    curl -s -X POST "$URL" \
        -H "Content-Type: application/json" \
        -d "$body" 2>/dev/null || echo ""
}

# Test 1: Server responds
test_server_responds() {
    log "Test: Server responds to requests"
    local response
    response=$(send_request '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test"}}}')
    
    if [ -n "$response" ]; then
        pass "Server responds to HTTP requests"
    else
        fail "Server did not respond"
    fi
}

# Test 2: Initialize handshake
test_initialize() {
    log "Test: Initialize handshake"
    local response
    response=$(send_request '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test"}}}')
    
    if echo "$response" | grep -q '"protocolVersion"'; then
        pass "Initialize returned protocol version"
        log "  Response: ${response:0:100}..."
    else
        fail "Initialize did not return protocolVersion"
        log "  Response: $response"
    fi
}

# Test 3: Tools list
test_tools_list() {
    log "Test: Tools list"
    local response
    response=$(send_request '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')
    
    if echo "$response" | grep -q '"tools"'; then
        pass "Tools list returned tools array"
        # Count tools
        local tool_count
        tool_count=$(echo "$response" | grep -o '"name"' | wc -l)
        log "  Found $tool_count tools"
    else
        fail "Tools list did not return tools"
        log "  Response: $response"
    fi
}

# Test 4: Call ACL2 eval tool
test_acl2_eval() {
    log "Test: ACL2 evaluation (+ 1 2)"
    local response
    response=$(send_request '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"acl2-eval","arguments":{"expression":"(+ 1 2)"}}}')
    
    if echo "$response" | grep -qE '("result"|"content"|3)'; then
        pass "ACL2 eval returned result"
        log "  Response: ${response:0:150}..."
    else
        fail "ACL2 eval did not return expected result"
        log "  Response: $response"
    fi
}

# Test 5: Session persistence (same session should remember state)
test_session_persistence() {
    log "Test: Session persistence"
    
    # Define a variable in one request
    local response1
    response1=$(send_request '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"cl-eval","arguments":{"expression":"(defvar *test-var* 42)"}}}')
    
    # Read it back in another request (same connection-id from IP)
    local response2
    response2=$(send_request '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"cl-eval","arguments":{"expression":"*test-var*"}}}')
    
    if echo "$response2" | grep -q "42"; then
        pass "Session maintained state between requests"
    else
        fail "Session state not persisted"
        log "  Response1: $response1"
        log "  Response2: $response2"
    fi
}

# Test 6: Error handling
test_error_handling() {
    log "Test: Error handling (invalid method)"
    local response
    response=$(send_request '{"jsonrpc":"2.0","id":6,"method":"nonexistent/method"}')
    
    if echo "$response" | grep -qE '("error"|"code")'; then
        pass "Server returns error for invalid method"
    else
        fail "Server did not return error for invalid method"
        log "  Response: $response"
    fi
}

# Main
echo "=============================================="
echo "ACL2 MCP HTTP Integration Tests"
echo "=============================================="

if ! command -v curl &>/dev/null; then
    log "ERROR: curl is required but not installed"
    exit 1
fi

if ! command -v acl2 &>/dev/null; then
    log "ERROR: acl2 command not found"
    exit 1
fi

if ! start_server; then
    log "ERROR: Failed to start server"
    exit 1
fi

test_server_responds
test_initialize
test_tools_list
test_acl2_eval
test_session_persistence
test_error_handling

cleanup

echo ""
echo "=============================================="
echo "Results: $PASSED passed, $FAILED failed"
echo "=============================================="
[ $FAILED -eq 0 ]
