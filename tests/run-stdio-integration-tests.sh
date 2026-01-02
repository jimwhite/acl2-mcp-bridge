#!/bin/bash
# Integration tests for MCP Unix socket transport (pure bash, no Python)
# Run with: ./tests/run-stdio-integration-tests.sh

set -e
cd "$(dirname "$0")/.."

SOCK_PATH="/tmp/acl2-mcp-test-$$.sock"
PASSED=0
FAILED=0
SERVER_PID=""

cleanup() {
    if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    rm -f "$SOCK_PATH"
}
trap cleanup EXIT

log() { echo "[test] $*"; }
pass() { log "PASS: $1"; ((PASSED++)) || true; }
fail() { log "FAIL: $1"; ((FAILED++)) || true; }

# Start server and wait for socket
start_server() {
    log "Starting ACL2 MCP server on $SOCK_PATH..."
    rm -f "$SOCK_PATH"
    
    # Start ACL2 with the MCP bridge using Unix socket transport
    # Use quicklisp setup which is pre-loaded
    acl2 > /tmp/acl2-mcp-test-$$.log 2>&1 <<LISP_EOF &
:q
(load "~/quicklisp/setup.lisp")
(push "/workspaces/acl2-mcp-bridge/" asdf:*central-registry*)
(push "/workspaces/acl2-mcp-bridge/vendor/40ants-mcp/" asdf:*central-registry*)
(asdf:load-system :acl2-mcp-bridge :verbose nil)
(acl2-mcp-bridge::start-server :transport :unix-socket :socket-path "$SOCK_PATH")
LISP_EOF
    SERVER_PID=$!
    
    # Wait for socket file to appear
    local waited=0
    while [ ! -S "$SOCK_PATH" ] && [ $waited -lt 45 ]; do
        sleep 1
        ((waited++)) || true
        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
            log "Server died during startup. Log:"
            cat /tmp/acl2-mcp-test-$$.log 2>/dev/null | tail -50
            return 1
        fi
    done
    
    if [ ! -S "$SOCK_PATH" ]; then
        log "Socket not created after 45s. Log:"
        cat /tmp/acl2-mcp-test-$$.log 2>/dev/null | tail -50
        return 1
    fi
    
    # Give server a moment to start accepting
    sleep 2
    log "Server ready (socket: $SOCK_PATH)"
    return 0
}

# Send JSON-RPC message via socat and get response
send_message() {
    local msg="$1"
    echo "$msg" | timeout 10 socat - UNIX-CONNECT:"$SOCK_PATH" 2>/dev/null || echo ""
}

# Test 1: Server starts and creates socket
test_server_starts() {
    log "Test: Server starts and creates socket"
    if [ -S "$SOCK_PATH" ]; then
        pass "Server created socket file"
    else
        fail "Socket file not created"
    fi
}

# Test 2: Initialize handshake
test_initialize() {
    log "Test: Initialize handshake"
    local response
    response=$(send_message '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test"}}}')
    
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
    response=$(send_message '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')
    
    if echo "$response" | grep -q '"tools"'; then
        pass "Tools list returned tools array"
        log "  Response: ${response:0:100}..."
    else
        fail "Tools list did not return tools"
        log "  Response: $response"
    fi
}

# Test 4: Call ACL2 eval tool
test_acl2_eval() {
    log "Test: ACL2 evaluation (+ 1 2)"
    local response
    response=$(send_message '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"acl2-eval","arguments":{"expression":"(+ 1 2)"}}}')
    
    if echo "$response" | grep -qE '("result"|"content"|3)'; then
        pass "ACL2 eval returned result"
        log "  Response: ${response:0:150}..."
    else
        fail "ACL2 eval did not return expected result"
        log "  Response: $response"
    fi
}

# Main
echo "=============================================="
echo "ACL2 MCP Unix Socket Integration Tests"
echo "=============================================="

if ! command -v socat &>/dev/null; then
    log "ERROR: socat is required but not installed"
    log "Install with: apt-get install socat"
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

test_server_starts
test_initialize
test_tools_list
test_acl2_eval

cleanup

echo ""
echo "=============================================="
echo "Results: $PASSED passed, $FAILED failed"
echo "=============================================="
[ $FAILED -eq 0 ]
