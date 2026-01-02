#!/bin/bash
# ACL2 MCP Launcher for VS Code
# Starts ACL2 with MCP HTTP server on Unix socket, then bridges stdio to it
#
# This script:
# 1. Starts ACL2 process with MCP server listening on a Unix socket
# 2. Waits for the socket to be ready
# 3. Exec's mcp-proxy-tool to bridge stdio to the Unix socket

set -e

SOCKET_PATH="${MCP_SOCKET:-/tmp/acl2-mcp-$$.sock}"
LOG_FILE="/tmp/acl2-mcp-$$.log"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Cleanup on exit
cleanup() {
    if [[ -n "$ACL2_PID" ]]; then
        kill "$ACL2_PID" 2>/dev/null || true
    fi
    rm -f "$SOCKET_PATH" "$LOG_FILE" /tmp/acl2-mcp-startup-$$.lisp
}
trap cleanup EXIT

# Create ACL2 startup script
cat > /tmp/acl2-mcp-startup-$$.lisp << ENDOFSCRIPT
;; MCP Server Startup - Unix Socket Mode
:q

(sb-ext:disable-debugger)
(load "~/quicklisp/setup.lisp")
(push #p"$SCRIPT_DIR/" asdf:*central-registry*)
(push #p"$SCRIPT_DIR/vendor/40ants-mcp/" asdf:*central-registry*)
(asdf:load-system :acl2-mcp-bridge)

;; Start MCP server on Unix socket
(funcall (find-symbol "START-SERVER" (find-package "ACL2-MCP-BRIDGE"))
         :protocol :mcp
         :transport :http
         :socket-path "$SOCKET_PATH")

(format t "~%MCP_SOCKET_READY~%")
(force-output)

;; Keep running
(loop (sleep 3600))
ENDOFSCRIPT

# Start ACL2 in background
acl2 < /tmp/acl2-mcp-startup-$$.lisp > "$LOG_FILE" 2>&1 &
ACL2_PID=$!

# Wait for socket to be created (max 60 seconds)
WAIT=0
while [[ ! -S "$SOCKET_PATH" ]] && [[ $WAIT -lt 60 ]]; do
    sleep 1
    WAIT=$((WAIT + 1))
    
    # Check if ACL2 died
    if ! kill -0 "$ACL2_PID" 2>/dev/null; then
        echo "ACL2 process died. Log:" >&2
        cat "$LOG_FILE" >&2
        exit 1
    fi
done

if [[ ! -S "$SOCKET_PATH" ]]; then
    echo "Timeout waiting for Unix socket. Log:" >&2
    cat "$LOG_FILE" >&2
    exit 1
fi

# Use mcp-proxy-tool to bridge stdio to the Unix socket
exec mcp-proxy-tool -p "$SOCKET_PATH"
