#!/bin/bash
# Self-contained MCP HTTP transport test
# Starts server, runs tests, tears down
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
PORT=${MCP_PORT:-8085}
URL="http://127.0.0.1:${PORT}/mcp"
SERVER_PID=""

cleanup() {
  if [ -n "$SERVER_PID" ]; then
    echo "Stopping server (PID $SERVER_PID)..."
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT

echo "=== MCP HTTP Transport Test ==="
echo "Port: $PORT"

# Start server
echo "Starting server..."
cd "$PROJECT_DIR"
MCP_PORT=$PORT sbcl --noinform --disable-debugger --script tests/readme-basic-server.lisp > /tmp/mcp-server.log 2>&1 &
SERVER_PID=$!

# Wait for server to be ready (poll with curl)
echo -n "Waiting for server"
for i in $(seq 1 30); do
  if curl -s -o /dev/null -w '' "$URL" -X POST -H "Content-Type: application/json" -d '{}' 2>/dev/null; then
    echo " ready!"
    break
  fi
  echo -n "."
  sleep 1
done

# Test 1: tools/list
echo -n "Test 1: tools/list... "
RESP=$(curl -s -X POST "$URL" \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}')

if echo "$RESP" | grep -q '"greet"'; then
  echo "PASS"
else
  echo "FAIL"
  echo "Response: $RESP"
  exit 1
fi

# Test 2: tools/call greet
echo -n "Test 2: tools/call greet... "
RESP=$(curl -s -X POST "$URL" \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"greet","arguments":{"name":"Alice"}}}')

if echo "$RESP" | grep -q 'Hello, Alice!'; then
  echo "PASS"
else
  echo "FAIL"
  echo "Response: $RESP"
  exit 1
fi

echo "=== All tests passed! ==="
