#!/bin/bash
# MCP Server Tester - uses LM Studio to generate and run tests
# Self-contained: starts server, generates tests, runs them, tears down
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
PORT=${MCP_PORT:-8085}
MCP_URL="http://127.0.0.1:${PORT}/mcp"
LM_STUDIO_URL="${LM_STUDIO_URL:-http://host.docker.internal:1234/v1}"
LM_MODEL="${LM_MODEL:-qwen/qwen3-coder-30b}"
SERVER_PID=""
RESULTS_FILE=$(mktemp)
echo "0 0" > "$RESULTS_FILE"

# Which server to test: "readme" for simple example, "bridge" for full acl2-mcp-bridge
SERVER_TYPE="${SERVER_TYPE:-readme}"

cleanup() {
  if [ -n "$SERVER_PID" ]; then
    echo ""
    echo "Stopping server (PID $SERVER_PID)..."
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  rm -f "$RESULTS_FILE"
}
trap cleanup EXIT

# MCP JSON-RPC helper
mcp_call() {
  local method="$1"
  local params="$2"
  local id="${3:-1}"
  local payload
  if [ -z "$params" ] || [ "$params" = "{}" ]; then
    payload="{\"jsonrpc\":\"2.0\",\"id\":$id,\"method\":\"$method\"}"
  else
    payload="{\"jsonrpc\":\"2.0\",\"id\":$id,\"method\":\"$method\",\"params\":$params}"
  fi
  curl -s -X POST "$MCP_URL" -H "Content-Type: application/json" -d "$payload"
}

# LM Studio chat helper
llm_chat() {
  local prompt="$1"
  local payload=$(jq -n --arg model "$LM_MODEL" --arg prompt "$prompt" '{
    model: $model,
    messages: [{role: "user", content: $prompt}],
    temperature: 0.3,
    max_tokens: 2000
  }')
  curl -s -X POST "$LM_STUDIO_URL/chat/completions" \
    -H "Content-Type: application/json" \
    -d "$payload" | jq -r '.choices[0].message.content // empty'
}

# Ask LLM to judge if response is correct
llm_judge() {
  local tool_name="$1"
  local tool_desc="$2"
  local input_args="$3"
  local response="$4"
  local expect_success="$5"
  
  local prompt="You are judging an MCP tool response. Answer ONLY 'PASS' or 'FAIL'.

Tool: $tool_name
Description: $tool_desc
Input: $input_args
Expected: $( [ "$expect_success" = "true" ] && echo "Success" || echo "Error" )
Response: $response

Rules:
- For empty input or incomplete syntax (missing parens), 'end of file' errors are valid/expected (PASS)
- Error messages indicating the input was invalid are correct for edge cases
- Success means meaningful output without crashes

Is this response correct for the given input? Reply ONLY 'PASS' or 'FAIL':"

  local verdict=$(llm_chat "$prompt" | tr -d '[:space:]' | grep -oE 'PASS|FAIL' | head -1)
  echo "${verdict:-FAIL}"
}

record_pass() {
  read PASSED FAILED < "$RESULTS_FILE"
  echo "$((PASSED + 1)) $FAILED" > "$RESULTS_FILE"
}

record_fail() {
  read PASSED FAILED < "$RESULTS_FILE"
  echo "$PASSED $((FAILED + 1))" > "$RESULTS_FILE"
}

echo "╔════════════════════════════════════════════════════════════╗"
echo "║           MCP Server Automated Test Suite                  ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""
echo "MCP Server: $MCP_URL"
echo "LM Studio:  $LM_STUDIO_URL"
echo "Model:      $LM_MODEL"
echo ""

# Start server
echo "▶ Starting MCP server ($SERVER_TYPE)..."
cd "$PROJECT_DIR"

if [ "$SERVER_TYPE" = "bridge" ]; then
  # Start full acl2-mcp-bridge server
  sbcl --noinform --disable-debugger \
    --eval "(require :asdf)" \
    --eval "(asdf:load-system :acl2-mcp-bridge)" \
    --eval "(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port $PORT)" \
    --eval "(loop (sleep 1))" \
    > /tmp/mcp-server.log 2>&1 &
else
  # Start simple readme-basic-server
  MCP_PORT=$PORT sbcl --noinform --disable-debugger --script tests/readme-basic-server.lisp > /tmp/mcp-server.log 2>&1 &
fi
SERVER_PID=$!

# Wait for server
echo -n "  Waiting for server"
for i in $(seq 1 30); do
  if curl -s -o /dev/null "$MCP_URL" -X POST -H "Content-Type: application/json" -d '{}' 2>/dev/null; then
    echo " ready!"
    break
  fi
  echo -n "."
  sleep 1
done
echo ""

# Discover tools
echo "▶ Discovering tools..."
TOOLS_RESP=$(mcp_call "tools/list")
TOOLS=$(echo "$TOOLS_RESP" | jq -c '.result.tools // []')
TOOL_COUNT=$(echo "$TOOLS" | jq 'length')
echo "  Found $TOOL_COUNT tool(s)"
echo ""

# Predefined test cases for known tools (avoids LLM generating invalid code)
EVAL_CL_TESTS='[
  {"description": "Happy path - arithmetic", "arguments": {"code": "(+ 1 2 3)"}, "expect_success": true},
  {"description": "Happy path - list operations", "arguments": {"code": "(length (list 1 2 3 4 5))"}, "expect_success": true},
  {"description": "Happy path - string", "arguments": {"code": "(string-upcase \"hello\")"}, "expect_success": true},
  {"description": "Edge case - empty string", "arguments": {"code": ""}, "expect_success": true},
  {"description": "Edge case - whitespace only", "arguments": {"code": "   "}, "expect_success": true},
  {"description": "Edge case - syntax error", "arguments": {"code": "(+ 1 2"}, "expect_success": true},
  {"description": "Multiple values", "arguments": {"code": "(values 1 2 3)"}, "expect_success": true},
  {"description": "Define and use variable", "arguments": {"code": "(let ((x 10)) (* x x))"}, "expect_success": true}
]'

LIST_SESSIONS_TESTS='[
  {"description": "List sessions - no args", "arguments": {}, "expect_success": true},
  {"description": "List sessions - empty object", "arguments": {}, "expect_success": true}
]'

# Process each tool
for i in $(seq 0 $((TOOL_COUNT - 1))); do
  tool=$(echo "$TOOLS" | jq -c ".[$i]")
  TOOL_NAME=$(echo "$tool" | jq -r '.name')
  TOOL_DESC=$(echo "$tool" | jq -r '.description // "No description"')
  TOOL_SCHEMA=$(echo "$tool" | jq -c '.inputSchema // {}')
  
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "Testing tool: $TOOL_NAME"
  echo "Description:  $TOOL_DESC"
  echo ""
  
  # Use predefined tests for known tools, otherwise ask LLM
  case "$TOOL_NAME" in
    eval_cl)
      TEST_CASES="$EVAL_CL_TESTS"
      echo "  Using predefined Common Lisp test cases..."
      ;;
    list_sessions)
      TEST_CASES="$LIST_SESSIONS_TESTS"
      echo "  Using predefined test cases..."
      ;;
    *)
      # Ask LLM to generate test cases for unknown tools
      PROMPT="Generate exactly 3 test cases for this MCP tool. Return ONLY a valid JSON array, no markdown, no explanation.

Tool: $TOOL_NAME
Description: $TOOL_DESC  
Input schema: $TOOL_SCHEMA

Format: [{\"description\": \"test name\", \"arguments\": {args}, \"expect_success\": true, \"expect_contains\": \"text\"}]

Include: 1 happy path, 1 edge case (empty string), 1 with special chars. JSON only:"

      echo "  Generating test cases with LLM..."
      LLM_RESPONSE=$(llm_chat "$PROMPT")
      # Extract JSON array
      TEST_CASES=$(echo "$LLM_RESPONSE" | sed 's/```json//g; s/```//g' | tr '\n' ' ' | grep -oE '\[.*\]' | head -1)
      
      if [ -z "$TEST_CASES" ] || ! echo "$TEST_CASES" | jq empty 2>/dev/null; then
        echo "  ⚠ Could not generate test cases, using defaults"
        TEST_CASES='[{"description": "basic call", "arguments": {"name": "World"}, "expect_success": true, "expect_contains": "Hello"}]'
      fi
      ;;
  esac
  
  # Run each test case
  TC_COUNT=$(echo "$TEST_CASES" | jq 'length')
  for j in $(seq 0 $((TC_COUNT - 1))); do
    tc=$(echo "$TEST_CASES" | jq -c ".[$j]")
    TC_DESC=$(echo "$tc" | jq -r '.description // "unnamed test"')
    TC_ARGS=$(echo "$tc" | jq -c '.arguments // {}')
    TC_EXPECT_SUCCESS=$(echo "$tc" | jq -r '.expect_success // true')
    
    echo -n "  • $TC_DESC... "
    
    PARAMS="{\"name\":\"$TOOL_NAME\",\"arguments\":$TC_ARGS}"
    RESP=$(mcp_call "tools/call" "$PARAMS" 2)
    
    # Check for error in response
    HAS_ERROR=$(echo "$RESP" | jq 'has("error")')
    
    # Use LLM to judge the response
    VERDICT=$(llm_judge "$TOOL_NAME" "$TOOL_DESC" "$TC_ARGS" "$RESP" "$TC_EXPECT_SUCCESS")
    
    if [ "$VERDICT" = "PASS" ]; then
      echo "PASS"
      record_pass
    else
      echo "FAIL"
      echo "    Input: $TC_ARGS"
      echo "    Response: $(echo "$RESP" | jq -c '.result.content[0].text // .error // .')"
      record_fail
    fi
  done
  echo ""
done

read PASSED FAILED < "$RESULTS_FILE"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Summary: $PASSED passed, $FAILED failed"
if [ "$FAILED" -gt 0 ]; then
  exit 1
fi
