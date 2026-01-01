# ACL2 MCP Bridge Server

A Model Context Protocol (MCP) server for ACL2 theorem proving and Common Lisp evaluation, built with `40ants-mcp`. Provides isolated per-client sessions with private namespaces.

## Features

- **MCP Protocol**: Full [MCP 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25) compliance with Streamable HTTP transport
- **Session Isolation**: Each client gets a private CL package - definitions don't leak between sessions
- **Common Lisp REPL**: Evaluate CL code, define functions, load files
- **ACL2 Integration**: Theorem proving tools (admit, check-theorem, verify-guards)
- **Legacy Bridge Protocol**: Backward compatible with ACL2 Bridge TCP protocol

## Quick Start

### HTTP Transport (recommended for multi-client)

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
     --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)' \
     --eval '(loop (sleep 1))'
```

### STDIO Transport (for single-client tools)

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
     --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :stdio)'
```

## MCP Session Management

The server implements MCP session management per the [Streamable HTTP spec](https://modelcontextprotocol.io/specification/2025-11-25/basic/transports#session-management):

1. **Initialize**: Client sends `initialize` request → Server returns `MCP-Session-Id` header
2. **Use**: Client includes `MCP-Session-Id` header on all subsequent requests
3. **Terminate**: Client sends `HTTP DELETE` with `MCP-Session-Id` → Server destroys session

Each session gets:
- **Private package**: `SESSION-<id>` package isolates all definitions
- **Own evaluation context**: `defun`, `defvar`, etc. are session-local
- **Activity tracking**: Last activity timestamp for debugging

## Available Tools

### Common Lisp Tools

| Tool | Description |
|------|-------------|
| `eval_cl` | Evaluate Common Lisp code |
| `load_file` | Load a .lisp source file |
| `define_function` | Define a function (name, args, body) |
| `get_package` | Get current evaluation package name |
| `reset_cl` | Reset context (recreates session package) |

### ACL2 Tools (stubs - implementation in progress)

| Tool | Description |
|------|-------------|
| `admit` | Admit an ACL2 event (defun, defthm, etc.) |
| `check_theorem` | Check if a theorem is provable |
| `verify_guards` | Verify guard conditions |
| `query_event` | Query event properties from history |

## Example Usage

### Evaluate CL Code

```bash
curl -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'
# Note the MCP-Session-Id header in response

curl -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -H 'MCP-Session-Id: <session-id-from-above>' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"eval_cl","arguments":{"code":"(+ 1 2 3)"}}}'
```

### Define and Call a Function

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{
  "name":"define_function",
  "arguments":{"name":"square","lambda_list":"(x)","body":"(* x x)"}}}

{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{
  "name":"eval_cl",
  "arguments":{"code":"(square 7)"}}}
```

### End Session

```bash
curl -X DELETE http://127.0.0.1:8085/mcp \
  -H 'MCP-Session-Id: <session-id>'
```

## VS Code / Cursor Configuration

Add to your workspace settings or `mcp.json`:

```json
{
  "mcp": {
    "servers": {
      "acl2-mcp-bridge": {
        "type": "http",
        "command": "sbcl",
        "args": [
          "--noinform", "--disable-debugger",
          "--eval", "(ql:quickload :acl2-mcp-bridge)",
          "--eval", "(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)",
          "--eval", "(loop (sleep 1))"
        ],
        "url": "http://127.0.0.1:8085/mcp"
      }
    }
  }
}
```

## Project Structure

```
acl2-mcp-bridge/
├── acl2-mcp-bridge.asd     # ASDF system definition
├── package.lisp            # Package exports
├── config.lisp             # Configuration (*bridge-port*, etc.)
├── main.lisp               # Entry points (start-server, stop-server)
├── mcp-server.lisp         # MCP tool definitions & server setup
├── session-transport.lisp  # MCP-compliant HTTP session handling
├── sessions.lisp           # Per-client session & package management
├── acl2-interface.lisp     # ACL2 process management (stub)
├── bridge-protocol.lisp    # Legacy Bridge protocol server
├── message-format.lisp     # Bridge message framing
├── tools-acl2.lisp         # ACL2 MCP tools
├── tools-cl.lisp           # (reserved)
├── tools-bridge.lisp       # Bridge interop tools
├── threading-utils.lisp    # Thread utilities
└── tests/
    ├── startup-tests.lisp  # Unit tests (FiveAM)
    └── mcp-test.sh         # Integration tests
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     MCP Clients                              │
│              (Claude, VS Code, Cursor, etc.)                │
└─────────────────────┬───────────────────────────────────────┘
                      │ HTTP POST/DELETE + MCP-Session-Id header
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              session-http-transport                          │
│  • Extracts/generates session ID per MCP spec               │
│  • Routes to session's private package                      │
│  • Handles DELETE for session termination                   │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                  Session Registry                            │
│  • *sessions* hash table (session-id → cl-session)          │
│  • Each session has private package SESSION-<id>            │
│  • Thread-safe with *sessions-lock*                         │
└─────────────────────┬───────────────────────────────────────┘
                      │
        ┌─────────────┴─────────────┐
        ▼                           ▼
┌───────────────────┐     ┌───────────────────┐
│   CL Evaluation   │     │  ACL2 Interface   │
│  • cl-eval        │     │  • admit          │
│  • cl-load-file   │     │  • check-theorem  │
│  • cl-reset       │     │  • (stubbed)      │
└───────────────────┘     └───────────────────┘
```

## Testing

### Unit Tests (FiveAM)

```bash
sbcl --eval '(asdf:load-system :acl2-mcp-bridge/tests)' \
     --eval '(5am:run! :acl2-mcp-bridge/tests)' \
     --quit
```

### Integration Tests

```bash
./tests/mcp-test.sh
```

## Session Isolation Example

Two clients connecting simultaneously get completely isolated environments:

```
Client A (session-abc123):              Client B (session-xyz789):
  (defvar *x* 100)                        (defvar *x* 999)
  *x* → 100                               *x* → 999
  (defun foo () :A)                       (defun foo () :B)  
  (foo) → :A                              (foo) → :B
```

Each `*x*` and `foo` lives in a separate package (`SESSION-abc123` vs `SESSION-xyz789`).

## Requirements

- SBCL (or other threaded CL implementation)
- Quicklisp with Ultralisp dist (for 40ants-mcp)
- Optional: ACL2 executable for theorem proving tools

## License

MIT License - See [LICENSE](LICENSE)
