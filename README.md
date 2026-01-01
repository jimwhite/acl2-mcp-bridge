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

### Session Identification

Sessions are identified by (in priority order):
1. **`MCP-Session-Id` header** (spec-compliant clients)
2. **Connection ID** (`remote-addr:port`) for clients like VS Code that maintain persistent HTTP connections

This means VS Code and similar clients work automatically without needing to send session headers - the server tracks sessions per HTTP connection.

### Session Lifecycle

1. **Initialize**: Client sends `initialize` request → Server creates session, returns `MCP-Session-Id` header
2. **Use**: All requests from same connection use same session
3. **Terminate**: Client sends `HTTP DELETE` → Server destroys session

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

## VS Code / GitHub Copilot Configuration

VS Code's MCP implementation uses Streamable HTTP with persistent connections. Add to your `.code-workspace` file or VS Code settings:

```json
{
  "mcp": {
    "servers": {
      "acl2-mcp-bridge": {
        "type": "http",
        "url": "http://127.0.0.1:8085/mcp"
      }
    }
  }
}
```

Then start the server separately:

```bash
sbcl --noinform --disable-debugger \
     --eval '(ql:quickload :acl2-mcp-bridge)' \
     --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)' \
     --eval '(loop (sleep 3600))'
```

**Note**: VS Code maintains session state via persistent HTTP connections, so no `MCP-Session-Id` header management is required.
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
├── bridge-sbcl.lisp        # ACL2 Bridge protocol (SBCL port)
├── bridge-protocol.lisp    # Legacy Bridge protocol server (deprecated)
├── message-format.lisp     # Bridge message framing
├── tools-acl2.lisp         # ACL2 MCP tools
├── tools-cl.lisp           # (reserved)
├── tools-bridge.lisp       # Bridge interop tools
├── threading-utils.lisp    # Thread utilities
└── tests/
    ├── startup-tests.lisp  # Unit tests (FiveAM)
    ├── mcp-test.sh         # Integration tests
    └── test-bridge-full.sh # ACL2 Bridge protocol tests
```

## ACL2 Bridge Protocol

The ACL2 Bridge protocol is a simple TCP-based protocol for connecting external tools (Python, Ruby, etc.) to ACL2. This implementation is an SBCL port of the original CCL-only code from `centaur/bridge`.

### Protocol Format

Messages use a simple text format:
```
TYPE LENGTH\n
CONTENT\n
```

**Command Types**: `LISP`, `LISP_MV`, `JSON`, `JSON_MV`
**Response Types**: `ACL2_BRIDGE_HELLO`, `READY`, `RETURN`, `ERROR`, `STDOUT`

### Starting the Bridge Server

```lisp
;; From ACL2 (after loading bridge-sbcl.lisp)
(bridge::start-fn 55433 nil nil nil)  ; TCP port 55433
```

### Python Client Example

```python
import socket

sock = socket.socket()
sock.connect(("127.0.0.1", 55433))

# Read HELLO and READY
# ...

# Send command
cmd = "(+ 1 2)"
sock.send(f"LISP {len(cmd)}\n{cmd}\n".encode())

# Read RETURN message with result "3"
```

### CCL → SBCL Portability Notes

The original `centaur/bridge/bridge-raw.lsp` uses CCL-specific APIs. Key translations:

| CCL | SBCL/Portable |
|-----|---------------|
| `ccl::make-lock` | `bt:make-lock` |
| `ccl::with-lock-grabbed` | `bt:with-lock-held` |
| `ccl::make-semaphore` | `bt:make-semaphore` |
| `ccl::wait-on-semaphore` | `bt:wait-on-semaphore` |
| `ccl::process-run-function` | `bt:make-thread` |
| `ccl::make-socket` | `usocket:socket-listen` |
| `ccl::accept-connection` | `usocket:socket-accept` |
| `ccl::without-interrupts` | `sb-sys:without-interrupts` |
| `cl-user::fundamental-character-output-stream` | `trivial-gray-streams:fundamental-character-output-stream` |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     MCP Clients                              │
│              (Claude, VS Code, Cursor, etc.)                │
└─────────────────────┬───────────────────────────────────────┘
                      │ HTTP POST/DELETE (connection = session)
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              session-http-transport                          │
│  • Identifies session by header OR connection ID            │
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
