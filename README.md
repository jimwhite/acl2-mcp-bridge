# ACL2 MCP Bridge Server

A Model Context Protocol (MCP) server for ACL2 theorem proving and Common Lisp evaluation, built with `40ants-mcp`. Provides isolated per-client sessions with private namespaces.

## Features

- **MCP Protocol**: Full [MCP 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25) compliance with Streamable HTTP transport
- **Session Isolation**: Each client gets a private CL package - definitions don't leak between sessions
- **Common Lisp REPL**: Evaluate CL code, define functions, load files
- **ACL2 Integration**: Theorem proving tools (admit, check-theorem, verify-guards)
- **Legacy Bridge Protocol**: Backward compatible with ACL2 Bridge TCP protocol

## Installation

This project is fully self-contained using VS Code Dev Containers with the `acl2-jupyter` Docker image. No local installation of ACL2, SBCL, or Quicklisp is required.

### Quick Start with VS Code Dev Container

1. **Clone the repository:**
   ```bash
   git clone https://github.com/jimwhite/acl2-mcp-bridge.git
   ```

2. **Open in VS Code:**
   ```bash
   code acl2-mcp-bridge
   ```

3. **Reopen in Container:**
   - VS Code will detect the `.devcontainer` configuration
   - Click "Reopen in Container" when prompted (or use Command Palette: "Dev Containers: Reopen in Container")
   - Wait for the container to build and start

4. **Start the MCP Server:**
   ```bash
   acl2 < start-mcp-server.lisp
   ```

The devcontainer includes:
- ACL2 built on SBCL
- Quicklisp with Ultralisp (for `40ants-mcp`)
- Python 3 with Jupyter
- All required dependencies pre-installed

## Key Features

### 1. Common Lisp REPL Tools
- **eval_cl** - Evaluate Common Lisp expressions
- **load_file** - Load Lisp source files
- **define_function** - Define CL functions dynamically
- **get_package** - Get current session package
- **query_cl_package** - Introspect packages
- **reset_cl** - Reset session state

### 2. ACL2 Theorem Proving Tools
- **acl2_evaluate** - Evaluate ACL2 expressions
- **acl2_admit** - Admit events (defun, defthm, etc.)
- **acl2_prove** - Prove and admit theorems
- **acl2_check_provable** - Test provability without admitting
- **acl2_check_syntax** - Quick syntax check
- **acl2_verify_guards** - Verify guard conditions
- **acl2_query_event** - Get definitions and properties
- **acl2_include_book** - Load certified books
- **acl2_certify_book** - Certify a book
- **acl2_undo** - Undo recent events

### 3. Bridge & Analysis Tools (experimental)
- **bridge_acl2_to_cl** - Evaluate ACL2, store result in CL
- **bridge_cl_to_acl2** - Evaluate CL, format for ACL2
- **acl2_cl_eval** - Cross-language evaluation
- **get_dependencies** - Analyze theorem dependencies
- **extract_lemmas** - Get lemmas used in a proof
- **suggest_proofs** - Proof strategy suggestions
- **dependency_graph** - Visualize dependencies
- **trace_execution** - Debug function calls


## Quick Start

### Running the MCP Server

From within the devcontainer terminal:

```bash
acl2 < start-mcp-server.lisp
```

The server will print:
```
MCP server starting via 40ants-mcp (HTTP transport)
Hunchentoot server is started. Listening on 127.0.0.1:8085.
```

### Alternative Startup Options

```lisp
;; Start Bridge protocol on port 55433
(acl2-mcp-bridge:start-server :protocol :bridge :port 55433)

;; Or start both protocols
(acl2-mcp-bridge:start-both :bridge-port 55433 :mcp-port 8085)
```

**Note**: stdio transport is NOT supported because ACL2's startup banners and prompts would corrupt the JSON-RPC stream. Use HTTP transport instead, or use the stdio wrapper below.

### STDIO Mode (via Python Wrapper)

For MCP clients that require stdio transport (like some CLI tools), use the Python wrapper:

```json
{
  "mcp": {
    "servers": {
      "acl2-mcp-bridge": {
        "type": "stdio",
        "command": "python3",
        "args": ["/path/to/acl2-mcp-bridge/acl2-mcp-stdio.py"]
      }
    }
  }
}
```

The wrapper:
1. Launches ACL2 as a subprocess
2. Starts the MCP server on a Unix socket
3. Bridges stdio ↔ Unix socket HTTP

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
| `query_cl_package` | Introspect packages (list functions, variables) |
| `reset_cl` | Reset context (recreates session package) |

### ACL2 Theorem Proving Tools

| Tool | Description |
|------|-------------|
| `acl2_evaluate` | Evaluate ACL2 expressions or define functions |
| `acl2_prove` | Submit a defthm for proof and admission |
| `acl2_check_provable` | Check if a theorem is provable (doesn't admit) |
| `acl2_admit` | Admit an ACL2 event (defun, defthm, defmacro, etc.) |
| `acl2_check_syntax` | Quick syntax check without execution |
| `acl2_verify_guards` | Verify guard conditions for a function |
| `acl2_query_event` | Get definition and properties of a named event |
| `acl2_include_book` | Load a certified ACL2 book |
| `acl2_certify_book` | Certify an ACL2 book |
| `acl2_undo` | Undo recent events |

### Bridge & Analysis Tools (experimental)

| Tool | Description |
|------|-------------|
| `bridge_acl2_to_cl` | Evaluate ACL2, store result in CL `*acl2-result*` |
| `bridge_cl_to_acl2` | Evaluate CL, format result for ACL2 |
| `acl2_cl_eval` | Cross-language evaluation (both environments) |
| `get_dependencies` | Analyze theorem/function dependencies |
| `extract_lemmas` | Get supporting lemmas used in a proof |
| `suggest_proofs` | Proof strategy suggestions |
| `dependency_graph` | Visualize proof dependency tree |
| `trace_execution` | Trace/untrace function calls |

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

The workspace file `acl2-mcp-bridge.code-workspace` is pre-configured for MCP. It contains:

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

Just run `acl2 < start-mcp-server.lisp` and VS Code will connect automatically.

**Note**: VS Code maintains session state via persistent HTTP connections, so no `MCP-Session-Id` header management is required.

## Project Structure

```
acl2-mcp-bridge/
├── acl2-mcp-bridge.asd     # ASDF system definition
├── acl2-mcp-stdio.py       # stdio wrapper (launches ACL2, bridges stdio↔Unix socket)
├── package.lisp            # Package exports
├── config.lisp             # Configuration (*bridge-port*, etc.)
├── main.lisp               # Entry points (start-server, stop-server)
├── mcp-server.lisp         # MCP tool definitions & server setup
├── session-transport.lisp  # MCP-compliant HTTP session handling
├── sessions.lisp           # Per-client session & package management
├── acl2-interface.lisp     # ACL2 theorem proving interface
├── bridge-sbcl.lisp        # ACL2 Bridge protocol (SBCL port)
├── bridge-protocol.lisp    # Legacy Bridge protocol server (deprecated)
├── message-format.lisp     # Bridge message framing
├── tools-acl2.lisp         # ACL2 MCP tools (check-theorem, admit, etc.)
├── tools-cl.lisp           # (reserved)
├── tools-bridge.lisp       # Bridge interop tools (cross-eval, dependencies)
├── threading-utils.lisp    # Thread utilities
└── tests/
    ├── startup-tests.lisp       # Unit tests (FiveAM)
    ├── mcp-client-tests.py      # MCP Python client tests (official SDK)
    ├── run-mcp-client-tests.sh  # MCP test runner script
    ├── mcp-test.sh              # MCP integration tests (curl)
    ├── test-bridge.sh           # Bridge protocol tests (unix|tcp)
    └── bridge-protocol-tests.py # Bridge protocol Python test suite
```

## Development Notes

### 40ants-mcp Tool Parameters

When defining tools with optional parameters, use `&key` instead of `&optional`:

```lisp
;; CORRECT - &key works with MCP's named arguments
(define-tool (my-tools query-package) (&key package-name)
  (:param package-name string "Package to query")
  ...)

;; WRONG - &optional doesn't bind MCP named arguments  
(define-tool (my-tools query-package) (&optional package-name)
  ...)
```

MCP sends arguments as a JSON object with named keys (e.g., `{"package_name": "COMMON-LISP"}`). The openrpc-server framework converts `package-name` → `package_name` for the JSON schema, but only `&key` parameters correctly bind from the arguments hash-table.

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

### MCP Client Tests (Python SDK)

The primary test suite uses the official MCP Python SDK to test all tools:

```bash
./tests/run-mcp-client-tests.sh
```

Options:
- `-v, --verbose` - Enable verbose output
- `-p, --port PORT` - Server port (default: 8080)
- `-t, --transport TYPE` - Transport: 'http' or 'stdio' (default: http)

The script:
1. Starts the MCP server in ACL2
2. Waits for it to be ready
3. Runs Python client tests using the official MCP SDK
4. Reports pass/fail results

Tests cover:
- Tool discovery (CL and ACL2 tools)
- `eval_cl`, `define_function`, `get_package`, `reset_cl`, `query_cl_package`
- `acl2_evaluate`, `acl2_admit`, `acl2_prove`, `acl2_check_provable`
- `acl2_query_event`, `acl2_include_book`
- Error handling

### Unit Tests (FiveAM)

Lisp-side unit tests:

```bash
sbcl --eval '(asdf:load-system :acl2-mcp-bridge/tests)' \
     --eval '(5am:run! :acl2-mcp-bridge/tests)' \
     --quit
```

### HTTP Integration Tests (curl)

Low-level HTTP transport tests using curl:

```bash
./tests/run-http-integration-tests.sh
```

### Manual Testing with curl

```bash
# Initialize session
curl -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'

# List available tools
curl -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'

# Call a tool
curl -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"eval_cl","arguments":{"code":"(+ 1 2)"}}}'
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

- **VS Code** with Dev Containers extension
- **Docker** (for running the devcontainer)

All other dependencies (ACL2, SBCL, Quicklisp, Ultralisp) are included in the devcontainer image.

## License

BSD 3-Clause License - See [LICENSE](LICENSE)
