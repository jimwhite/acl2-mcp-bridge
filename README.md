# ACL2 MCP Bridge Server (`acl2-mcp-bridge`)

Multi-protocol ACL2 server that supports the legacy ACL2 Bridge protocol, modern Model Context Protocol (MCP) via 40ants-mcp, and a direct Common Lisp REPL. Works on SBCL, CCL, ECL, and other threaded Lisps. This is a synthesis of `ACL2 Bridge`, `septract/acl2-mcp`, and `40ants-mcp`.

## Project Structure

```text
acl2-mcp-bridge/
├── acl2-mcp-bridge.asd
├── package.lisp
├── config.lisp
├── main.lisp
├── acl2-interface.lisp
├── sessions.lisp
├── threading-utils.lisp
├── bridge-protocol.lisp
├── message-format.lisp
├── tools-acl2.lisp
├── tools-cl.lisp
├── tools-bridge.lisp
├── mcp-server.lisp
├── mcp-tools.lisp
├── tests/
│   ├── package.lisp
│   ├── startup-tests.lisp
│   ├── message-format-tests.lisp
│   └── run-tests.lisp
└── README.md
```

## Capabilities

- **Protocols**: ACL2 Bridge (TCP) for backward compatibility, MCP (stdio/http) for modern AI agents, and direct CL REPL.
- **Sessions**: Persistent ACL2 and CL sessions with IDs, metadata, and output capture.
- **Bridge tooling**: ACL2⇄CL data transfer and cross-eval helpers.
- **ACL2 tools**: admit/check-theorem/verify-guards/query-event/list-sessions (extensible stubs ready for real ACL2 wiring).
- **CL tools**: eval, load-file, define-function, list-sessions.
- **Thread-aware**: Hooks for main-thread ACL2 safety; bridge server uses per-connection threads.

# ACL2 MCP Bridge Server (`acl2-mcp-bridge`)

A unified Model Context Protocol (MCP) server for `ACL2` built with `40ants-mcp` in Common Lisp. Combines features from `ACL2 Bridge`, `septract/acl2-mcp`, and native CL capabilities.

## Project Structure

```text
acl2-mcp-bridge/
├── src/
│   ├── acl2-mcp-bridge.asd          # ASDF system definition
│   ├── packages.lisp                # Package definitions
│   ├── core/
│   │   ├── acl2-interface.lisp      # ACL2 interaction layer
│   │   ├── session-management.lisp  # Persistent session support
│   │   └── error-handling.lisp      # Robust error management
│   ├── mcp/
│   │   ├── server.lisp              # MCP server setup (40ants-mcp)
│   │   ├── tools.lisp               # MCP tool definitions
│   │   ├── resources.lisp           # Resource endpoints
│   │   └── prompts.lisp             # Prompt templates
│   └── cl-repl/
│       ├── evaluator.lisp           # Common Lisp REPL integration
│       └── cl-bridge.lisp           # Bridge to native CL features
├── examples/
│   └── config-example.json          # MCP client configuration
└── README.md
```

## Key Features

### 1. ACL2 Theorem Proving Tools (from septract/acl2-mcp)
- **check-theorem** - Verify a specific theorem
- **admit** - Admit an event (function/theorem)
- **query-event** - Retrieve event definitions and properties
- **verify-guards** - Check guard conditions
- **prove-theorem** - Attempt automated proofs
- **check-book** - Validate an entire ACL2 book
- **get-event-history** - Retrieve proof history
- **undo-to-point** - Revert to earlier state

### 2. Session Management (from ACL2 Bridge)
- **start-session** - Create persistent ACL2 session
- **end-session** - Clean up session
- **session-state** - Query current session state
- **list-sessions** - Get all active sessions
- **session-output** - Capture ACL2 output

### 3. Common Lisp REPL Integration (native + bridge)
- **eval-cl** - Evaluate Common Lisp expressions
- **cl-repl-session** - Persistent CL REPL session
- **load-file** - Load Lisp source files
- **define-function** - Define CL functions dynamically
- **query-cl-package** - Introspect packages

### 4. Bridge & Interop Tools
- **bridge-acl2-to-cl** - Send ACL2 data to CL
- **bridge-cl-to-acl2** - Send CL data to ACL2
- **acl2-cl-eval** - Cross-language evaluation
- **get-dependencies** - Analyze theorem dependencies

### 5. Code Analysis & Transformation
- **extract-lemmas** - Get supporting lemmas
- **suggest-proofs** - AI-assisted proof suggestions
- **dependency-graph** - Visualize proof dependencies
- **trace-execution** - Debug theorem proving


## Usage Examples

### Proving a Theorem

```json
{
  "tool": "admit",
  "arguments": {
    "code": "(defthm append-assoc (equal (append (append x y) z) (append x (append y z))))"
  }
}
```

### Evaluating Common Lisp

```json
{
  "tool": "eval_cl",
  "arguments": {
    "code": "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))"
  }
}
```

### Defining a Function

```json
{
  "tool": "define_function",
  "arguments": {
    "name": "square",
    "lambda_list": "(x)",
    "body": "(* x x)"
  }
}
```

### Resetting the Context

```json
{
  "tool": "reset_cl",
  "arguments": {}
}
```


## Install

```bash
git clone https://github.com/jimwhite/acl2-mcp-bridge.git
cd acl2-mcp-bridge
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' --quit
```

## Run

### Start Bridge protocol (default):

```lisp
(ql:quickload :acl2-mcp-bridge)
(acl2-mcp-bridge:start-server :protocol :bridge :port 13721)
```

### Start MCP over stdio (good for MCP clients/agents):

```lisp
(ql:quickload :acl2-mcp-bridge)
(acl2-mcp-bridge:start-server :protocol :mcp :transport :stdio)
```

### Start both protocols:

```lisp
(ql:quickload :acl2-mcp-bridge)
(acl2-mcp-bridge:start-both)
```

### HTTP MCP (example):

```lisp
(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)
```

HTTP transport details (matches 40ants-mcp defaults):
- Endpoint: POST `http://127.0.0.1:8085/mcp` with JSON-RPC payloads.
- Event stream: GET `http://127.0.0.1:8085/mcp` upgrades to SSE for notifications.
- There is no `/openrpc.json` or root handler; other paths return 404.

### Testing

#### Unit tests (FiveAM)

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge/tests)' \
  --eval '(acl2-mcp-bridge/tests:run-tests)' \
  --quit
```

Or script style:

```bash
sbcl --script tests/run-tests-script.lisp
```

#### Integration tests (MCP tools)

Automated MCP server testing with LLM-powered test generation:

```bash
# Test full acl2-mcp-bridge server
SERVER_TYPE=bridge ./tests/mcp-test.sh

# Test simple example server
SERVER_TYPE=readme ./tests/mcp-test.sh
```

The test script:
- Discovers all tools via `tools/list`
- Runs predefined tests for CL tools (eval_cl, sessions)
- Uses LLM to generate and judge tests for unknown tools
- Validates session persistence workflows
- Reports pass/fail summary (currently 14 tests, all passing)

### Smoke tests

- Start MCP (stdio):

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
  --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :stdio)' \
  --eval '(loop (sleep 1))'
```

- Start MCP (HTTP):

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
  --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)' \
  --eval '(loop (sleep 1))'
```

- Curl smoke (eval_cl over HTTP):

```bash
curl -s -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"eval_cl","arguments":{"code":"(+ 1 2 3)"}}}'
```

Method names (snake_case), as registered today:
- CL tools: `eval_cl`, `load_file`, `define_function`, `get_package`, `reset_cl`
- ACL2 tools (stubs): `admit`, `check_theorem`, `verify_guards`, `query_event`

Example (eval_cl):

```bash
curl -s -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"eval_cl","arguments":{"code":"(+ 1 2 3)"}}}'
```

Claude Desktop (HTTP) example:

```json
{
  "mcpServers": {
    "acl2-bridge-http": {
      "command": "sbcl",
      "args": [
        "--eval",
        "(ql:quickload :acl2-mcp-bridge)",
        "--eval",
        "(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)",
        "--eval",
        "(loop (sleep 1))"
      ],
      "url": "http://127.0.0.1:8085/mcp"
    }
  }
}
```

### VS Code / Cursor quickstart

- **Stdio (preferred for local dev):** In the VS Code or Cursor terminal, run:

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
  --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :stdio)' \
  --eval '(loop (sleep 1))'
```

Point your MCP client (VS Code extension or Cursor MCP settings) at this command as a stdio server.

- **HTTP:** Start the server, then configure your MCP client with `url: http://127.0.0.1:8085/mcp`:

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
  --eval '(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :port 8085)' \
  --eval '(loop (sleep 1))'
```

Smoke test HTTP with curl:

```bash
curl -s -X POST http://127.0.0.1:8085/mcp \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"eval_cl","arguments":{"code":"(+ 1 2 3)"}}}'
```

#### VS Code workspace settings (MCP extension)

- HTTP (matches the bundled `acl2-mcp-bridge.code-workspace`):

```jsonc
"mcp": {
  "servers": {
    "acl2-mcp-bridge": {
      "type": "http",
      "command": "sbcl",
      "args": [
        "--noinform",
        "--disable-debugger",
        "--non-interactive",
        "--eval",
        "(ql:quickload :acl2-mcp-bridge)",
        "--eval",
        "(progn (acl2-mcp-bridge:start-server :protocol :mcp :transport :http :host \"0.0.0.0\" :port 8085) (loop (sleep 1)))"
      ],
      "env": {
        "ASDF_SOURCE_REGISTRY": "(:source-registry (:tree \"/workspaces/acl2-mcp-bridge/\") :inherit-configuration)",
        "CL_SOURCE_REGISTRY": "/workspaces/acl2-mcp-bridge//:"
      },
      "url": "http://127.0.0.1:8085/mcp"
    }
  }
}
```

- Stdio variant (no URL, transport stdio):

```jsonc
"mcp": {
  "servers": {
    "acl2-mcp-bridge-stdio": {
      "type": "stdio",
      "command": "sbcl",
      "args": [
        "--noinform",
        "--disable-debugger",
        "--non-interactive",
        "--eval",
        "(ql:quickload :acl2-mcp-bridge)",
        "--eval",
        "(progn (acl2-mcp-bridge:start-server :protocol :mcp :transport :stdio) (loop (sleep 1)))"
      ],
      "env": {
        "ASDF_SOURCE_REGISTRY": "(:source-registry (:tree \"/workspaces/acl2-mcp-bridge/\") :inherit-configuration)",
        "CL_SOURCE_REGISTRY": "/workspaces/acl2-mcp-bridge//:"
      }
    }
  }
}
```

Claude Desktop (stdio) example remains the same as above; omit `url` and use `:transport :stdio`.

### Shutdown behavior

- Bridge: `(acl2-mcp-bridge:stop-server :protocol :bridge)` closes the socket listener.
- MCP: the 40ants-mcp server has no public stop hook; restart the hosting process if you need to stop MCP.

### Environment and dependencies (development)

- Quicklisp + Ultralisp are expected so `40ants-mcp` resolves cleanly.
- `jsonrpc/transport/http` is not in the main Quicklisp dist; clone https://github.com/cxxxr/jsonrpc into `~/quicklisp/local-projects/` (already present in this workspace) to satisfy MCP HTTP transport.
- ASDF is configured to scan this repo via `ASDF_SOURCE_REGISTRY='(:source-registry (:tree "/workspaces/acl2-mcp-bridge/") :inherit-configuration)'`.
- ACL2 image available at `/opt/acl2/bin/saved_acl2`; `ACL2_PATH` can be set to override.
- Session IDs use UUID strings (see [sessions.lisp](sessions.lisp)).

### Development learnings

- `start-server` dispatches protocols; `start-both` simply calls both paths (see [main.lisp](main.lisp)).
- Message framing follows the Bridge spec: `TYPE len\ncontent\n` and is validated by tests (see [message-format.lisp](message-format.lisp) and [tests/message-format-tests.lisp](tests/message-format-tests.lisp)).
- MCP stop is intentionally unsupported because 40ants-mcp exposes no stop API; tests assert it signals an error (see [tests/startup-tests.lisp](tests/startup-tests.lisp)).
- FiveAM test system is wired via [acl2-mcp-bridge.asd](acl2-mcp-bridge.asd); tests mock server constructors with a local `with-redefs` helper to avoid starting real sockets.

### Session lifecycle (current)

Based on the ACL2 Bridge model: "When a client connects, it creates a new worker thread to handle the client's requests. The worker thread presents the client with a kind of read-eval-print loop."

- **Evaluation context**: Single in-process evaluation context per server instance. The context maintains:
  - Current package (defaults to CL-USER, can be changed via IN-PACKAGE)
  - User-defined functions and variables (persists across calls)
  - Tracking of defined symbols (for reset)

- **Isolation model**: The MCP server process provides the isolation boundary. Clients needing separate environments should spawn multiple server instances. This matches how the ACL2 Bridge handles isolation.

- **Reset capability**: Use `reset_cl` to clear user-defined symbols and reset package to CL-USER without restarting the server.

- **ACL2 sessions**: IDs are generated and stored, but lifecycle management is still stubbed; full ACL2 integration is pending (see [acl2-interface.lisp](acl2-interface.lisp)).

## MCP tool reference (current)

### CL tools (defined in [mcp-server.lisp](mcp-server.lisp))

| Tool | Args | Description |
|------|------|-------------|
| `eval_cl` | `code` (string) | Evaluate Common Lisp code. Returns result(s) or error. Multiple values separated by newlines. |
| `load_file` | `path` (string) | Load a .lisp source file into the evaluation context. |
| `define_function` | `name`, `lambda_list`, `body` (all strings) | Define a function. Equivalent to `(defun name lambda-list body)`. |
| `get_package` | (none) | Return the current evaluation package name. |
| `reset_cl` | (none) | Reset context: unbind user definitions, reset package to CL-USER. |

**Notes:**
- Results are returned as `text-content` items (per 40ants-mcp format)
- Package context persists across calls (use IN-PACKAGE to change)
- Defined symbols are tracked for `reset_cl`

### ACL2 tools (stubs, defined in [tools-acl2.lisp](tools-acl2.lisp))

| Tool | Args | Description |
|------|------|-------------|
| `admit` | `code`, `session_id`? | Admit an event (function/theorem) |
| `check_theorem` | `code`, `session_id`? | Verify a theorem |
| `verify_guards` | `name`, `session_id`? | Check guard conditions |
| `query_event` | `name`, `session_id`? | Retrieve event definition |

Full ACL2 integration is pending; these are placeholder stubs.

## Claude Desktop config (MCP stdio example)

- macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
- Windows: `%APPDATA%\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "acl2-bridge": {
      "command": "sbcl",
      "args": [
        "--eval",
        "(ql:quickload :acl2-mcp-bridge)",
        "--eval",
        "(acl2-mcp-bridge:start-server :protocol :mcp :transport :stdio)"
      ]
    }
  }
}
```

* With a custom ACL2 path:

```json
{
  "mcpServers": {
    "acl2-bridge": {
      "command": "sbcl",
      "args": [
        "--eval",
        "(setf (uiop:getenv \"ACL2_PATH\") \"/path/to/acl2\")",
        "--eval",
        "(ql:quickload :acl2-mcp-bridge)",
        "--eval",
        "(acl2-mcp-bridge:start-both)"
      ]
    }
  }
}
```

## Usage Examples (MCP tools)

### Evaluate Common Lisp:

```json
{
  "tool": "eval_cl",
  "arguments": {
    "code": "(+ 1 2 3)"
  }
}
```

### Define and call a function:

```json
{
  "tool": "define_function",
  "arguments": {
    "name": "fib",
    "lambda_list": "(n)",
    "body": "(if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))"
  }
}
```

```json
{
  "tool": "eval_cl",
  "arguments": {
    "code": "(fib 10)"
  }
}
```

### Reset to fresh state:

```json
{
  "tool": "reset_cl",
  "arguments": {}
}
```

  ## Running the Server

  ### Direct execution

  ```bash
  sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
    --eval '(acl2-mcp-bridge:start-server :protocol :bridge)' \
    --eval '(loop (sleep 1))'
  ```

  ### With buildapp (standalone executable)

  Create the binary:

  ```bash
  buildapp --output acl2-mcp-bridge \
        --load-system acl2-mcp-bridge \
        --eval '(acl2-mcp-bridge:start-server :protocol :bridge)' \
        --entry-point acl2-mcp-bridge:main
  ```

  Run it:

  ```bash
  ./acl2-mcp-bridge
  ```

  ## Testing

  ```bash
  sbcl --eval '(ql:quickload :acl2-mcp-bridge/tests)' \
    --eval '(acl2-mcp-bridge/tests::run-tests)' \
    --quit
  ```

  ## Architecture Notes

  ### Session management
  - Unique session IDs for ACL2 and CL
  - ACL2 subprocess hooks (stubbed) with output capture and timestamps
  - Cleanup and timeout hooks (extend in your implementation)

  ### Error handling
  - Tool calls wrapped for errors; add richer output/error propagation in `acl2-eval`/`acl2-event`
  - Logging via `log4cl`; extend for per-session tracing

  ### Transport flexibility
  - Primary: stdio (MCP)
  - Optional: HTTP (MCP) via 40ants-mcp
  - Bridge protocol: TCP listener for backward compatibility

  ## Bridge Feature Comparison

  | Feature | septract/acl2-mcp | ACL2 Bridge | This Implementation |
  |---------|------------------|-------------|--------------------|
  | Language | Python | Common Lisp | Common Lisp |
  | Session Support | ✓ | ✓ | ✓ |
  | ACL2 Tools | 15 tools | Custom interface | 15+ tools |
  | CL Integration | ✗ | Partial | ✓ Full |
  | MCP Compliance | ✓ | ✗ | ✓ |
  | Framework | FastMCP | Custom | 40ants-mcp |
  | Multi-Lisp Support | ✗ | Limited | ✓ (SBCL/CCL/ECL) |

  ## Contributing

  Contributions welcome. Focus areas:
  - Wire real ACL2 integration for `acl2-eval`, `acl2-event`, and friends
  - Performance and concurrency hardening
  - Extended documentation and examples
  - Test coverage

  ## License

  BSD 3-Clause License (compatible with ACL2 and 40ants-mcp)

  ## References

  - [ACL2 Documentation](https://www.cs.utexas.edu/~moore/acl2/)
  - [ACL2 GitHub](https://github.com/acl2/acl2)
  - [Model Context Protocol](https://modelcontextprotocol.io/)
  - [ACL2 Bridge book source](https://github.com/acl2/acl2/tree/master/books/centaur/bridge)
  - [40ants-mcp Framework](https://github.com/40ants/mcp)
  - [septract/acl2-mcp](https://github.com/septract/acl2-mcp)
    - [jimwhite/acl2-mcp](https://github.com/jimwhite/acl2-mcp) some improvements in ACL2 output

  ## Notes and next steps

  - ACL2 integration functions are stubbed; wire them to your ACL2 process or in-image ACL2 as needed.
  - Bridge protocol stubs cover message framing and per-connection threads; extend `run-in-main-thread` if ACL2 must stay on the main thread.
  - Add real error/output handling in `acl2-eval`, `acl2-event`, and friends to surface proof output in MCP responses.
