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

- CL sessions: a missing `session_id` auto-creates a new session (default is "default") via `ensure-cl-session` (see [sessions.lisp](sessions.lisp)). There is currently **no explicit start/stop tool** for CL; garbage collection/expiry is not implemented yet.
- ACL2 sessions: IDs are generated and stored, but lifecycle management is still stubbed; tool calls accept `session_id` and will use the default when omitted (see [acl2-interface.lisp](acl2-interface.lisp)). Start/stop tooling is a planned addition.

## MCP tool reference (current)

- CL tools (API `cl-api`, defined in [tools-cl.lisp](tools-cl.lisp))
  - `eval-cl`: args `code` (string, required), `session_id` (string, optional, default "default"); returns printed result or error text.
  - `load-file`: args `path` (string, required), `session_id` (string, optional, default "default"); loads a Lisp file and reports success/error.
  - `define-function`: args `name` (string), `lambda_list` (string), `body` (string), `session_id` (string, optional); reads forms and interns a function in the session.
  - `list-sessions`: no args; returns the list of active CL session IDs.
  - `start-session`: no args; creates a new CL session and returns its id.
  - `stop-session`: args `session_id` (string, required); removes the CL session (no-op message if missing).

Notes
- Session IDs are plain UUID strings; the default CL session ID is "default" when not provided.
- Results are returned as a list of `text-content` items as required by 40ants-mcp.

- ACL2 tools (API `acl2-api`, defined in [tools-acl2.lisp](tools-acl2.lisp))
  - `start-session`: no args; creates a new ACL2 session and returns its id.
  - `stop-session`: args `session_id` (string, required); removes the ACL2 session (no-op message if missing).
  - `list-sessions`: no args; lists ACL2 sessions.
  - Other ACL2 tools (`admit`, `check-theorem`, `verify-guards`, `query-event`) accept `session_id` (default is "default"); full ACL2 integration is pending, but session lifecycle is explicit.

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

### Admit a theorem:

```json
{
  "tool": "admit",
  "arguments": {
    "code": "(defthm append-assoc (equal (append (append x y) z) (append x (append y z))))",
    "session_id": "session-1"
  }
}
```

### Evaluate Common Lisp:

```json
{
  "tool": "eval-cl",
  "arguments": {
    "code": "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
    "session_id": "cl-session-1"
  }
}
```

### Bridge ACL2 → CL:

```json
{
  "tool": "bridge-acl2-to-cl",
  "arguments": {
    "data": "(cons 1 (cons 2 (cons 3 nil)))",
    "acl2-session-id": "session-1",
    "cl-session-id": "cl-session-1"
  }
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
