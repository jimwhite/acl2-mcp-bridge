# ACL2 MCP Bridge Server (`acl2-mcp-40ants`)

Multi-protocol ACL2 server that supports the legacy ACL2 Bridge protocol, modern Model Context Protocol (MCP) via 40ants-mcp, and a direct Common Lisp REPL. Works on SBCL, CCL, ECL, and other threaded Lisps.

## Project Structure

```text
acl2-mcp-bridge/
├── acl2-mcp-40ants.asd
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
├── mcp/
│   ├── server.lisp
│   └── tools.lisp
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
sbcl --eval '(ql:quickload :acl2-mcp-40ants)' --quit
```

## Run

Start Bridge protocol (default):

```lisp
(ql:quickload :acl2-mcp-40ants)
(acl2-mcp-40ants:start-server :protocol :bridge :port 13721)
```

Start MCP over stdio (good for MCP clients/agents):

```lisp
(ql:quickload :acl2-mcp-40ants)
(acl2-mcp-40ants:start-server :protocol :mcp :transport :stdio)
```

Start both protocols:

```lisp
(ql:quickload :acl2-mcp-40ants)
(acl2-mcp-40ants:start-both)
```

HTTP MCP (example):

```lisp
(acl2-mcp-40ants:start-server :protocol :mcp :transport :http :port 8085)
```

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
        "(ql:quickload :acl2-mcp-40ants)",
        "--eval",
        "(acl2-mcp-40ants:start-server :protocol :mcp :transport :stdio)"
      ]
    }
  }
}
```

With a custom ACL2 path:

```json
{
  "mcpServers": {
    "acl2-bridge": {
      "command": "sbcl",
      "args": [
        "--eval",
        "(setf (uiop:getenv \"ACL2_PATH\") \"/path/to/acl2\")",
        "--eval",
        "(ql:quickload :acl2-mcp-40ants)",
        "--eval",
        "(acl2-mcp-40ants:start-both)"
      ]
    }
  }
}
```

## Usage Examples (MCP tools)

Admit a theorem:

```json
{
  "tool": "admit",
  "arguments": {
    "code": "(defthm append-assoc (equal (append (append x y) z) (append x (append y z))))",
    "session_id": "session-1"
  }
}
```

Evaluate Common Lisp:

```json
{
  "tool": "eval-cl",
  "arguments": {
    "code": "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
    "session_id": "cl-session-1"
  }
}
```

Bridge ACL2 → CL:

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

## Notes and next steps

- ACL2 integration functions are stubbed; wire them to your ACL2 process or in-image ACL2 as needed.
- Bridge protocol stubs cover message framing and per-connection threads; extend `run-in-main-thread` if ACL2 must stay on the main thread.
- Add real error/output handling in `acl2-eval`, `acl2-event`, and friends to surface proof output in MCP responses.
