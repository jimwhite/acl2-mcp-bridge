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

## Installation

### Prerequisites
- SBCL, CCL, or ECL (Common Lisp implementations)
- ACL2 system installed and accessible
- Quicklisp (Common Lisp package manager)

### Setup

```bash
# Clone the repository
git clone https://github.com/jimwhite/acl2-mcp-bridge.git
cd acl2-mcp-bridge

# Install dependencies via Quicklisp
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' --quit
```

### Quicklisp Dependency Management

Add to your quicklisp local-projects or register in dist:

```lisp
(ql:quickload '(:40ants-mcp
               :cl-user                  ; Standard CL packages
               :uiop                     ; System operations
               :str                      ; String utilities
               :jonathan                 ; JSON (alternative to yason)
               :alexandria               ; Common utilities
               :bordeaux-threads         ; Thread support
               :usocket                  ; TCP/IP support))
```

## Configuration

### Claude Desktop

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
        "(acl2-mcp-bridge:start-server)"
      ]
    }
  }
}
```

### With Custom ACL2 Path

```json
{
  "mcpServers": {
    "acl2-bridge": {
      "command": "sbcl",
      "args": [
        "--eval",
        "(ql:quickload :acl2-mcp-bridge)",
        "--eval",
        "(acl2-mcp-bridge:start-server :acl2-path \"/path/to/acl2\")"
      ],
      "env": {
        "ACL2_PATH": "/path/to/acl2"
      }
    }
  }
}
```

## Usage Examples

### Proving a Theorem

```json
{
  "tool": "admit",
  "arguments": {
    "code": "(defthm append-assoc (equal (append (append x y) z) (append x (append y z))))",
    "session_id": "session-1"
  }
}
```

### Evaluating Common Lisp

```json
{
  "tool": "eval-cl",
  "arguments": {
    "code": "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
    "session_id": "cl-session-1"
  }
}
```

### Cross-Language Bridge

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

### Direct Execution

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge)' \
     --eval '(acl2-mcp-bridge:start-server)' \
     --eval '(loop (sleep 1))'
```

### With Buildapp (Standalone Executable)

# Create standalone executable

```bash
buildapp --output acl2-mcp-bridge \
         --load-system acl2-mcp-bridge \
         --eval '(acl2-mcp-bridge:start-server)' \
         --entry-point acl2-mcp-bridge:main
```

# Run it

```bash
./acl2-mcp-bridge
```

## Testing

# Run unit tests

```bash
sbcl --eval '(ql:quickload :acl2-mcp-bridge/tests)' \
     --eval '(run-tests)' \
     --quit
```

## Architecture Notes

### Session Management Design

Each session maintains:
- Unique session ID (UUID)
- ACL2 subprocess with persistent pipes
- Output buffer for capturing results
- Timeout handling and cleanup

### Error Handling Strategy

- All tool functions wrapped in handler-case
- Timeouts enforced via process management
- Graceful degradation with meaningful error messages
- Session recovery mechanisms

### Transport Flexibility

- Primary: STDIO (efficient, built-in)
- Secondary: HTTP (for remote access)
- Extensible for SSE and streaming

## Bridge Feature Comparison

| Feature | septract/acl2-mcp | ACL2 Bridge | This Implementation |
|---------|------------------|------------|-------------------|
| Language | Python | Common Lisp | Common Lisp |
| Session Support | ✓ | ✓ | ✓ |
| ACL2 Tools | 15 tools | Custom interface | 15+ tools |
| CL Integration | ✗ | Partial | ✓ Full |
| MCP Compliance | ✓ | ✗ | ✓ |
| Framework | FastMCP | Custom | 40ants-mcp |
| Multi-Lisp Support | ✗ | Limited | ✓ (SBCL/CCL/ECL) |

## Contributing

Contributions welcome! Focus areas:
- Additional ACL2 tools
- Performance optimization
- Extended documentation
- Test coverage

## License

BSD 3-Clause License (compatible with ACL2 and 40ants-mcp)

## References

- [ACL2 Documentation](https://www.cs.utexas.edu/~moore/acl2/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [40ants-mcp Framework](https://github.com/40ants/mcp)
- [septract/acl2-mcp](https://github.com/septract/acl2-mcp)
