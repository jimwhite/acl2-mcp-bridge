# Design Document

## Overview

ACL2 MCP Bridge is an MCP server providing Common Lisp evaluation and ACL2 theorem proving capabilities. It implements the MCP 2025-11-25 specification with proper session management.

## Current State (January 2026)

### Working Features

- **MCP Streamable HTTP Transport**: Full compliance with MCP session management spec
  - Session ID assigned at `initialize` via `MCP-Session-Id` header
  - Client must include header on all subsequent requests
  - `HTTP DELETE` terminates session and cleans up resources
  
- **Per-Client Session Isolation**: Each session gets a private CL package
  - Package named `SESSION-<uuid>` 
  - All `defun`, `defvar`, etc. are local to that package
  - No definition leakage between concurrent clients
  - Package destroyed when session terminates

- **CL Evaluation Tools**: All functional
  - `eval_cl` - Evaluate arbitrary CL code
  - `load_file` - Load .lisp files
  - `define_function` - Define functions programmatically
  - `get_package` - Query current package
  - `reset_cl` - Recreate session package (fresh state)

- **Test Suite**: 38 unit tests passing (FiveAM)
  - Startup/dispatch tests
  - Session lifecycle tests
  - Session isolation tests (verifies private packages work)
  - MCP server construction tests

### Stubbed / In Progress

- **ACL2 Integration**: Tools defined but not wired to real ACL2
  - `admit`, `check_theorem`, `verify_guards`, `query_event`
  - ACL2 session management (`start_acl2_session`, etc.)
  - Needs: ACL2 process spawning, Bridge protocol I/O

- **Legacy Bridge Protocol**: Server skeleton exists
  - Message framing implemented and tested
  - Needs: Full ACL2 integration to be useful

## Key Design Decisions

### 1. Session Isolation via Packages

**Problem**: Multiple MCP clients connecting simultaneously would share `CL-USER`, causing definition conflicts.

**Solution**: Each session creates a private package `SESSION-<id>` that uses `:CL`. All evaluation happens in this package.

```lisp
;; Session creation
(defun create-session-package (session-id)
  (make-package (format nil "SESSION-~A" session-id) :use '(:cl)))

;; Evaluation binds *package*
(let ((*package* (cl-session-eval-package session)))
  (eval (read-from-string code)))
```

**Benefits**:
- True isolation - `(defun foo ...)` in session A creates `SESSION-A::foo`
- No cleanup needed - delete package destroys all symbols
- Simple - no subprocess overhead

### 2. MCP Session Management per Spec

**Spec requirement** (from MCP 2025-11-25):
- Server assigns session ID at initialization via `MCP-Session-Id` header
- Client includes it in all subsequent requests
- Server responds `400` if missing, `404` if expired
- Client sends `HTTP DELETE` to terminate

**Implementation**: Custom `session-http-transport` that wraps 40ants-mcp's HTTP transport with a Lack app that:
1. Detects `initialize` requests → generates session ID
2. Extracts `MCP-Session-Id` header on other requests
3. Binds `*current-session*` during request processing
4. Handles `DELETE` method for session termination

### 3. No Automatic Session Timeout

**Rationale**: ACL2 proof sessions can run for hours. Automatic timeout would kill long-running work.

**Cleanup strategy**:
- HTTP clients: Call `DELETE` when done (per MCP spec)
- STDIO clients: Process exit naturally cleans up
- Forgotten sessions: Accumulate until server restart (acceptable for dev server)

### 4. 40ants-mcp Integration

We use 40ants-mcp for MCP tool registration but bypass its `start-server` for HTTP transport because:
- `start-server` only accepts `:http` or `:stdio` keywords
- We need custom transport class for session handling
- Solution: Call internal `initialize-rpc-server` and `handle-message` via `find-symbol`

## File Responsibilities

| File | Purpose |
|------|---------|
| `main.lisp` | Entry points: `start-server`, `start-both`, `stop-server` |
| `mcp-server.lisp` | MCP tool definitions (define-tool macros) |
| `session-transport.lisp` | MCP HTTP session handling (the spec-compliant layer) |
| `sessions.lisp` | Session registry, package creation, `cl-eval` etc. |
| `acl2-interface.lisp` | ACL2 process management (stubbed) |
| `bridge-protocol.lisp` | Legacy Bridge TCP server |
| `tools-acl2.lisp` | ACL2-specific MCP tools |

## Next Steps

1. **ACL2 Process Integration**
   - Spawn ACL2 subprocess per session (or shared with serialized access)
   - Use Bridge book protocol for communication
   - Wire `admit`, `check_theorem`, etc. to real ACL2

2. **SSE Support** 
   - Currently returns 405 on GET
   - Needed for server-to-client notifications
   - Lower priority than ACL2 integration

3. **Bridge Protocol Integration Tests**
   - Test message framing over real sockets
   - Validate compatibility with existing Bridge clients

## Testing

```bash
# Unit tests
sbcl --eval '(asdf:load-system :acl2-mcp-bridge/tests)' \
     --eval '(5am:run! :acl2-mcp-bridge/tests)' --quit

# Integration tests  
./tests/mcp-test.sh
```

Key test: `CL-SESSION-ISOLATION` verifies that two sessions can define the same symbol with different values and see their own values.
