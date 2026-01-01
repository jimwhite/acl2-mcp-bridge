# Plan and Design

## Objective
Build a reliable ACL2 bridge server that supports the legacy Bridge protocol, Model Context Protocol (MCP) via 40ants-mcp, and a CL REPL, reusing proven ACL2 components (ACL2 Bridge, acl2-mcp, and ACL2 system books). Development must be TDD-first with no reliance on draft stubs.

## Current State (survey)
- ACL2 integration remains stubbed: [acl2-interface.lisp](acl2-interface.lisp) defines session plumbing and UUID IDs but no real ACL2 I/O yet.
- Bridge protocol server is present and framed correctly; message framing is now covered by tests (see [bridge-protocol.lisp](bridge-protocol.lisp) and [tests/message-format-tests.lisp](tests/message-format-tests.lisp)).
- MCP tool registration lives in [tools-acl2.lisp](tools-acl2.lisp), [tools-cl.lisp](tools-cl.lisp), and [tools-bridge.lisp](tools-bridge.lisp); [mcp-tools.lisp](mcp-tools.lisp) is no longer loaded by the system.
- Server startup is unified: [main.lisp](main.lisp) exports `start-server`, `start-both`, and `stop-server`, delegating to [mcp-server.lisp](mcp-server.lisp) for MCP startup. MCP stop is intentionally unsupported (no API in 40ants-mcp); tests assert the error behavior.
- Threading helpers are still stubbed (see [threading-utils.lisp](threading-utils.lisp)); ACL2 main-thread serialization remains to be built.
- FiveAM test system exists with two suites: startup dispatch/stop coverage and bridge message framing (see [tests/startup-tests.lisp](tests/startup-tests.lisp)).
- Environment notes: requires Ultralisp for 40ants-mcp and a local clone of `cxxxr/jsonrpc` to provide `jsonrpc/transport/http` for MCP HTTP transport.

## Recent Work / Learnings
- Added FiveAM test system wired via ASDF; tests mock server entry points with a local `with-redefs` helper to avoid opening sockets.
- Reordered [acl2-mcp-bridge.asd](acl2-mcp-bridge.asd) so API tool definitions load before the MCP server; removed `mcp-tools` from the system to avoid duplicate registrations.
- Fixed Bridge message framing to preserve newline expectations and validated with round-trip tests.
- Clarified shutdown semantics: Bridge stop closes the listener; MCP stop is unsupported and should be handled by process restart.
- Session IDs use stringified UUIDs for both ACL2 and CL registries.

## External Assets to Reuse
- ACL2 Bridge book: `/home/acl2/books/centaur/bridge/` provides battle-tested bridge framing and ACL2 orchestration.
- ACL2 system books for utilities and patterns (session handling, process orchestration) in `/home/acl2/books`.
- Prior acl2-mcp implementation(s) for MCP-specific tool wiring; review before re-implementing logic.

## Design Direction
1. **Single entry point**: Consolidate server startup API (one exported `start-server`/`start-both`) that configures both Bridge and MCP, avoiding duplicate definitions.
2. **Session management**: Maintain ACL2 and CL session registries with locks. Expose lifecycle operations (create/list/cleanup). Guard ACL2 access through a main-thread execution path.
3. **ACL2 integration**: Implement ACL2 invocation by reusing the Bridge book process manager or in-image ACL2 where available. Provide adapters for:
   - Evaluating events/queries with output capture.
   - Translating data between ACL2 and CL safely (package-aware, no read/print ambiguity).
4. **Bridge protocol**: Validate framing, HELLO/READY/RETURN/ERROR semantics, and error handling. Add socket tests to prevent regressions.
5. **MCP layer**: Define OpenRPC schemas once, register tools consistently (ACL2, CL, bridge), and ensure responses include structured success/error data.
6. **Threading**: Implement `run-in-main-thread` using a worker channel or condition variable to serialize ACL2 actions when required.

## ACL2 Adapter + Threading Design Draft
- **Executable discovery**: Use `initialize-acl2-interface` to record the ACL2 binary path (arg > `$ACL2_PATH` > default "acl2"). Expose `*acl2-executable*` for process launch.
- **Bridge reuse**: Lift the proven worker loop and framing from the ACL2 Bridge book at `/home/acl2/books/centaur/bridge/` (notably `bridge-raw.lsp`/`top.lisp`) instead of re-implementing nld/ld plumbing. Keep message framing identical (type length newline, content newline) and add round-trip tests.
- **Process model**: Start a managed ACL2 process per ACL2 session or share one with serialized access. Capture stdout/stderr via pipes; surface outputs in tool responses. Favor a single ACL2 instance initially to reduce complexity.
- **Thread serialization**: Back `run-in-main-thread` with a dedicated executor thread and a bounded queue (e.g., `bt:condition-variable` + `bt:mutex`). Bridge and MCP requests enqueue ACL2 forms; executor runs them in ACL2-safe context, returning values/output.
- **Data flow**: For ACL2 events (`defun`, `defthm`), feed forms through ACL2's `ld` or Bridge helpers; for queries, evaluate within the session, returning structured success/error plus captured output. Avoid `read-from-string` ambiguity by controlling *package* and print/read settings.
- **Testing strategy**: Start with a fake ACL2 backend (deterministic responses) to drive tool/bridge behavior; then gate real ACL2 integration tests that spawn the executable and assert round-trip behavior on small forms.

## TDD Plan
- Adopt a Common Lisp test library (e.g., FiveAM) and wire it into ASDF and CI later.
- Add fast unit tests for:
  - Bridge message framing (send/read round-trips, error cases).
  - Session registry behaviors (create/list/cleanup, locking).
  - MCP tool argument/response shapes with fakes for ACL2.
  - ACL2 process adapter using a controlled fake ACL2 or minimal real ACL2 session (skip if environment unavailable; mark clearly when blocked).
  - Integration: bridge protocol over loopback with a stub ACL2 backend.

## Immediate Next Steps
1. Implement ACL2 adapter backed by the Bridge book or a managed ACL2 process; surface real outputs/errors in tool responses.
2. Add session-registry and ACL2/CL adapter tests (including locking semantics and main-thread serialization once implemented).
3. Add MCP tool contract tests (OpenRPC shapes) with a fake ACL2 backend to pin request/response formats.
4. Introduce integration tests for Bridge and MCP over loopback once real ACL2 plumbing lands; keep a skip/marker for environments lacking ACL2.
5. Decide whether `mcp-tools.lisp` should be deleted or merged into the active tool definitions to avoid drift.
