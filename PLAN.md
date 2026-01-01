# Plan and Design

## Objective
Build a reliable ACL2 bridge server that supports the legacy Bridge protocol, Model Context Protocol (MCP) via 40ants-mcp, and a CL REPL, reusing proven ACL2 components (ACL2 Bridge, acl2-mcp, and ACL2 system books). Development must be TDD-first with no reliance on draft stubs.

## Current State (survey)
- Core functions for ACL2 interaction are stubs (see [acl2-interface.lisp](acl2-interface.lisp)). Session structs exist but have no real ACL2 I/O.
- Bridge protocol server exists but depends on stubbed ACL2 eval (see [bridge-protocol.lisp](bridge-protocol.lisp)).
- MCP tool registration is inconsistent: MCP tools are defined in [tools-acl2.lisp](tools-acl2.lisp), [tools-cl.lisp](tools-cl.lisp), [tools-bridge.lisp](tools-bridge.lisp), and additional draft registrations in [mcp-tools.lisp](mcp-tools.lisp). No tests assert API shapes.
- Duplicate `start-server` entry points in [main.lisp](main.lisp) and [mcp-server.lisp](mcp-server.lisp); unclear which should be public.
- Threading helpers are stubbed (see [threading-utils.lisp](threading-utils.lisp)), so ACL2 main-thread constraints are not honored.
- No automated tests present in this repo.

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

## TDD Plan
- Adopt a Common Lisp test library (e.g., FiveAM) and wire it into ASDF and CI later.
- Add fast unit tests for:
  - Bridge message framing (send/read round-trips, error cases).
  - Session registry behaviors (create/list/cleanup, locking).
  - MCP tool argument/response shapes with fakes for ACL2.
  - ACL2 process adapter using a controlled fake ACL2 or minimal real ACL2 session (skip if environment unavailable; mark clearly when blocked).
  - Integration: bridge protocol over loopback with a stub ACL2 backend.

## Immediate Next Steps
1. Choose and wire a test framework; add ASDF test system skeleton.
2. Reconcile startup API (remove duplication between main and mcp-server) under tests.
3. Implement bridge message framing tests and harden code accordingly.
4. Sketch ACL2 adapter using the Bridge book; prototype with a fake backend to unlock tests.
5. Add MCP tool contract tests to pin down OpenRPC definitions before implementing behavior.
