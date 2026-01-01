# Copilot Instructions

## Context
- Project: ACL2 MCP Bridge server combining legacy ACL2 Bridge, 40ants-mcp, and Common Lisp REPL. Existing code is draft-quality; treat all implementations as potentially wrong.
- Reuse: Prefer proven ACL2 sources (ACL2 Bridge book, acl2-mcp, ACL2 system books at /home/acl2/books) over reinventing logic. Avoid diverging semantics from ACL2 where possible.

## Expectations
- Work in TDD style: add failing tests before fixes/features. Do not delete or disable tests; surface blockers instead of bypassing them.
- Keep implementations non-stubbed; no swallowing errors or suppressing output. If blocked by failing tests or missing dependencies, stop and report.
- Concurrency and sessions: respect ACL2 main-thread constraints; serialize ACL2 interactions when required.
- Protocols: maintain compatibility with both ACL2 Bridge and MCP (40ants-mcp). Be explicit about transport (stdio/http) assumptions.

## Coding Style
- Common Lisp: prefer clear, small functions; document non-obvious behavior. Avoid overusing global state; guard with locks where necessary. Preserve ASCII unless justified.
- Configuration: make defaults overridable via env/parameters. Avoid hardcoding ACL2 paths.
- Logging: use log4cl at appropriate levels; do not hide errors.

## Integration Notes
- Locate and reuse existing bridge message framing, session handling, and ACL2 invocation patterns from ACL2 books before writing new code.
- Ensure data translation between ACL2 and CL is faithful (avoid read/print ambiguity, handle packages carefully).

## Testing
- Add automated tests for session management, ACL2 interactions (with suitable fakes/mocks), and protocol framing. Prefer fast, deterministic tests.
- When adding features, include regression tests that capture expected ACL2/Bridge/MCP behaviors.

## Deliverables
- Keep documentation up to date (README, design notes, plan). Communicate assumptions and open questions.
