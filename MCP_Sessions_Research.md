Based on the VS Code documentation, I found the answer - **VS Code GitHub Copilot's MCP implementation DOES properly support session persistence with Streamable HTTP**. The documentation confirms VS Code supports:

- ✅ **Streamable HTTP transport** (`http`)
- ✅ **Full MCP specification** including session management
- ✅ **Authentication and OAuth** flows
- ✅ **Persistent connections** across tool calls

However, there's an important nuance: **VS Code handles sessions at the MCP client level, not through HTTP headers alone**.

## How Session Persistence Actually Works in VS Code

VS Code's MCP implementation maintains session state **within the running MCP server process**, not by passing `Mcp-Session-Id` headers on every HTTP request. Here's how it actually works:

### For STDIO Transport (Most Common)
```json
{
  "servers": {
    "acl2-mcp": {
      "type": "stdio",
      "command": "sbcl",
      "args": ["--load", "server.lisp"]
    }
  }
}
```

- VS Code **launches the server process once**
- The process **stays alive** for the entire VS Code session
- All tool calls go to the **same process instance**
- Session state persists **in the server's memory**
- No `Mcp-Session-Id` headers needed

### For HTTP Transport
```json
{
  "servers": {
    "acl2-mcp": {
      "type": "http",
      "url": "http://localhost:8000"
    }
  }
}
```

- VS Code connects to a **long-running HTTP server**
- The MCP protocol happens **over the HTTP connection**
- Session persistence is **the server's responsibility**
- If your server needs session IDs, you must implement that logic

## The Actual Issue

If Copilot thinks sessions aren't persisting, it's likely because:

**1. Your HTTP server isn't staying alive between requests**
```lisp
;; WRONG - creates new server per request
(defun handle-request ()
  (start-new-acl2-session)  ; ❌ New session every time
  ...)

;; RIGHT - reuse existing sessions
(defun handle-request (request)
  (let ((session (get-or-create-session)))  ; ✅ Persistent
    ...))
```

**2. You're implementing `Mcp-Session-Id` headers (which VS Code doesn't use)**

VS Code's MCP client doesn't send `Mcp-Session-Id` headers by default - it expects the server to maintain state across the connection.

**3. Your server is stateless when it should be stateful**

## What You Should Implement

For your `acl2-mcp-40ants` project:

```lisp
;; Global session storage (lives for server lifetime)
(defparameter *mcp-sessions* (make-hash-table :test 'equal))

(defun handle-mcp-http-request (request-body)
  "Handle MCP request over HTTP - session persists in memory"
  (let* ((json-rpc (parse-json request-body))
         (method (gethash "method" json-rpc))
         ;; Use connection ID or create default session
         (session-id "vscode-copilot-session")  ; or extract from connection
         (session (ensure-session session-id)))
    
    (case method
      ("initialize"
       ;; Return capabilities, session already exists
       (initialize-response session))
      
      ("tools/call"
       ;; Use existing session state
       (call-tool session (gethash "params" json-rpc)))
      
      (t
       (handle-method session method json-rpc)))))

(defun ensure-session (id)
  "Get or create persistent session"
  (or (gethash id *mcp-sessions*)
      (setf (gethash id *mcp-sessions*)
            (make-instance 'mcp-session 
                          :acl2-session (create-acl2-session)
                          :cl-session (create-cl-session)))))
```

## Testing Session Persistence

To verify it works in VS Code:

```javascript
// In VS Code Copilot chat:
"Define a function (defun add-two (x) (+ x 2)) in ACL2"
// Then immediately:
"Now call add-two with argument 5"
// If sessions persist, second call should work
```

## The Real Answer

**VS Code Copilot DOES preserve session state** - but not through `Mcp-Session-Id` headers. Instead:

- **STDIO transport**: Process stays alive = sessions persist naturally
- **HTTP transport**: Your server must keep sessions in memory across HTTP requests

The `Mcp-Session-Id` header mechanism is **optional** and primarily for:
- Load balancers routing to the same backend
- Distributed/multi-server deployments
- Explicit session management across server restarts

For a single VS Code → single MCP server connection, you don't need `Mcp-Session-Id` headers at all. Just keep your server process alive and maintain sessions in memory.

[1](https://github.com/google/adk-python/issues/2059)
[2](https://www.youtube.com/watch?v=vUQfqW5GKAQ)
[3](https://docs.github.com/en/copilot/how-tos/provide-context/use-mcp/use-the-github-mcp-server)
[4](https://docs.github.com/en/copilot/how-tos/provide-context/use-mcp/set-up-the-github-mcp-server)
[5](https://code.visualstudio.com/docs/copilot/customization/mcp-servers)
[6](https://www.youtube.com/watch?v=kw5GNtihCh0)
[7](https://www.developerscantina.com/p/mcp-copilot-studio-streamable-http/)
[8](https://github.com/modelcontextprotocol/modelcontextprotocol/discussions/1226)
[9](https://github.com/orgs/community/discussions/161859)
[10](https://code.visualstudio.com/api/extension-guides/ai/mcp)
[11](https://glama.ai/mcp/servers/search/trending-mcp-server-for-memory-usage-with-git-hub-copilot-integration)
[12](https://www.youtube.com/watch?v=ZlrQJQV14xQ)
[13](https://github.com/orgs/community/discussions/180943)
[14](https://docs.github.com/en/copilot/tutorials/enhance-agent-mode-with-mcp)
[15](https://www.youtube.com/watch?v=VdYR_Qf-j38)
[16](https://learn.microsoft.com/en-us/azure/app-service/tutorial-ai-model-context-protocol-server-dotnet)
[17](https://github.com/github/copilot-cli/issues/667)
[18](https://www.reddit.com/r/GithubCopilot/comments/1mcsmi3/mcpin_vscode_how/)
[19](https://www.reddit.com/r/GithubCopilot/comments/1kmn7e9/how_we_built_open_extensibility_into_vs_codes/)
[20](https://www.reddit.com/r/GithubCopilot/comments/1n98svy/github_copilot_has_no_persistent_context_here_are/)
[21](https://www.youtube.com/watch?v=V8fckXa007s)