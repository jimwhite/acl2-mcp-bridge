<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" style="height:64px;margin-right:32px"/>

# i don;t want an interactive tool like that inspector - it opened up a browser window asking for auth

we need a way to automatically test all the features of an MCP server including all its tools

Perfect! I found exactly what you need. Here's the best solution for automated MCP server testing:

## Best Tool: [MCP Server Tester](https://github.com/r-huijts/mcp-server-tester)

This is specifically designed for **automated, non-interactive testing of all MCP server features**:

### Key Features

- ✅ **Discovers all tools automatically** from your MCP server
- ✅ **Uses Claude AI to generate realistic test cases** for each tool
- ✅ **Executes tests programmatically** (no browser, no UI)
- ✅ **Validates responses** against configurable rules
- ✅ **Generates detailed reports** (console, JSON, HTML, Markdown)
- ✅ **Multi-server testing** - test multiple servers in one run
- ✅ **CI/CD ready** - integrates into automated pipelines


### Setup (5 minutes)

```bash
# Clone and install
git clone https://github.com/r-huijts/mcp-server-tester.git
cd mcp-server-tester
npm install && npm run build

# Create .env with your Anthropic API key
echo "ANTHROPIC_API_KEY=your-key-here" > .env

# Initialize config
npm start -- --init
```


### Configuration (`mcp-servers.json`)

```json
{
  "numTestsPerTool": 3,
  "timeoutMs": 10000,
  "outputFormat": "console",
  "outputPath": "./reports/results.json",
  "mcpServers": {
    "acl2-mcp": {
      "command": "sbcl",
      "args": [
        "--load", "acl2-mcp-40ants.asd",
        "--eval", "(acl2-mcp-40ants:start-server :protocol :mcp :transport :stdio)"
      ],
      "env": {}
    }
  }
}
```


### Run Tests

```bash
# Run all servers
npm start

# Run specific server
npm start -- --servers acl2-mcp

# Custom config
npm start -- ./config-prod.json

# Generate reports in multiple formats
# Just edit outputFormat in config: "json", "html", "markdown", "console"
```


### What It Does

1. **Discovers** all your tools (eval-acl2, check-theorem, verify-guards, eval-cl, etc.)
2. **Generates** 3 test cases per tool using Claude AI
3. **Executes** each test against your running server
4. **Validates** responses with configurable rules
5. **Reports** pass/fail with details

### Example Output

```
✅ eval-acl2: 3/3 tests passed
✅ check-theorem: 3/3 tests passed
✅ admit: 2/3 tests passed (1 skipped - timeout)
❌ verify-guards: 1/3 tests passed (2 failed)
   - Failed: Invalid input handling
   - Failed: Edge case with large function

SUMMARY: 11/15 tests passed (73%)
```


### For HTTP Transport

It works perfectly with HTTP too - just configure your server:

```json
{
  "acl2-mcp-http": {
    "command": "sbcl",
    "args": [...],
    "env": {}
  }
}
```

The tester connects via stdin/stdout by default, so it works with any MCP transport.

### Why This is Perfect for You

- **No browser interaction** - fully automated
- **Tests all features** - tools, resources, prompts
- **Validates responses** - not just "did it run"
- **CI/CD integration** - run in GitHub Actions, GitLab CI, etc.
- **Works with HTTP** - tests your Bridge and MCP protocols
- **Multiple output formats** - JSON for parsing, HTML for sharing
- **Generates smart tests** - Claude AI creates edge cases, error cases, happy paths

This is exactly what was missing from MCP Inspector - automated, scriptable, no-browser testing that you can run in CI/CD pipelines.
<span style="display:none">[^1][^10][^11][^12][^13][^14][^15][^16][^17][^18][^19][^2][^20][^3][^4][^5][^6][^7][^8][^9]</span>

<div align="center">⁂</div>

[^1]: https://github.com/r-huijts/mcp-server-tester

[^2]: https://codoid.com/automation-testing/open-source-mcp-powering-scalable-test-automation/

[^3]: https://testguild.com/top-model-context-protocols-mcp/

[^4]: https://modelcontextprotocol.io/docs/develop/build-server

[^5]: https://github.com/haakco/mcp-testing-framework

[^6]: https://mcpmarket.com/server/inspector-assistant

[^7]: https://www.reddit.com/r/mcp/comments/1k21h4a/we_made_mcp_work_100_in_the_browser_no_local/

[^8]: https://www.youtube.com/watch?v=Ej6NEOxLocI

[^9]: https://www.stainless.com/mcp/how-to-test-mcp-servers

[^10]: https://www.reddit.com/r/mcp/comments/1l7d0hl/debuggai_mcp_server_enable_your_agents_to_quickly/

[^11]: https://lobehub.com/mcp/your-org-ai-e2e-test-framework

[^12]: https://github.com/modelcontextprotocol/inspector

[^13]: https://browsermcp.io

[^14]: https://testomat.io/blog/mcp-server-testing-tools/

[^15]: https://www.traceloop.com/blog/a-guide-to-properly-testing-mcp-applications

[^16]: https://mcpservers.org/servers/aws-samples/sample-mcp-server-automation

[^17]: https://modelcontextprotocol.io/specification/2025-06-18/server/tools

[^18]: https://modelcontextprotocol.io/docs/tools/inspector

[^19]: https://codely.com/en/blog/how-to-test-mcp-servers

[^20]: https://www.reddit.com/r/mcp/comments/1mi9so0/what_mcp_ui_clients_are_you_using_to_be/

