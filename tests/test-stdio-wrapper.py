#!/usr/bin/env python3
"""
Test the ACL2 MCP stdio wrapper (acl2-mcp-stdio.py).

This tests the full stdio transport by:
1. Launching acl2-mcp-stdio.py as a subprocess
2. Sending JSON-RPC messages via stdin
3. Reading responses from stdout
"""

import argparse
import json
import subprocess
import sys
import time
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
PROJECT_DIR = SCRIPT_DIR.parent
WRAPPER_SCRIPT = PROJECT_DIR / "acl2-mcp-stdio.py"


def log(msg: str):
    print(f"[test] {msg}", file=sys.stderr, flush=True)


def send_message(proc: subprocess.Popen, msg: dict):
    """Send JSON-RPC message as newline-delimited JSON (MCP stdio format)."""
    data = json.dumps(msg)
    proc.stdin.write(data + "\n")
    proc.stdin.flush()


def read_message(proc: subprocess.Popen, timeout: float = 30.0) -> dict | None:
    """Read JSON-RPC message as newline-delimited JSON (MCP stdio format)."""
    import select
    
    # Wait for data to be available
    if not select.select([proc.stdout], [], [], timeout)[0]:
        return None
    
    # Read a single line of JSON
    line = proc.stdout.readline()
    if not line:
        return None
    
    return json.loads(line.strip())


class StdioWrapperTests:
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.proc = None
        self.passed = 0
        self.failed = 0
        self.results = []
    
    def start_wrapper(self):
        """Start the stdio wrapper subprocess."""
        log(f"Starting wrapper: {WRAPPER_SCRIPT}")
        self.proc = subprocess.Popen(
            [sys.executable, str(WRAPPER_SCRIPT)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0
        )
        
    def stop_wrapper(self):
        """Stop the stdio wrapper."""
        if self.proc:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.proc.kill()
                self.proc.wait()
    
    def record(self, name: str, passed: bool, detail: str = ""):
        """Record test result."""
        if passed:
            self.passed += 1
            status = "✓ PASS"
        else:
            self.failed += 1
            status = "✗ FAIL"
        self.results.append((name, passed, detail))
        print(f"  {status}: {name}")
        if detail and (self.verbose or not passed):
            print(f"         {detail}")
    
    def test_initialize(self) -> dict | None:
        """Test MCP initialize request."""
        send_message(self.proc, {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-11-25",
                "capabilities": {},
                "clientInfo": {"name": "stdio-test", "version": "1.0"}
            }
        })
        
        response = read_message(self.proc)
        if response is None:
            self.record("Initialize", False, "No response received")
            return None
        
        has_result = "result" in response
        self.record("Initialize", has_result, 
                   f"Got result" if has_result else f"Got: {response}")
        
        if has_result:
            # Send initialized notification
            send_message(self.proc, {
                "jsonrpc": "2.0",
                "method": "notifications/initialized"
            })
        
        return response if has_result else None
    
    def test_tools_list(self):
        """Test tools/list request."""
        send_message(self.proc, {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list"
        })
        
        response = read_message(self.proc)
        if response is None:
            self.record("Tools list", False, "No response received")
            return
        
        has_tools = "result" in response and "tools" in response.get("result", {})
        tool_count = len(response.get("result", {}).get("tools", []))
        self.record("Tools list", has_tools, f"{tool_count} tools found")
    
    def test_eval_cl(self):
        """Test eval_cl tool."""
        send_message(self.proc, {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "eval_cl",
                "arguments": {"code": "(+ 1 2 3)"}
            }
        })
        
        response = read_message(self.proc)
        if response is None:
            self.record("eval_cl: arithmetic", False, "No response received")
            return
        
        content = ""
        if "result" in response:
            content = response["result"].get("content", [{}])[0].get("text", "")
        
        passed = "6" in content
        self.record("eval_cl: arithmetic", passed, f"(+ 1 2 3) => {content[:50]}")
    
    def test_define_function(self):
        """Test define_function tool."""
        send_message(self.proc, {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "tools/call",
            "params": {
                "name": "define_function",
                "arguments": {
                    "name": "test-square",
                    "lambda_list": "(x)",
                    "body": "(* x x)"
                }
            }
        })
        
        response = read_message(self.proc)
        if response is None:
            self.record("define_function", False, "No response received")
            return
        
        passed = "result" in response
        self.record("define_function", passed)
        
        # Call the defined function
        send_message(self.proc, {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "tools/call",
            "params": {
                "name": "eval_cl",
                "arguments": {"code": "(test-square 7)"}
            }
        })
        
        response = read_message(self.proc)
        if response is None:
            self.record("Call defined function", False, "No response received")
            return
        
        content = ""
        if "result" in response:
            content = response["result"].get("content", [{}])[0].get("text", "")
        
        passed = "49" in content
        self.record("Call defined function", passed, f"(test-square 7) => {content[:50]}")
    
    def run_all(self):
        """Run all tests."""
        print("\n--- Initialize ---")
        init_result = self.test_initialize()
        if init_result is None:
            print("\nInitialize failed, cannot continue tests")
            return
        
        print("\n--- Tool Discovery ---")
        self.test_tools_list()
        
        print("\n--- eval_cl ---")
        self.test_eval_cl()
        
        print("\n--- define_function ---")
        self.test_define_function()
    
    def print_summary(self):
        """Print test summary."""
        total = self.passed + self.failed
        print("\n" + "=" * 60)
        print("TEST SUMMARY")
        print("=" * 60)
        for name, passed, detail in self.results:
            status = "✓ PASS" if passed else "✗ FAIL"
            print(f"  {status}: {name}")
        print("-" * 60)
        print(f"Total: {total}  Passed: {self.passed}  Failed: {self.failed}")
        print("=" * 60)


def main():
    parser = argparse.ArgumentParser(description="Test ACL2 MCP stdio wrapper")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="Enable verbose output")
    args = parser.parse_args()
    
    print("=" * 60)
    print("ACL2 MCP stdio Wrapper Tests")
    print("=" * 60)
    print(f"Wrapper: {WRAPPER_SCRIPT}")
    
    tests = StdioWrapperTests(verbose=args.verbose)
    
    try:
        tests.start_wrapper()
        
        # Just run the tests - wrapper handles its own startup
        tests.run_all()
        tests.print_summary()
        
        return 0 if tests.failed == 0 else 1
        
    except Exception as e:
        log(f"ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1
        
    finally:
        tests.stop_wrapper()


if __name__ == "__main__":
    sys.exit(main())
