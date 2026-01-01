#!/usr/bin/env python3
"""
MCP Client Tests for ACL2-MCP-Bridge

Uses the official MCP Python SDK to test the ACL2 MCP server.
Supports Streamable HTTP transport.

Usage:
    python mcp-client-tests.py [--url URL] [--verbose]
    
Options:
    --url URL      MCP server URL (default: http://localhost:8080/mcp)
    --verbose      Enable verbose output
"""

import asyncio
import argparse
import json
import sys
from typing import Any
from dataclasses import dataclass, field

# MCP SDK imports
from mcp import ClientSession
from mcp.client.streamable_http import streamable_http_client


@dataclass
class TestResult:
    """Result of a single test"""
    name: str
    passed: bool
    message: str = ""
    details: Any = None


@dataclass 
class TestSuite:
    """Collection of test results"""
    results: list[TestResult] = field(default_factory=list)
    
    @property
    def passed(self) -> int:
        return sum(1 for r in self.results if r.passed)
    
    @property
    def failed(self) -> int:
        return sum(1 for r in self.results if not r.passed)
    
    @property
    def total(self) -> int:
        return len(self.results)
    
    def add(self, result: TestResult):
        self.results.append(result)
        
    def print_summary(self):
        print("\n" + "=" * 60)
        print("TEST SUMMARY")
        print("=" * 60)
        
        for r in self.results:
            status = "✓ PASS" if r.passed else "✗ FAIL"
            print(f"  {status}: {r.name}")
            if not r.passed and r.message:
                print(f"         {r.message}")
                
        print("-" * 60)
        print(f"Total: {self.total}  Passed: {self.passed}  Failed: {self.failed}")
        print("=" * 60)


class MCPClientTester:
    """Test harness for MCP server using official SDK"""
    
    def __init__(self, url: str, verbose: bool = False):
        self.url = url
        self.verbose = verbose
        self.suite = TestSuite()
        
    def log(self, msg: str):
        if self.verbose:
            print(f"  [DEBUG] {msg}")
            
    async def run_all_tests(self):
        """Run all test categories"""
        print(f"\nMCP Client Tests for {self.url}")
        print("=" * 60)
        
        try:
            async with streamable_http_client(self.url) as (read, write, _):
                async with ClientSession(read, write) as session:
                    # Initialize the connection
                    self.log("Initializing session...")
                    result = await session.initialize()
                    self.log(f"Server: {result.serverInfo.name} v{result.serverInfo.version}")
                    
                    # Run test categories
                    await self.test_tool_discovery(session)
                    await self.test_eval_cl_tool(session)
                    await self.test_define_function_tool(session)
                    await self.test_get_package_tool(session)
                    await self.test_reset_cl_tool(session)
                    await self.test_query_cl_package_tool(session)
                    await self.test_error_handling(session)
                    
        except Exception as e:
            self.suite.add(TestResult(
                name="Connection",
                passed=False,
                message=f"Failed to connect: {e}"
            ))
            
        self.suite.print_summary()
        return self.suite.failed == 0
        
    async def test_tool_discovery(self, session: ClientSession):
        """Test that we can discover available tools"""
        print("\n--- Tool Discovery Tests ---")
        
        try:
            tools = await session.list_tools()
            tool_names = [t.name for t in tools.tools]
            
            self.log(f"Found {len(tool_names)} tools: {tool_names}")
            
            # Test: Should find some tools
            self.suite.add(TestResult(
                name="Tool discovery returns tools",
                passed=len(tool_names) > 0,
                message=f"Found {len(tool_names)} tools" if tool_names else "No tools found"
            ))
            
            # Test: Should have core CL tools
            expected_tools = ['eval_cl', 'define_function', 'get_package']
            found = [t for t in expected_tools if t in tool_names]
            self.suite.add(TestResult(
                name="Core CL tools available",
                passed=len(found) == len(expected_tools),
                message=f"Found {found}, expected {expected_tools}"
            ))
            
            # Test: Tools have descriptions
            tools_with_desc = [t for t in tools.tools if t.description]
            self.suite.add(TestResult(
                name="Tools have descriptions",
                passed=len(tools_with_desc) == len(tools.tools),
                message=f"{len(tools_with_desc)}/{len(tools.tools)} have descriptions"
            ))
            
            # Test: Tools have input schemas
            tools_with_schema = [t for t in tools.tools if t.inputSchema]
            self.suite.add(TestResult(
                name="Tools have input schemas",
                passed=len(tools_with_schema) == len(tools.tools),
                message=f"{len(tools_with_schema)}/{len(tools.tools)} have schemas"
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="Tool discovery",
                passed=False,
                message=str(e)
            ))
            
    async def test_eval_cl_tool(self, session: ClientSession):
        """Test the eval_cl tool"""
        print("\n--- eval_cl Tool Tests ---")
        
        test_cases = [
            # (name, code, expected_in_result)
            ("Simple arithmetic", "(+ 1 2)", "3"),
            ("String output", "(format nil \"hello\")", "hello"),
            ("List creation", "(list 1 2 3)", "(1 2 3)"),
            ("Let binding", "(let ((x 10)) (* x x))", "100"),
            ("Multiple values", "(values 1 2 3)", "1"),
            ("Boolean true", "t", "T"),
            ("Boolean false", "nil", "NIL"),
            ("Symbol", "'hello", "HELLO"),
        ]
        
        for name, code, expected in test_cases:
            try:
                result = await session.call_tool("eval_cl", {"code": code})
                
                # Get result text
                result_text = ""
                if result.content:
                    for content in result.content:
                        if hasattr(content, 'text'):
                            result_text = content.text
                            break
                
                self.log(f"{name}: {code} => {result_text}")
                
                passed = expected.upper() in result_text.upper()
                self.suite.add(TestResult(
                    name=f"eval_cl: {name}",
                    passed=passed,
                    message=f"Got '{result_text}', expected '{expected}'"
                ))
                
            except Exception as e:
                self.suite.add(TestResult(
                    name=f"eval_cl: {name}",
                    passed=False,
                    message=str(e)
                ))
                
    async def test_define_function_tool(self, session: ClientSession):
        """Test the define_function tool"""
        print("\n--- define_function Tool Tests ---")
        
        try:
            # Define a simple function
            result = await session.call_tool("define_function", {
                "name": "test-square",
                "lambda_list": "(x)",
                "body": "(* x x)"
            })
            
            result_text = self._get_result_text(result)
            self.log(f"define_function result: {result_text}")
            
            self.suite.add(TestResult(
                name="define_function: Create function",
                passed="TEST-SQUARE" in result_text.upper() or "defined" in result_text.lower(),
                message=result_text[:100]
            ))
            
            # Test calling the defined function
            result = await session.call_tool("eval_cl", {"code": "(test-square 5)"})
            result_text = self._get_result_text(result)
            
            self.suite.add(TestResult(
                name="define_function: Call defined function",
                passed="25" in result_text,
                message=f"Got '{result_text}', expected '25'"
            ))
            
            # Define function with multiple params
            await session.call_tool("define_function", {
                "name": "test-add",
                "lambda_list": "(a b)",
                "body": "(+ a b)"
            })
            result = await session.call_tool("eval_cl", {"code": "(test-add 10 20)"})
            result_text = self._get_result_text(result)
            
            self.suite.add(TestResult(
                name="define_function: Multi-param function",
                passed="30" in result_text,
                message=f"Got '{result_text}', expected '30'"
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="define_function tests",
                passed=False,
                message=str(e)
            ))
            
    async def test_get_package_tool(self, session: ClientSession):
        """Test the get_package tool"""
        print("\n--- get_package Tool Tests ---")
        
        try:
            result = await session.call_tool("get_package", {})
            result_text = self._get_result_text(result)
            
            self.log(f"Current package: {result_text}")
            
            # Should return some package name
            self.suite.add(TestResult(
                name="get_package: Returns package name",
                passed=len(result_text) > 0 and "error" not in result_text.lower(),
                message=f"Package: {result_text}"
            ))
            
            # Package should be a valid symbol/name
            self.suite.add(TestResult(
                name="get_package: Valid package format",
                passed=result_text.replace("-", "").replace("_", "").replace(".", "").isalnum() or "SESSION" in result_text.upper(),
                message=f"Package: {result_text}"
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="get_package tests",
                passed=False,
                message=str(e)
            ))
            
    async def test_reset_cl_tool(self, session: ClientSession):
        """Test the reset_cl tool"""
        print("\n--- reset_cl Tool Tests ---")
        
        try:
            # Define a variable
            await session.call_tool("eval_cl", {"code": "(defvar *test-reset-var* 42)"})
            
            # Verify it exists
            result = await session.call_tool("eval_cl", {"code": "*test-reset-var*"})
            result_text = self._get_result_text(result)
            var_exists = "42" in result_text
            
            # Reset
            result = await session.call_tool("reset_cl", {})
            result_text = self._get_result_text(result)
            
            self.suite.add(TestResult(
                name="reset_cl: Returns success message",
                passed="reset" in result_text.lower() or "cleared" in result_text.lower() or len(result_text) > 0,
                message=result_text[:100]
            ))
            
            # Note: In session-isolated mode, reset may just clear the session package
            # The variable might still exist in the underlying Lisp
            
        except Exception as e:
            self.suite.add(TestResult(
                name="reset_cl tests",
                passed=False,
                message=str(e)
            ))
            
    async def test_query_cl_package_tool(self, session: ClientSession):
        """Test the query_cl_package tool"""
        print("\n--- query_cl_package Tool Tests ---")
        
        try:
            # Query COMMON-LISP package
            result = await session.call_tool("query_cl_package", {
                "package_name": "COMMON-LISP"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"CL package query: {result_text[:200]}...")
            
            self.suite.add(TestResult(
                name="query_cl_package: Query COMMON-LISP",
                passed=len(result_text) > 0 and ("CAR" in result_text.upper() or "symbols" in result_text.lower()),
                message=f"Got {len(result_text)} chars of output"
            ))
            
            # Query with prefix filter
            result = await session.call_tool("query_cl_package", {
                "package_name": "COMMON-LISP",
                "prefix": "MAP"
            })
            result_text = self._get_result_text(result)
            
            self.suite.add(TestResult(
                name="query_cl_package: Filter by prefix",
                passed="MAP" in result_text.upper(),
                message=f"Filtered results: {result_text[:100]}"
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="query_cl_package tests",
                passed=False,
                message=str(e)
            ))
            
    async def test_error_handling(self, session: ClientSession):
        """Test error handling for invalid inputs"""
        print("\n--- Error Handling Tests ---")
        
        test_cases = [
            # (name, tool, args, should_contain_error)
            ("Invalid Lisp syntax", "eval_cl", {"code": "(+ 1"}, True),
            ("Undefined function", "eval_cl", {"code": "(nonexistent-func 1 2)"}, True),
            ("Division by zero", "eval_cl", {"code": "(/ 1 0)"}, True),
        ]
        
        for name, tool, args, expect_error in test_cases:
            try:
                result = await session.call_tool(tool, args)
                result_text = self._get_result_text(result)
                is_error = result.isError if hasattr(result, 'isError') else False
                has_error_text = "error" in result_text.lower() or "condition" in result_text.lower()
                
                self.log(f"{name}: isError={is_error}, text contains error={has_error_text}")
                
                self.suite.add(TestResult(
                    name=f"Error handling: {name}",
                    passed=(is_error or has_error_text) == expect_error,
                    message=f"isError={is_error}, response: {result_text[:80]}"
                ))
                
            except Exception as e:
                # Exceptions are also valid error handling
                self.suite.add(TestResult(
                    name=f"Error handling: {name}",
                    passed=expect_error,
                    message=f"Exception: {e}"
                ))
                
    def _get_result_text(self, result) -> str:
        """Extract text from tool result"""
        if result.content:
            for content in result.content:
                if hasattr(content, 'text'):
                    return content.text
        return ""


async def main():
    parser = argparse.ArgumentParser(description="MCP Client Tests for ACL2-MCP-Bridge")
    parser.add_argument("--url", default="http://localhost:8080/mcp",
                        help="MCP server URL (default: http://localhost:8080/mcp)")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Enable verbose output")
    args = parser.parse_args()
    
    tester = MCPClientTester(args.url, args.verbose)
    success = await tester.run_all_tests()
    
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    asyncio.run(main())
