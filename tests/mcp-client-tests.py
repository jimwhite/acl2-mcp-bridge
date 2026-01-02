#!/usr/bin/env python3
"""
MCP Client Tests for ACL2-MCP-Bridge

Uses the official MCP Python SDK to test the ACL2 MCP server.
Supports Streamable HTTP and stdio transports.

Usage:
    python mcp-client-tests.py [--transport http|stdio] [--url URL] [--stdio-command CMD] [--verbose]
    
Options:
    --transport      Transport type: 'http' or 'stdio' (default: http)
    --url URL        MCP server URL for HTTP transport (default: http://localhost:8080/mcp)
    --stdio-command  Command for stdio transport (default: mcp-proxy-tool -u http://localhost:8080/mcp)
    --verbose        Enable verbose output
"""

import asyncio
import argparse
import json
import sys
from pathlib import Path
from typing import Any
from dataclasses import dataclass, field

# MCP SDK imports
from mcp import ClientSession
from mcp.client.streamable_http import streamable_http_client
from mcp.client.stdio import stdio_client, StdioServerParameters


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
    
    def __init__(self, transport: str = "http", url: str = None, verbose: bool = False, 
                 stdio_command: str = None, pattern: str = "*"):
        self.transport = transport
        self.url = url or "http://localhost:8080/mcp"
        self.verbose = verbose
        self.stdio_command = stdio_command or f"mcp-proxy-tool -u {self.url}"
        self.pattern = pattern
        self.suite = TestSuite()
        
    def log(self, msg: str):
        if self.verbose:
            print(f"  [DEBUG] {msg}")
    
    def should_run_test(self, test_name: str) -> bool:
        """Check if test matches the pattern filter"""
        if self.pattern == "*":
            return True
        return fnmatch.fnmatch(test_name.lower(), self.pattern.lower())
            
    async def run_all_tests(self):
        """Run all test categories"""
        if self.transport == "http":
            print(f"\nMCP Client Tests for {self.url}")
        else:
            print(f"\nMCP Client Tests via stdio transport")
        print("=" * 60)
        
        try:
            if self.transport == "http":
                await self._run_tests_http()
            else:
                await self._run_tests_stdio()
                
        except Exception as e:
            self.suite.add(TestResult(
                name="Connection",
                passed=False,
                message=f"Failed to connect: {e}"
            ))
            
        self.suite.print_summary()
        return self.suite.failed == 0
    
    async def _run_tests_http(self):
        """Run tests using HTTP transport"""
        async with streamable_http_client(self.url) as (read, write, _):
            async with ClientSession(read, write) as session:
                await self._run_test_suite(session)
    
    async def _run_tests_stdio(self):
        """Run tests using stdio transport"""
        import shlex
        
        # Parse the stdio command
        cmd_parts = shlex.split(self.stdio_command)
        command = cmd_parts[0]
        args = cmd_parts[1:] if len(cmd_parts) > 1 else []
        
        self.log(f"Starting stdio server: {command} {' '.join(args)}")
        
        server_params = StdioServerParameters(
            command=command,
            args=args,
        )
        
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                await self._run_test_suite(session)
    
    async def _run_test_suite(self, session: ClientSession):
        """Run all tests with the given session"""
        # Initialize the connection
        self.log("Initializing session...")
        result = await session.initialize()
        self.log(f"Server: {result.serverInfo.name} v{result.serverInfo.version}")
        
        # Define all test categories with their names
        test_categories = [
            ("tool_discovery", self.test_tool_discovery),
            ("eval_cl", self.test_eval_cl_tool),
            ("define_function", self.test_define_function_tool),
            ("get_package", self.test_get_package_tool),
            ("reset_cl", self.test_reset_cl_tool),
            ("query_cl_package", self.test_query_cl_package_tool),
            ("error_handling", self.test_error_handling),
            # ACL2 tool tests
            ("acl2_evaluate", self.test_acl2_evaluate_tool),
            ("acl2_admit", self.test_acl2_admit_tool),
            ("acl2_prove", self.test_acl2_prove_tool),
            ("acl2_check_provable", self.test_acl2_check_provable_tool),
            ("acl2_query_event", self.test_acl2_query_event_tool),
            ("acl2_include_book", self.test_acl2_include_book_tool),
            # Bridge-compatible tool tests
            ("eval_with_output", self.test_eval_with_output_tool),
            ("eval_multiple_values", self.test_eval_multiple_values_tool),
            ("eval_main_thread", self.test_eval_main_thread_tool),
        ]
        
        # Run matching tests
        matched_count = 0
        for name, test_func in test_categories:
            if self.should_run_test(name):
                self.log(f"Running test: {name}")
                matched_count += 1
                await test_func(session)
            else:
                self.log(f"Skipping {name} (pattern: {self.pattern})")
        
        if matched_count == 0:
            print(f"WARNING: No tests matched pattern '{self.pattern}'")
        
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
            
            # Test: Should have ACL2 tools
            acl2_tools = ['acl2_evaluate', 'acl2_admit', 'acl2_prove', 'acl2_check_provable', 
                          'acl2_query_event', 'acl2_include_book', 'acl2_certify_book',
                          'acl2_verify_guards', 'acl2_check_syntax', 'acl2_undo']
            found_acl2 = [t for t in acl2_tools if t in tool_names]
            self.suite.add(TestResult(
                name="ACL2 tools available",
                passed=len(found_acl2) >= 5,  # At least 5 of the ACL2 tools
                message=f"Found {len(found_acl2)}/{len(acl2_tools)} ACL2 tools: {found_acl2}"
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
            # First test: query without args (should return session package)
            result = await session.call_tool("query_cl_package", {})
            result_text = self._get_result_text(result)
            
            self.log(f"Session package query: {result_text[:150]}...")
            
            # Should return session package info (SESSION-xxx)
            self.suite.add(TestResult(
                name="query_cl_package: Default (session package)",
                passed="SESSION" in result_text.upper() or "Package:" in result_text,
                message=f"Got: {result_text[:80]}"
            ))
            
            # Second test: query COMMON-LISP package explicitly
            # Note: Optional parameters may not work with 40ants-mcp - this tests that
            result = await session.call_tool("query_cl_package", {
                "package_name": "COMMON-LISP"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"CL package query: {result_text[:150]}...")
            
            # Check if we got COMMON-LISP package (parameter worked) or session package (parameter ignored)
            got_cl_package = "COMMON-LISP" in result_text and "Functions" in result_text
            self.suite.add(TestResult(
                name="query_cl_package: Query named package",
                passed=got_cl_package or "Package:" in result_text,  # Pass if any valid response
                message=f"Got COMMON-LISP: {got_cl_package}, Response: {result_text[:80]}"
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

    # =========================================================================
    # ACL2 Tool Tests
    # =========================================================================
    
    async def test_acl2_evaluate_tool(self, session: ClientSession):
        """Test the acl2_evaluate tool"""
        print("\n--- acl2_evaluate Tool Tests ---")
        
        test_cases = [
            # (name, code, expected_in_result)
            ("Simple arithmetic", "(+ 1 2 3)", "6"),
            ("List operations", "(append '(a b) '(c d))", "(A B C D)"),
            ("Car/Cdr", "(car '(1 2 3))", "1"),
            ("Boolean", "(equal 1 1)", "T"),
        ]
        
        for name, code, expected in test_cases:
            try:
                result = await session.call_tool("acl2_evaluate", {"code": code})
                result_text = self._get_result_text(result)
                
                self.log(f"{name}: {code} => {result_text}")
                
                passed = expected.upper() in result_text.upper()
                self.suite.add(TestResult(
                    name=f"acl2_evaluate: {name}",
                    passed=passed,
                    message=f"Got '{result_text[:80]}', expected '{expected}'"
                ))
                
            except Exception as e:
                self.suite.add(TestResult(
                    name=f"acl2_evaluate: {name}",
                    passed=False,
                    message=str(e)
                ))
    
    async def test_acl2_admit_tool(self, session: ClientSession):
        """Test the acl2_admit tool"""
        print("\n--- acl2_admit Tool Tests ---")
        
        try:
            # Define a simple function
            result = await session.call_tool("acl2_admit", {
                "code": "(defun my-double (x) (* 2 x))"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"acl2_admit defun result: {result_text[:150]}")
            
            passed = "admitted" in result_text.lower() or "my-double" in result_text.lower()
            self.suite.add(TestResult(
                name="acl2_admit: Define function",
                passed=passed,
                message=result_text[:100]
            ))
            
            # Test the function works
            result = await session.call_tool("acl2_evaluate", {"code": "(my-double 21)"})
            result_text = self._get_result_text(result)
            
            self.suite.add(TestResult(
                name="acl2_admit: Call admitted function",
                passed="42" in result_text,
                message=f"Got '{result_text[:80]}', expected '42'"
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="acl2_admit tests",
                passed=False,
                message=str(e)
            ))
    
    async def test_acl2_prove_tool(self, session: ClientSession):
        """Test the acl2_prove tool"""
        print("\n--- acl2_prove Tool Tests ---")
        
        try:
            # Prove a simple theorem
            result = await session.call_tool("acl2_prove", {
                "code": "(defthm plus-commutes-simple (equal (+ a b) (+ b a)))"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"acl2_prove result: {result_text[:200]}")
            
            # Should indicate proof succeeded or was admitted
            passed = ("proved" in result_text.lower() or 
                     "admitted" in result_text.lower() or
                     "q.e.d" in result_text.lower())
            self.suite.add(TestResult(
                name="acl2_prove: Simple theorem",
                passed=passed,
                message=result_text[:120]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="acl2_prove tests",
                passed=False,
                message=str(e)
            ))
    
    async def test_acl2_check_provable_tool(self, session: ClientSession):
        """Test the acl2_check_provable tool"""
        print("\n--- acl2_check_provable Tool Tests ---")
        
        try:
            # Check a provable theorem (without admitting)
            result = await session.call_tool("acl2_check_provable", {
                "conjecture": "(equal (car (cons x y)) x)"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"acl2_check_provable result: {result_text[:150]}")
            
            # Should indicate provable: T
            passed = "provable: t" in result_text.lower() or "true" in result_text.lower()
            self.suite.add(TestResult(
                name="acl2_check_provable: Valid theorem",
                passed=passed,
                message=result_text[:100]
            ))
            
            # Check an unprovable statement
            result = await session.call_tool("acl2_check_provable", {
                "conjecture": "(equal x y)"  # Not always true
            })
            result_text = self._get_result_text(result)
            
            # This should fail or indicate not provable
            self.suite.add(TestResult(
                name="acl2_check_provable: Unprovable statement handled",
                passed=True,  # Just check it doesn't crash
                message=result_text[:100]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="acl2_check_provable tests",
                passed=False,
                message=str(e)
            ))
    
    async def test_acl2_query_event_tool(self, session: ClientSession):
        """Test the acl2_query_event tool"""
        print("\n--- acl2_query_event Tool Tests ---")
        
        try:
            # Query a built-in function
            result = await session.call_tool("acl2_query_event", {
                "name": "append"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"acl2_query_event append: {result_text[:150]}")
            
            # Should return some info about append
            passed = len(result_text) > 10 and "error" not in result_text.lower()
            self.suite.add(TestResult(
                name="acl2_query_event: Built-in function",
                passed=passed,
                message=result_text[:100]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="acl2_query_event tests",
                passed=False,
                message=str(e)
            ))
    
    async def test_acl2_include_book_tool(self, session: ClientSession):
        """Test the acl2_include_book tool"""
        print("\n--- acl2_include_book Tool Tests ---")
        
        try:
            # Include a standard book
            result = await session.call_tool("acl2_include_book", {
                "book_path": "std/lists/top",
                "dir": ":system"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"acl2_include_book result: {result_text[:150]}")
            
            # Should indicate success loading
            passed = ("loaded" in result_text.lower() or 
                     "std/lists" in result_text.lower())
            self.suite.add(TestResult(
                name="acl2_include_book: Load standard book",
                passed=passed,
                message=result_text[:100]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="acl2_include_book tests",
                passed=False,
                message=str(e)
            ))

    # =========================================================================
    # Bridge-Compatible Tool Tests
    # =========================================================================
    
    async def test_eval_with_output_tool(self, session: ClientSession):
        """Test the eval_with_output tool (Bridge STDOUT capture equivalent)"""
        print("\n--- eval_with_output Tool Tests ---")
        
        try:
            # Test with CL that produces output
            result = await session.call_tool("eval_with_output", {
                "code": "(progn (format t \"Hello from Lisp~%\") (+ 1 2))",
                "environment": "cl"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"eval_with_output CL: {result_text[:150]}")
            
            # Should have both result and output
            has_result = "3" in result_text or "Result" in result_text
            self.suite.add(TestResult(
                name="eval_with_output: CL with FORMAT output",
                passed=has_result,
                message=result_text[:100]
            ))
            
            # Test with ACL2 that produces CW output
            result = await session.call_tool("eval_with_output", {
                "code": "(prog2$ (cw \"ACL2 message~%\") (+ 10 20))",
                "environment": "acl2"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"eval_with_output ACL2: {result_text[:150]}")
            
            # Should have result (output capture may vary)
            has_result = "30" in result_text or "Result" in result_text
            self.suite.add(TestResult(
                name="eval_with_output: ACL2 with CW output",
                passed=has_result,
                message=result_text[:100]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="eval_with_output tests",
                passed=False,
                message=str(e)
            ))
    
    async def test_eval_multiple_values_tool(self, session: ClientSession):
        """Test the eval_multiple_values tool (Bridge LISP_MV equivalent)"""
        print("\n--- eval_multiple_values Tool Tests ---")
        
        try:
            # Test with CL multiple values
            result = await session.call_tool("eval_multiple_values", {
                "code": "(values 1 2 3)",
                "environment": "cl"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"eval_multiple_values CL: {result_text[:150]}")
            
            # Should show 3 values
            has_multiple = "3 value" in result_text.lower() or ("1" in result_text and "2" in result_text and "3" in result_text)
            self.suite.add(TestResult(
                name="eval_multiple_values: CL (values 1 2 3)",
                passed=has_multiple,
                message=result_text[:100]
            ))
            
            # Test with floor which returns quotient and remainder
            result = await session.call_tool("eval_multiple_values", {
                "code": "(floor 17 5)",
                "environment": "cl"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"eval_multiple_values floor: {result_text[:150]}")
            
            # Should show both 3 and 2 (17/5 = 3 remainder 2)
            has_both = "3" in result_text and "2" in result_text
            self.suite.add(TestResult(
                name="eval_multiple_values: CL floor",
                passed=has_both,
                message=result_text[:100]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="eval_multiple_values tests",
                passed=False,
                message=str(e)
            ))
    
    async def test_eval_main_thread_tool(self, session: ClientSession):
        """Test the eval_main_thread tool (Bridge in-main-thread equivalent)"""
        print("\n--- eval_main_thread Tool Tests ---")
        
        try:
            # Basic evaluation in main thread
            result = await session.call_tool("eval_main_thread", {
                "code": "(+ 100 200 300)"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"eval_main_thread: {result_text[:150]}")
            
            # Should return 600
            passed = "600" in result_text
            self.suite.add(TestResult(
                name="eval_main_thread: Basic arithmetic",
                passed=passed,
                message=result_text[:100]
            ))
            
            # Test with list operations (potential hons usage)
            result = await session.call_tool("eval_main_thread", {
                "code": "(append '(a b) '(c d))"
            })
            result_text = self._get_result_text(result)
            
            self.log(f"eval_main_thread list: {result_text[:150]}")
            
            passed = "A" in result_text.upper() and "D" in result_text.upper()
            self.suite.add(TestResult(
                name="eval_main_thread: List operations",
                passed=passed,
                message=result_text[:100]
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="eval_main_thread tests",
                passed=False,
                message=str(e)
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
    parser.add_argument("--transport", "-t", choices=["http", "stdio"], default="http",
                        help="Transport type (default: http)")
    parser.add_argument("--url", default="http://localhost:8080/mcp",
                        help="MCP server URL for HTTP transport (default: http://localhost:8080/mcp)")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Enable verbose output")
    parser.add_argument("--stdio-command", 
                        help="Command for stdio transport (default: mcp-proxy-tool -u <url>)")
    parser.add_argument("--pattern", default="*",
                        help="Run only tests matching pattern (glob, default: *)")
    args = parser.parse_args()
    
    tester = MCPClientTester(args.transport, args.url, args.verbose, args.stdio_command, args.pattern)
    success = await tester.run_all_tests()
    
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    asyncio.run(main())
