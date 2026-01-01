#!/usr/bin/env python3
"""
Unified Protocol Test Suite for ACL2-MCP-Bridge

Tests both MCP and Bridge protocols over multiple transports:
- MCP over HTTP (Streamable HTTP)
- MCP over TCP socket (raw JSON-RPC)
- MCP over Unix socket (raw JSON-RPC)
- Bridge over TCP socket
- Bridge over Unix socket

Usage:
    python protocol-tests.py [OPTIONS]

Options:
    --protocol mcp|bridge|all    Protocol to test (default: all)
    --transport http|tcp|unix|all  Transport to test (default: all)
    --host HOST                  TCP host (default: localhost)
    --port PORT                  TCP port (default: 8080 for MCP, 5000 for Bridge)
    --socket PATH                Unix socket path
    --verbose                    Verbose output
"""

import asyncio
import argparse
import json
import socket
import struct
import sys
import os
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Optional
from pathlib import Path

# Optional: MCP SDK for HTTP transport
try:
    from mcp import ClientSession
    from mcp.client.streamable_http import streamable_http_client
    HAS_MCP_SDK = True
except ImportError:
    HAS_MCP_SDK = False
    print("Note: MCP SDK not installed, HTTP transport tests will use raw HTTP")


# =============================================================================
# Test Infrastructure
# =============================================================================

@dataclass
class TestResult:
    """Result of a single test"""
    name: str
    passed: bool
    message: str = ""
    protocol: str = ""
    transport: str = ""


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
    
    def add(self, result: TestResult):
        self.results.append(result)
        status = "✓" if result.passed else "✗"
        print(f"  {status} [{result.protocol}/{result.transport}] {result.name}")
        if not result.passed and result.message:
            print(f"      {result.message}")
    
    def print_summary(self):
        print("\n" + "=" * 70)
        print("TEST SUMMARY")
        print("=" * 70)
        
        # Group by protocol/transport
        by_category = {}
        for r in self.results:
            key = f"{r.protocol}/{r.transport}"
            if key not in by_category:
                by_category[key] = {"passed": 0, "failed": 0}
            if r.passed:
                by_category[key]["passed"] += 1
            else:
                by_category[key]["failed"] += 1
        
        for cat, counts in sorted(by_category.items()):
            total = counts["passed"] + counts["failed"]
            print(f"  {cat}: {counts['passed']}/{total} passed")
        
        print("-" * 70)
        print(f"Total: {len(self.results)}  Passed: {self.passed}  Failed: {self.failed}")
        print("=" * 70)


# =============================================================================
# Transport Abstractions
# =============================================================================

class Transport(ABC):
    """Abstract base for protocol transports"""
    
    @abstractmethod
    async def connect(self):
        pass
    
    @abstractmethod
    async def send(self, data: bytes) -> bytes:
        pass
    
    @abstractmethod
    async def close(self):
        pass


class TCPTransport(Transport):
    """TCP socket transport"""
    
    def __init__(self, host: str, port: int):
        self.host = host
        self.port = port
        self.reader: Optional[asyncio.StreamReader] = None
        self.writer: Optional[asyncio.StreamWriter] = None
        
    async def connect(self):
        self.reader, self.writer = await asyncio.open_connection(self.host, self.port)
        
    async def send(self, data: bytes) -> bytes:
        if not self.writer:
            raise RuntimeError("Not connected")
        self.writer.write(data)
        await self.writer.drain()
        return await self.reader.read(65536)
    
    async def send_line(self, data: str) -> str:
        """Send line-based message (for MCP JSON-RPC)"""
        if not self.writer:
            raise RuntimeError("Not connected")
        self.writer.write((data + "\n").encode())
        await self.writer.drain()
        response = await self.reader.readline()
        return response.decode().strip()
    
    async def close(self):
        if self.writer:
            self.writer.close()
            await self.writer.wait_closed()


class UnixTransport(Transport):
    """Unix domain socket transport"""
    
    def __init__(self, path: str):
        self.path = path
        self.reader: Optional[asyncio.StreamReader] = None
        self.writer: Optional[asyncio.StreamWriter] = None
        
    async def connect(self):
        self.reader, self.writer = await asyncio.open_unix_connection(self.path)
        
    async def send(self, data: bytes) -> bytes:
        if not self.writer:
            raise RuntimeError("Not connected")
        self.writer.write(data)
        await self.writer.drain()
        return await self.reader.read(65536)
    
    async def send_line(self, data: str) -> str:
        """Send line-based message (for MCP JSON-RPC)"""
        if not self.writer:
            raise RuntimeError("Not connected")
        self.writer.write((data + "\n").encode())
        await self.writer.drain()
        response = await self.reader.readline()
        return response.decode().strip()
    
    async def close(self):
        if self.writer:
            self.writer.close()
            await self.writer.wait_closed()


# =============================================================================
# Bridge Protocol Client
# =============================================================================

class BridgeClient:
    """Client for ACL2 Bridge protocol"""
    
    def __init__(self, transport: Transport):
        self.transport = transport
        self.request_id = 0
        
    async def connect(self):
        await self.transport.connect()
        
    async def close(self):
        await self.transport.close()
        
    def _make_request(self, code: str) -> bytes:
        """Create a Bridge protocol request packet"""
        self.request_id += 1
        # Bridge protocol: 4-byte length prefix (big-endian) + JSON
        request = {
            "id": self.request_id,
            "type": "eval",
            "code": code
        }
        json_bytes = json.dumps(request).encode('utf-8')
        length = struct.pack('>I', len(json_bytes))
        return length + json_bytes
    
    def _parse_response(self, data: bytes) -> dict:
        """Parse a Bridge protocol response"""
        if len(data) < 4:
            raise ValueError(f"Response too short: {len(data)} bytes")
        length = struct.unpack('>I', data[:4])[0]
        json_bytes = data[4:4+length]
        return json.loads(json_bytes.decode('utf-8'))
    
    async def eval(self, code: str) -> dict:
        """Evaluate code via Bridge protocol"""
        request = self._make_request(code)
        response = await self.transport.send(request)
        return self._parse_response(response)


# =============================================================================
# MCP Protocol Client (Socket-based)
# =============================================================================

class MCPSocketClient:
    """Client for MCP protocol over raw sockets (JSON-RPC)"""
    
    def __init__(self, transport: Transport):
        self.transport = transport
        self.request_id = 0
        
    async def connect(self):
        await self.transport.connect()
        
    async def close(self):
        await self.transport.close()
        
    async def _send_jsonrpc(self, method: str, params: dict = None) -> dict:
        """Send JSON-RPC request and get response"""
        self.request_id += 1
        request = {
            "jsonrpc": "2.0",
            "id": self.request_id,
            "method": method,
        }
        if params:
            request["params"] = params
            
        response_str = await self.transport.send_line(json.dumps(request))
        if not response_str:
            raise RuntimeError("Empty response")
        return json.loads(response_str)
    
    async def initialize(self) -> dict:
        """Initialize MCP session"""
        return await self._send_jsonrpc("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "protocol-tests",
                "version": "1.0.0"
            }
        })
    
    async def list_tools(self) -> dict:
        """List available tools"""
        return await self._send_jsonrpc("tools/list", {})
    
    async def call_tool(self, name: str, arguments: dict) -> dict:
        """Call a tool"""
        return await self._send_jsonrpc("tools/call", {
            "name": name,
            "arguments": arguments
        })


# =============================================================================
# MCP HTTP Client (using SDK)
# =============================================================================

class MCPHTTPClient:
    """Client for MCP over Streamable HTTP"""
    
    def __init__(self, url: str):
        self.url = url
        self.session: Optional[ClientSession] = None
        self._context = None
        
    async def connect(self):
        if not HAS_MCP_SDK:
            raise RuntimeError("MCP SDK not installed")
        self._context = streamable_http_client(self.url)
        read, write, _ = await self._context.__aenter__()
        self.session = ClientSession(read, write)
        await self.session.__aenter__()
        await self.session.initialize()
        
    async def close(self):
        if self.session:
            await self.session.__aexit__(None, None, None)
        if self._context:
            await self._context.__aexit__(None, None, None)
            
    async def list_tools(self):
        return await self.session.list_tools()
    
    async def call_tool(self, name: str, arguments: dict):
        return await self.session.call_tool(name, arguments)


# =============================================================================
# Test Classes
# =============================================================================

class BridgeProtocolTests:
    """Tests for Bridge protocol"""
    
    def __init__(self, suite: TestSuite, transport_name: str, verbose: bool = False):
        self.suite = suite
        self.transport_name = transport_name
        self.verbose = verbose
        
    def log(self, msg: str):
        if self.verbose:
            print(f"    [DEBUG] {msg}")
            
    async def run_tests(self, client: BridgeClient):
        """Run all Bridge protocol tests"""
        print(f"\n--- Bridge Protocol Tests ({self.transport_name}) ---")
        
        await self.test_simple_eval(client)
        await self.test_arithmetic(client)
        await self.test_string_operations(client)
        await self.test_list_operations(client)
        await self.test_defun(client)
        await self.test_error_handling(client)
        
    async def test_simple_eval(self, client: BridgeClient):
        try:
            result = await client.eval("(+ 1 2)")
            self.log(f"Result: {result}")
            
            passed = result.get("success", False) and "3" in str(result.get("result", ""))
            self.suite.add(TestResult(
                name="Simple eval (+ 1 2)",
                passed=passed,
                message=str(result)[:80],
                protocol="bridge",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="Simple eval",
                passed=False,
                message=str(e),
                protocol="bridge",
                transport=self.transport_name
            ))
            
    async def test_arithmetic(self, client: BridgeClient):
        test_cases = [
            ("(* 6 7)", "42"),
            ("(- 100 58)", "42"),
            ("(/ 84 2)", "42"),
        ]
        
        for code, expected in test_cases:
            try:
                result = await client.eval(code)
                passed = result.get("success", False) and expected in str(result.get("result", ""))
                self.suite.add(TestResult(
                    name=f"Arithmetic: {code}",
                    passed=passed,
                    message=f"Expected {expected}, got {result.get('result')}",
                    protocol="bridge",
                    transport=self.transport_name
                ))
            except Exception as e:
                self.suite.add(TestResult(
                    name=f"Arithmetic: {code}",
                    passed=False,
                    message=str(e),
                    protocol="bridge",
                    transport=self.transport_name
                ))
                
    async def test_string_operations(self, client: BridgeClient):
        try:
            result = await client.eval('(concatenate \'string "hello" " " "world")')
            passed = result.get("success", False) and "hello world" in str(result.get("result", "")).lower()
            self.suite.add(TestResult(
                name="String concatenation",
                passed=passed,
                message=str(result.get("result", ""))[:80],
                protocol="bridge",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="String concatenation",
                passed=False,
                message=str(e),
                protocol="bridge",
                transport=self.transport_name
            ))
            
    async def test_list_operations(self, client: BridgeClient):
        try:
            result = await client.eval("(length '(1 2 3 4 5))")
            passed = result.get("success", False) and "5" in str(result.get("result", ""))
            self.suite.add(TestResult(
                name="List length",
                passed=passed,
                message=str(result.get("result", "")),
                protocol="bridge",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="List length",
                passed=False,
                message=str(e),
                protocol="bridge",
                transport=self.transport_name
            ))
            
    async def test_defun(self, client: BridgeClient):
        try:
            # Define function
            await client.eval("(defun bridge-test-fn (x) (* x x))")
            # Call it
            result = await client.eval("(bridge-test-fn 7)")
            passed = result.get("success", False) and "49" in str(result.get("result", ""))
            self.suite.add(TestResult(
                name="Define and call function",
                passed=passed,
                message=str(result.get("result", "")),
                protocol="bridge",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="Define and call function",
                passed=False,
                message=str(e),
                protocol="bridge",
                transport=self.transport_name
            ))
            
    async def test_error_handling(self, client: BridgeClient):
        try:
            result = await client.eval("(/ 1 0)")
            # Should get an error response
            is_error = not result.get("success", True) or "error" in str(result).lower()
            self.suite.add(TestResult(
                name="Error handling (division by zero)",
                passed=is_error,
                message=str(result)[:80],
                protocol="bridge",
                transport=self.transport_name
            ))
        except Exception as e:
            # Exception is also acceptable error handling
            self.suite.add(TestResult(
                name="Error handling (division by zero)",
                passed=True,
                message=f"Exception: {e}",
                protocol="bridge",
                transport=self.transport_name
            ))


class MCPProtocolTests:
    """Tests for MCP protocol"""
    
    def __init__(self, suite: TestSuite, transport_name: str, verbose: bool = False):
        self.suite = suite
        self.transport_name = transport_name
        self.verbose = verbose
        
    def log(self, msg: str):
        if self.verbose:
            print(f"    [DEBUG] {msg}")
            
    async def run_tests_socket(self, client: MCPSocketClient):
        """Run MCP tests over socket transport"""
        print(f"\n--- MCP Protocol Tests ({self.transport_name}) ---")
        
        await self.test_initialize_socket(client)
        await self.test_list_tools_socket(client)
        await self.test_eval_cl_socket(client)
        
    async def run_tests_http(self, client: MCPHTTPClient):
        """Run MCP tests over HTTP transport"""
        print(f"\n--- MCP Protocol Tests ({self.transport_name}) ---")
        
        await self.test_list_tools_http(client)
        await self.test_eval_cl_http(client)
        await self.test_define_function_http(client)
        
    async def test_initialize_socket(self, client: MCPSocketClient):
        try:
            result = await client.initialize()
            self.log(f"Initialize result: {result}")
            
            has_result = "result" in result
            self.suite.add(TestResult(
                name="Initialize session",
                passed=has_result and "error" not in result,
                message=str(result)[:80],
                protocol="mcp",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="Initialize session",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport=self.transport_name
            ))
            
    async def test_list_tools_socket(self, client: MCPSocketClient):
        try:
            result = await client.list_tools()
            self.log(f"List tools result: {result}")
            
            tools = result.get("result", {}).get("tools", [])
            tool_names = [t.get("name") for t in tools]
            
            self.suite.add(TestResult(
                name="List tools",
                passed=len(tools) > 0,
                message=f"Found {len(tools)} tools: {tool_names[:5]}...",
                protocol="mcp",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="List tools",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport=self.transport_name
            ))
            
    async def test_eval_cl_socket(self, client: MCPSocketClient):
        try:
            result = await client.call_tool("eval_cl", {"code": "(+ 1 2)"})
            self.log(f"eval_cl result: {result}")
            
            content = result.get("result", {}).get("content", [])
            text = content[0].get("text", "") if content else ""
            
            self.suite.add(TestResult(
                name="eval_cl: (+ 1 2)",
                passed="3" in text,
                message=f"Result: {text}",
                protocol="mcp",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="eval_cl: (+ 1 2)",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport=self.transport_name
            ))
            
    async def test_list_tools_http(self, client: MCPHTTPClient):
        try:
            result = await client.list_tools()
            tools = result.tools
            tool_names = [t.name for t in tools]
            
            self.suite.add(TestResult(
                name="List tools",
                passed=len(tools) > 0,
                message=f"Found {len(tools)} tools: {tool_names[:5]}",
                protocol="mcp",
                transport=self.transport_name
            ))
            
            # Check for expected tools
            expected = ["eval_cl", "define_function", "get_package"]
            found = [t for t in expected if t in tool_names]
            self.suite.add(TestResult(
                name="Core tools available",
                passed=len(found) == len(expected),
                message=f"Found {found}",
                protocol="mcp",
                transport=self.transport_name
            ))
            
        except Exception as e:
            self.suite.add(TestResult(
                name="List tools",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport=self.transport_name
            ))
            
    async def test_eval_cl_http(self, client: MCPHTTPClient):
        test_cases = [
            ("(+ 1 2)", "3"),
            ("(* 6 7)", "42"),
            ("(list 1 2 3)", "(1 2 3)"),
        ]
        
        for code, expected in test_cases:
            try:
                result = await client.call_tool("eval_cl", {"code": code})
                text = ""
                if result.content:
                    for c in result.content:
                        if hasattr(c, 'text'):
                            text = c.text
                            break
                
                passed = expected.upper() in text.upper()
                self.suite.add(TestResult(
                    name=f"eval_cl: {code}",
                    passed=passed,
                    message=f"Got '{text}', expected '{expected}'",
                    protocol="mcp",
                    transport=self.transport_name
                ))
            except Exception as e:
                self.suite.add(TestResult(
                    name=f"eval_cl: {code}",
                    passed=False,
                    message=str(e),
                    protocol="mcp",
                    transport=self.transport_name
                ))
                
    async def test_define_function_http(self, client: MCPHTTPClient):
        try:
            # Define function
            result = await client.call_tool("define_function", {
                "name": "mcp-test-square",
                "lambda_list": "(x)",
                "body": "(* x x)"
            })
            
            # Call it
            result = await client.call_tool("eval_cl", {"code": "(mcp-test-square 9)"})
            text = ""
            if result.content:
                for c in result.content:
                    if hasattr(c, 'text'):
                        text = c.text
                        break
            
            self.suite.add(TestResult(
                name="define_function and call",
                passed="81" in text,
                message=f"(mcp-test-square 9) = {text}",
                protocol="mcp",
                transport=self.transport_name
            ))
        except Exception as e:
            self.suite.add(TestResult(
                name="define_function and call",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport=self.transport_name
            ))


# =============================================================================
# Main Test Runner
# =============================================================================

async def run_bridge_tests(suite: TestSuite, args):
    """Run Bridge protocol tests"""
    
    if args.transport in ("tcp", "all"):
        print(f"\nConnecting to Bridge TCP: {args.host}:{args.bridge_port}")
        try:
            transport = TCPTransport(args.host, args.bridge_port)
            client = BridgeClient(transport)
            await client.connect()
            
            tests = BridgeProtocolTests(suite, "tcp", args.verbose)
            await tests.run_tests(client)
            
            await client.close()
        except Exception as e:
            suite.add(TestResult(
                name="TCP Connection",
                passed=False,
                message=str(e),
                protocol="bridge",
                transport="tcp"
            ))
    
    if args.transport in ("unix", "all") and args.bridge_socket:
        print(f"\nConnecting to Bridge Unix: {args.bridge_socket}")
        try:
            transport = UnixTransport(args.bridge_socket)
            client = BridgeClient(transport)
            await client.connect()
            
            tests = BridgeProtocolTests(suite, "unix", args.verbose)
            await tests.run_tests(client)
            
            await client.close()
        except Exception as e:
            suite.add(TestResult(
                name="Unix Connection",
                passed=False,
                message=str(e),
                protocol="bridge",
                transport="unix"
            ))


async def run_mcp_tests(suite: TestSuite, args):
    """Run MCP protocol tests"""
    
    if args.transport in ("http", "all"):
        url = f"http://{args.host}:{args.mcp_port}/mcp"
        print(f"\nConnecting to MCP HTTP: {url}")
        
        if HAS_MCP_SDK:
            try:
                client = MCPHTTPClient(url)
                await client.connect()
                
                tests = MCPProtocolTests(suite, "http", args.verbose)
                await tests.run_tests_http(client)
                
                await client.close()
            except Exception as e:
                suite.add(TestResult(
                    name="HTTP Connection",
                    passed=False,
                    message=str(e),
                    protocol="mcp",
                    transport="http"
                ))
        else:
            suite.add(TestResult(
                name="HTTP Transport",
                passed=False,
                message="MCP SDK not installed (pip install mcp)",
                protocol="mcp",
                transport="http"
            ))
    
    if args.transport in ("tcp", "all"):
        print(f"\nConnecting to MCP TCP: {args.host}:{args.mcp_port}")
        try:
            transport = TCPTransport(args.host, args.mcp_port)
            client = MCPSocketClient(transport)
            await client.connect()
            
            tests = MCPProtocolTests(suite, "tcp", args.verbose)
            await tests.run_tests_socket(client)
            
            await client.close()
        except Exception as e:
            suite.add(TestResult(
                name="TCP Connection",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport="tcp"
            ))
    
    if args.transport in ("unix", "all") and args.mcp_socket:
        print(f"\nConnecting to MCP Unix: {args.mcp_socket}")
        try:
            transport = UnixTransport(args.mcp_socket)
            client = MCPSocketClient(transport)
            await client.connect()
            
            tests = MCPProtocolTests(suite, "unix", args.verbose)
            await tests.run_tests_socket(client)
            
            await client.close()
        except Exception as e:
            suite.add(TestResult(
                name="Unix Connection",
                passed=False,
                message=str(e),
                protocol="mcp",
                transport="unix"
            ))


async def main():
    parser = argparse.ArgumentParser(
        description="Unified Protocol Tests for ACL2-MCP-Bridge",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Test all protocols and transports
  python protocol-tests.py
  
  # Test only MCP over HTTP
  python protocol-tests.py --protocol mcp --transport http
  
  # Test Bridge over Unix socket
  python protocol-tests.py --protocol bridge --transport unix --bridge-socket /tmp/bridge.sock
  
  # Test with custom ports
  python protocol-tests.py --mcp-port 8080 --bridge-port 5000
"""
    )
    
    parser.add_argument("--protocol", choices=["mcp", "bridge", "all"], default="all",
                        help="Protocol to test (default: all)")
    parser.add_argument("--transport", choices=["http", "tcp", "unix", "all"], default="all",
                        help="Transport to test (default: all)")
    parser.add_argument("--host", default="localhost",
                        help="TCP host (default: localhost)")
    parser.add_argument("--mcp-port", type=int, default=8080,
                        help="MCP server port (default: 8080)")
    parser.add_argument("--bridge-port", type=int, default=5000,
                        help="Bridge server port (default: 5000)")
    parser.add_argument("--mcp-socket", default="/tmp/acl2-mcp.sock",
                        help="MCP Unix socket path")
    parser.add_argument("--bridge-socket", default="/tmp/acl2-bridge.sock",
                        help="Bridge Unix socket path")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Verbose output")
    
    args = parser.parse_args()
    
    print("=" * 70)
    print("ACL2-MCP-Bridge Protocol Test Suite")
    print("=" * 70)
    print(f"Protocol:  {args.protocol}")
    print(f"Transport: {args.transport}")
    print(f"MCP SDK:   {'installed' if HAS_MCP_SDK else 'NOT installed'}")
    
    suite = TestSuite()
    
    if args.protocol in ("mcp", "all"):
        await run_mcp_tests(suite, args)
        
    if args.protocol in ("bridge", "all"):
        await run_bridge_tests(suite, args)
    
    suite.print_summary()
    
    return suite.failed == 0


if __name__ == "__main__":
    success = asyncio.run(main())
    sys.exit(0 if success else 1)
