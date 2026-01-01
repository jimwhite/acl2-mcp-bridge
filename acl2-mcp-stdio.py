#!/usr/bin/env python3
"""
ACL2 MCP Bridge - stdio wrapper

This script provides MCP stdio transport by:
1. Launching ACL2 and starting the MCP HTTP server on a Unix socket
2. Reading JSON-RPC from stdin, forwarding as HTTP to the Unix socket
3. Writing HTTP responses back to stdout

Usage:
    python acl2-mcp-stdio.py [--socket PATH] [--acl2 PATH] [--project PATH]

VS Code configuration:
    {
      "mcp": {
        "servers": {
          "acl2-mcp-bridge": {
            "type": "stdio",
            "command": "python3",
            "args": ["/path/to/acl2-mcp-stdio.py"]
          }
        }
      }
    }
"""

import argparse
import json
import os
import signal
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path


def log(msg: str):
    """Log to stderr (doesn't interfere with stdio protocol)."""
    print(f"[acl2-mcp-stdio] {msg}", file=sys.stderr, flush=True)


def send_http_request(sock_path: str, method: str, body: bytes) -> bytes:
    """Send HTTP request over Unix socket, return response body."""
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect(sock_path)
    
    # Build HTTP request
    request = (
        f"{method} /mcp HTTP/1.1\r\n"
        f"Host: localhost\r\n"
        f"Content-Type: application/json\r\n"
        f"Accept: application/json, text/event-stream\r\n"
        f"Content-Length: {len(body)}\r\n"
        f"\r\n"
    ).encode() + body
    
    sock.sendall(request)
    
    # Read response
    response = b""
    while True:
        chunk = sock.recv(4096)
        if not chunk:
            break
        response += chunk
        # Check if we have complete response (Content-Length based)
        if b"\r\n\r\n" in response:
            header_end = response.index(b"\r\n\r\n") + 4
            headers = response[:header_end].decode()
            # Find Content-Length
            for line in headers.split("\r\n"):
                if line.lower().startswith("content-length:"):
                    content_length = int(line.split(":")[1].strip())
                    if len(response) >= header_end + content_length:
                        sock.close()
                        return response[header_end:header_end + content_length]
                    break
    
    sock.close()
    # Return body after headers
    if b"\r\n\r\n" in response:
        return response.split(b"\r\n\r\n", 1)[1]
    return response


def wait_for_socket(sock_path: str, timeout: float = 30.0) -> bool:
    """Wait for Unix socket to become available."""
    start = time.time()
    while time.time() - start < timeout:
        if os.path.exists(sock_path):
            try:
                sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                sock.connect(sock_path)
                sock.close()
                return True
            except (ConnectionRefusedError, FileNotFoundError):
                pass
        time.sleep(0.1)
    return False


def start_acl2_server(acl2_path: str, sock_path: str, project_path: str) -> subprocess.Popen:
    """Start ACL2 with MCP server on Unix socket."""
    
    # Lisp code to start MCP server on Unix socket
    startup_code = f'''
:q
(sb-ext:disable-debugger)
(load "~/quicklisp/setup.lisp")
(push #P"{project_path}/" asdf:*central-registry*)
(ql:quickload :acl2-mcp-bridge)
(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :socket-path "{sock_path}")
'''
    
    log(f"Starting ACL2 with MCP server on {sock_path}")
    
    proc = subprocess.Popen(
        [acl2_path],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    
    # Send startup commands
    proc.stdin.write(startup_code)
    proc.stdin.flush()
    
    return proc


def read_jsonrpc_message() -> bytes | None:
    """Read a JSON-RPC message from stdin (Content-Length framing)."""
    # Read headers
    headers = {}
    while True:
        line = sys.stdin.buffer.readline()
        if line == b"\r\n" or line == b"\n" or line == b"":
            break
        if b":" in line:
            key, value = line.decode().split(":", 1)
            headers[key.strip().lower()] = value.strip()
    
    if "content-length" not in headers:
        return None
    
    content_length = int(headers["content-length"])
    body = sys.stdin.buffer.read(content_length)
    return body


def write_jsonrpc_message(body: bytes):
    """Write a JSON-RPC message to stdout (Content-Length framing)."""
    header = f"Content-Length: {len(body)}\r\n\r\n"
    sys.stdout.buffer.write(header.encode() + body)
    sys.stdout.buffer.flush()


def main():
    parser = argparse.ArgumentParser(description="ACL2 MCP Bridge stdio wrapper")
    parser.add_argument("--socket", default=None,
                        help="Unix socket path (default: auto-generated temp path)")
    parser.add_argument("--acl2", default="acl2",
                        help="Path to ACL2 executable (default: acl2)")
    parser.add_argument("--project", default=None,
                        help="Path to acl2-mcp-bridge project")
    args = parser.parse_args()
    
    # Determine paths
    if args.project:
        project_path = args.project
    else:
        # Assume script is in the project directory
        project_path = str(Path(__file__).parent.absolute())
    
    if args.socket:
        sock_path = args.socket
    else:
        # Create temp socket path
        sock_path = os.path.join(tempfile.gettempdir(), f"acl2-mcp-{os.getpid()}.sock")
    
    # Clean up any existing socket
    if os.path.exists(sock_path):
        os.unlink(sock_path)
    
    acl2_proc = None
    
    def cleanup(signum=None, frame=None):
        """Clean up on exit."""
        log("Shutting down...")
        if acl2_proc:
            acl2_proc.terminate()
            acl2_proc.wait(timeout=5)
        if os.path.exists(sock_path):
            os.unlink(sock_path)
        sys.exit(0)
    
    signal.signal(signal.SIGTERM, cleanup)
    signal.signal(signal.SIGINT, cleanup)
    
    try:
        # Start ACL2 server
        acl2_proc = start_acl2_server(args.acl2, sock_path, project_path)
        
        # Wait for socket to be ready
        log(f"Waiting for MCP server on {sock_path}...")
        if not wait_for_socket(sock_path, timeout=60):
            log("ERROR: Timeout waiting for MCP server to start")
            # Print ACL2 output for debugging
            if acl2_proc.poll() is not None:
                stdout, stderr = acl2_proc.communicate()
                log(f"ACL2 stdout: {stdout[:1000]}")
                log(f"ACL2 stderr: {stderr[:1000]}")
            sys.exit(1)
        
        log("MCP server ready, bridging stdio <-> Unix socket")
        
        # Main loop: read from stdin, forward to socket, write response to stdout
        while True:
            message = read_jsonrpc_message()
            if message is None:
                log("EOF on stdin, exiting")
                break
            
            try:
                # Forward to MCP server via HTTP
                response = send_http_request(sock_path, "POST", message)
                
                # Write response to stdout
                write_jsonrpc_message(response)
                
            except Exception as e:
                log(f"Error forwarding message: {e}")
                # Send error response
                try:
                    req = json.loads(message)
                    req_id = req.get("id")
                    error_response = json.dumps({
                        "jsonrpc": "2.0",
                        "id": req_id,
                        "error": {
                            "code": -32603,
                            "message": f"Internal error: {e}"
                        }
                    }).encode()
                    write_jsonrpc_message(error_response)
                except:
                    pass
    
    finally:
        cleanup()


if __name__ == "__main__":
    main()
