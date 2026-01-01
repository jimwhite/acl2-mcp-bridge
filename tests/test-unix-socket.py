#!/usr/bin/env python3
"""
Test Unix socket MCP server directly.

This tests the Unix socket HTTP server without the stdio wrapper,
to isolate any issues with the socket layer.
"""

import json
import os
import socket
import subprocess
import sys
import time

SOCK_PATH = "/tmp/test-acl2-mcp.sock"
PROJECT_PATH = "/workspaces/acl2-mcp-bridge"


def log(msg: str):
    print(f"[test] {msg}", file=sys.stderr, flush=True)


def start_acl2_server() -> subprocess.Popen:
    """Start ACL2 with MCP server on Unix socket."""
    # Remove old socket
    if os.path.exists(SOCK_PATH):
        os.unlink(SOCK_PATH)
    
    startup_code = f'''
:q
(sb-ext:disable-debugger)
(load "~/quicklisp/setup.lisp")
(push #P"{PROJECT_PATH}/" asdf:*central-registry*)
(ql:quickload :acl2-mcp-bridge :silent t)
(acl2-mcp-bridge:start-server :protocol :mcp :transport :http :socket-path "{SOCK_PATH}")
'''
    
    log("Starting ACL2...")
    proc = subprocess.Popen(
        ["acl2"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    proc.stdin.write(startup_code)
    proc.stdin.flush()
    return proc


def wait_for_socket(timeout: float = 60.0) -> bool:
    """Wait for socket file to exist."""
    start = time.time()
    while time.time() - start < timeout:
        if os.path.exists(SOCK_PATH):
            log(f"Socket exists after {time.time() - start:.1f}s")
            return True
        time.sleep(0.5)
    return False


def send_http_request(body: bytes) -> bytes:
    """Send HTTP POST to Unix socket, return response body."""
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.settimeout(30)
    sock.connect(SOCK_PATH)
    
    request = (
        f"POST /mcp HTTP/1.1\r\n"
        f"Host: localhost\r\n"
        f"Content-Type: application/json\r\n"
        f"Accept: application/json\r\n"
        f"Content-Length: {len(body)}\r\n"
        f"\r\n"
    ).encode() + body
    
    log(f"Sending {len(request)} bytes...")
    sock.sendall(request)
    
    # Read response
    response = b""
    try:
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                break
            response += chunk
            log(f"Received {len(chunk)} bytes, total {len(response)}")
            
            # Check if we have complete response
            if b"\r\n\r\n" in response:
                header_end = response.index(b"\r\n\r\n") + 4
                headers = response[:header_end].decode()
                for line in headers.split("\r\n"):
                    if line.lower().startswith("content-length:"):
                        content_length = int(line.split(":")[1].strip())
                        if len(response) >= header_end + content_length:
                            sock.close()
                            return response[header_end:header_end + content_length]
                        break
    except socket.timeout:
        log("Timeout waiting for response")
    
    sock.close()
    if b"\r\n\r\n" in response:
        return response.split(b"\r\n\r\n", 1)[1]
    return response


def test_initialize():
    """Test MCP initialize request."""
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2025-11-25",
            "capabilities": {},
            "clientInfo": {"name": "test", "version": "1.0"}
        }
    }).encode()
    
    response = send_http_request(body)
    log(f"Response: {response.decode()}")
    
    data = json.loads(response)
    assert "result" in data, f"Expected result, got: {data}"
    assert data["id"] == 1
    log("✓ Initialize OK")
    return data


def test_tools_list():
    """Test tools/list request."""
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list"
    }).encode()
    
    response = send_http_request(body)
    log(f"Response: {response.decode()}")
    
    data = json.loads(response)
    assert "result" in data, f"Expected result, got: {data}"
    assert "tools" in data["result"]
    log(f"✓ Tools list OK: {len(data['result']['tools'])} tools")
    return data


def test_eval_cl():
    """Test eval_cl tool."""
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {
            "name": "eval_cl",
            "arguments": {"code": "(+ 1 2 3)"}
        }
    }).encode()
    
    response = send_http_request(body)
    log(f"Response: {response.decode()}")
    
    data = json.loads(response)
    assert "result" in data, f"Expected result, got: {data}"
    content = data["result"]["content"][0]["text"]
    assert "6" in content, f"Expected 6, got: {content}"
    log("✓ eval_cl OK")
    return data


def main():
    acl2_proc = None
    try:
        acl2_proc = start_acl2_server()
        
        if not wait_for_socket():
            log("ERROR: Socket not created")
            stdout, stderr = acl2_proc.communicate(timeout=5)
            log(f"ACL2 stdout:\n{stdout[:2000]}")
            log(f"ACL2 stderr:\n{stderr[:2000]}")
            return 1
        
        # Wait for server to be ready
        time.sleep(1)
        
        log("Running tests...")
        test_initialize()
        test_tools_list()
        test_eval_cl()
        
        log("\n=== All tests passed! ===")
        return 0
        
    except Exception as e:
        log(f"ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1
        
    finally:
        if acl2_proc:
            acl2_proc.terminate()
            try:
                acl2_proc.wait(timeout=5)
            except:
                acl2_proc.kill()
        if os.path.exists(SOCK_PATH):
            os.unlink(SOCK_PATH)


if __name__ == "__main__":
    sys.exit(main())
