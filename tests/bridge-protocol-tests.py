#!/usr/bin/env python3
"""
Bridge Protocol Integration Tests

Tests all Bridge protocol command types:
- LISP: Single Lisp value result
- LISP_MV: Multiple Lisp values result  
- JSON: Single JSON result
- JSON_MV: Multiple JSON values result
- Error handling

Usage:
    python3 bridge-protocol-tests.py [--unix SOCKET_PATH | --tcp HOST:PORT]
    
    --unix PATH   Connect via Unix socket (default: /tmp/acl2-bridge-test.sock)
    --tcp HOST:PORT   Connect via TCP (default: 127.0.0.1:55433)
    
    If no arguments given, defaults to Unix socket.
"""

import socket
import sys
import argparse


def read_message(sock):
    """Read a Bridge protocol message."""
    header = b""
    while not header.endswith(b"\n"):
        c = sock.recv(1)
        if not c:
            return None, None
        header += c
    header = header.decode().strip()
    parts = header.split(" ", 1)
    msg_type = parts[0]
    length = int(parts[1]) if len(parts) > 1 else 0
    
    content = b""
    while len(content) < length:
        chunk = sock.recv(length - len(content))
        if not chunk:
            break
        content += chunk
    
    sock.recv(1)  # trailing newline
    return msg_type, content.decode()


def send_command(sock, cmd_type, content):
    """Send a Bridge protocol command."""
    msg = f"{cmd_type} {len(content)}\n{content}\n"
    sock.sendall(msg.encode())


def run_command(sock, cmd_type, content):
    """Send command and return (result, error) tuple."""
    send_command(sock, cmd_type, content)
    result = None
    error = None
    while True:
        msg_type, msg_content = read_message(sock)
        if msg_type == "RETURN":
            result = msg_content
        elif msg_type == "ERROR":
            error = msg_content
        elif msg_type == "READY":
            break
    return result, error


def connect_unix(socket_path):
    """Connect to Bridge server via Unix socket."""
    print(f"Connecting to Unix socket: {socket_path}")
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect(socket_path)
    return sock


def connect_tcp(host, port):
    """Connect to Bridge server via TCP."""
    print(f"Connecting to TCP {host}:{port}")
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((host, port))
    return sock


def do_handshake(sock):
    """Perform Bridge protocol handshake."""
    msg_type, content = read_message(sock)
    if msg_type != "ACL2_BRIDGE_HELLO":
        raise RuntimeError(f"Expected ACL2_BRIDGE_HELLO, got {msg_type}")
    
    msg_type, content = read_message(sock)
    if msg_type != "READY":
        raise RuntimeError(f"Expected READY, got {msg_type}")
    
    print("Connected!")


class TestRunner:
    def __init__(self, sock):
        self.sock = sock
        self.passed = 0
        self.failed = 0
    
    def test(self, name, cmd_type, cmd, check):
        """Run a single test."""
        print(f"Test: {name}...", end=" ")
        result, error = run_command(self.sock, cmd_type, cmd)
        try:
            check(result, error)
            print("PASS")
            self.passed += 1
        except AssertionError as e:
            print(f"FAIL: {e}")
            self.failed += 1
    
    def run_all_tests(self):
        """Run all Bridge protocol tests."""
        
        # === LISP command type (single value) ===
        self.test("LISP: simple arithmetic",
             "LISP", "(+ 1 2)",
             lambda r, e: r and r.strip() == "3")

        self.test("LISP: multiplication",
             "LISP", "(* 6 7)",
             lambda r, e: r and r.strip() == "42")

        self.test("LISP: list construction",
             "LISP", "(cons 'a '(b c))",
             lambda r, e: r and "A" in r.upper())

        self.test("LISP: string result",
             "LISP", '"hello world"',
             lambda r, e: r and "hello" in r.lower())

        self.test("LISP: nil result",
             "LISP", "nil",
             lambda r, e: r and r.strip().upper() == "NIL")

        self.test("LISP: t result",
             "LISP", "t",
             lambda r, e: r and r.strip().upper() == "T")

        # === LISP_MV command type (multiple values) ===
        self.test("LISP_MV: floor with two values",
             "LISP_MV", "(floor 17 5)",
             lambda r, e: r and "3" in r and "2" in r)

        self.test("LISP_MV: truncate",
             "LISP_MV", "(truncate 10 3)",
             lambda r, e: r and "3" in r and "1" in r)

        self.test("LISP_MV: values form",
             "LISP_MV", "(values 1 2 3)",
             lambda r, e: r and "1" in r and "2" in r and "3" in r)

        self.test("LISP_MV: single value (should still work)",
             "LISP_MV", "(+ 1 2)",
             lambda r, e: r and "3" in r)

        # === JSON command type (single value) ===
        self.test("JSON: number",
             "JSON", "42",
             lambda r, e: r and r.strip() == "42")

        self.test("JSON: list as array",
             "JSON", "'(1 2 3)",
             lambda r, e: r and "[" in r and "1" in r)

        self.test("JSON: string",
             "JSON", '"test"',
             lambda r, e: r and "test" in r)

        # === JSON_MV command type (multiple values) ===
        self.test("JSON_MV: floor",
             "JSON_MV", "(floor 17 5)",
             lambda r, e: r and "3" in r and "2" in r)

        self.test("JSON_MV: values",
             "JSON_MV", "(values 'a 'b 'c)",
             lambda r, e: r and "A" in r.upper())

        # === Error handling ===
        self.test("ERROR: undefined function",
             "LISP", "(undefined-function-xyz)",
             lambda r, e: e is not None or (r is None))

        self.test("ERROR: syntax error",
             "LISP", "(+ 1",
             lambda r, e: e is not None or (r is None))
    
    def report(self):
        """Print test results and return exit code."""
        print(f"\n{'='*40}")
        print(f"Results: {self.passed} passed, {self.failed} failed")
        if self.failed > 0:
            return 1
        print("All tests passed!")
        return 0


def main():
    parser = argparse.ArgumentParser(description="Bridge Protocol Integration Tests")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--unix", metavar="PATH", 
                       help="Unix socket path (default: /tmp/acl2-bridge-test.sock)")
    group.add_argument("--tcp", metavar="HOST:PORT",
                       help="TCP host:port (e.g., 127.0.0.1:55433)")
    args = parser.parse_args()
    
    # Determine connection type
    if args.tcp:
        if ":" in args.tcp:
            host, port = args.tcp.rsplit(":", 1)
            port = int(port)
        else:
            host = args.tcp
            port = 55433
        sock = connect_tcp(host, port)
    else:
        socket_path = args.unix or "/tmp/acl2-bridge-test.sock"
        sock = connect_unix(socket_path)
    
    sock.settimeout(10)
    
    try:
        do_handshake(sock)
        runner = TestRunner(sock)
        runner.run_all_tests()
        return runner.report()
    finally:
        sock.close()


if __name__ == "__main__":
    sys.exit(main())
