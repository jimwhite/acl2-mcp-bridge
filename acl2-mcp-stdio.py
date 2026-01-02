#!/usr/bin/env python3
"""
ACL2 MCP Bridge - stdio wrapper

Transparent bridge: stdin/stdout <-> Unix socket.
Both sides use the same protocol (Content-Length framed JSON-RPC).
"""

import os
import select
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path


def log(msg: str):
    print(f"[acl2-mcp-stdio] {msg}", file=sys.stderr, flush=True)


def start_acl2_server(sock_path: str, project_path: str) -> subprocess.Popen:
    """Start ACL2 with MCP server on Unix socket."""
    
    startup_file = tempfile.NamedTemporaryFile(mode='w', suffix='.lisp', delete=False)
    startup_file.write(f''':q
(sb-ext:disable-debugger)
(load "~/quicklisp/setup.lisp")
(push #P"{project_path}/" asdf:*central-registry*)
(push #P"{project_path}/vendor/40ants-mcp/" asdf:*central-registry*)
(asdf:load-system :acl2-mcp-bridge)
(acl2-mcp-bridge:start-server :protocol :mcp :transport :unix-socket 
                              :socket-path (uiop:getenv "ACL2_MCP_SOCKET_PATH"))
(loop (sleep 3600))
''')
    startup_file.close()
    
    env = os.environ.copy()
    env["ACL2_MCP_SOCKET_PATH"] = sock_path
    
    return subprocess.Popen(
        ["acl2"],
        stdin=open(startup_file.name, 'r'),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env
    )


def main():
    import socket
    
    project_path = str(Path(__file__).parent.absolute())
    sock_path = f"/tmp/acl2-mcp-{os.getpid()}.sock"
    
    if os.path.exists(sock_path):
        os.unlink(sock_path)
    
    log(f"Starting ACL2 MCP server on {sock_path}")
    proc = start_acl2_server(sock_path, project_path)
    conn = None
    
    def cleanup(signum=None, frame=None):
        if conn:
            conn.close()
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except:
            proc.kill()
        if os.path.exists(sock_path):
            os.unlink(sock_path)
        sys.exit(0)
    
    signal.signal(signal.SIGTERM, cleanup)
    signal.signal(signal.SIGINT, cleanup)
    
    try:
        # Wait for socket, watching for subprocess death
        log("Waiting for server...")
        while True:
            if proc.poll() is not None:
                log(f"ACL2 died with code {proc.returncode}")
                stdout, stderr = proc.communicate()
                log(f"stderr: {stderr.decode()[:2000] if stderr else ''}")
                sys.exit(1)
            
            if os.path.exists(sock_path):
                try:
                    conn = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                    conn.connect(sock_path)
                    log("Connected to socket")
                    break
                except (ConnectionRefusedError, FileNotFoundError, OSError) as e:
                    # Socket exists but not listening yet - close and retry
                    conn.close()
                    conn = None
            
            # Stream stderr while waiting, with a delay
            readable, _, _ = select.select([proc.stderr], [], [], 0.05)
            if readable:
                line = proc.stderr.readline()
                if line:
                    sys.stderr.buffer.write(line)
                    sys.stderr.buffer.flush()
        
        log("Connected, bridging stdio <-> socket")
        conn.setblocking(False)
        stdin_fd = sys.stdin.buffer.fileno()
        
        # Transparent bridge: stdin -> socket, socket -> stdout
        while True:
            readable, _, _ = select.select([stdin_fd, conn], [], [], 1.0)
            
            for fd in readable:
                if fd == stdin_fd:
                    data = sys.stdin.buffer.read1(4096)
                    if not data:
                        log("EOF on stdin")
                        return
                    conn.sendall(data)
                elif fd == conn:
                    try:
                        data = conn.recv(4096)
                        if not data:
                            log("Socket closed")
                            return
                        sys.stdout.buffer.write(data)
                        sys.stdout.buffer.flush()
                    except BlockingIOError:
                        pass
            
            # Check if subprocess died
            if proc.poll() is not None:
                log(f"ACL2 process exited with code {proc.returncode}")
                return
    
    finally:
        cleanup()


if __name__ == "__main__":
    main()
