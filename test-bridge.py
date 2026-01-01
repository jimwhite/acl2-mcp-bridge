#!/usr/bin/env python3
"""Test the ACL2 Bridge server."""

from acl2_bridge import ACL2Bridge

print("Connecting to ACL2 Bridge on port 55433...")
# ACL2Bridge connects automatically in __init__
bridge = ACL2Bridge(port=55433)
print(f"Connected! Worker: {bridge._worker}")

# Test a simple command
print("\nTesting (+ 1 2)...")
result = bridge.acl2_command(cmd="(+ 1 2)")
print(f"Result: {result}")

# Test another command
print("\nTesting (list 'a 'b 'c)...")
result = bridge.acl2_command(cmd="(list 'a 'b 'c)")
print(f"Result: {result}")

print("\nDone.")
