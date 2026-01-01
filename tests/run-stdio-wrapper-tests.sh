#!/bin/bash
# stdio Wrapper Test Driver
# Tests the acl2-mcp-stdio.py wrapper that bridges stdio <-> Unix socket

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

VERBOSE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE="-v"
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [-v|--verbose]"
            echo ""
            echo "Tests the acl2-mcp-stdio.py wrapper that enables MCP stdio transport."
            echo ""
            echo "Options:"
            echo "  -v, --verbose    Enable verbose output"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=============================================="
echo "stdio Wrapper Test Suite"
echo "=============================================="
echo "Wrapper: $PROJECT_DIR/acl2-mcp-stdio.py"
echo "Project: $PROJECT_DIR"
echo ""

# Run the Python test
cd "$SCRIPT_DIR"
"$PROJECT_DIR/.venv/bin/python" test-stdio-wrapper.py $VERBOSE
exit $?
