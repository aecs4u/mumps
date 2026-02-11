#!/bin/bash
# Wrapper script to run pytest with test dependencies and library paths
# Usage: ./scripts/run_tests.sh [pytest arguments]

# Get the project root (parent of scripts/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Set library paths for MUMPS shared libraries
export LD_LIBRARY_PATH="${PROJECT_ROOT}/lib:${PROJECT_ROOT}/src/PORD/lib:${LD_LIBRARY_PATH}"

# Install test dependencies if needed
uv pip install --quiet pytest pytest-cov pytest-benchmark pytest-env 2>/dev/null ||  true

# Run pytest preserving LD_LIBRARY_PATH
cd "$PROJECT_ROOT"
exec python3 -m pytest "$@"
