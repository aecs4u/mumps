#!/bin/bash
# Quick vendor test - runs tests on current build without rebuilding
# Useful for testing a single vendor quickly

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

VENDOR=${1:-current}

echo "Running quick test for vendor: $VENDOR"
echo "======================================"

# Set library paths
export LD_LIBRARY_PATH="${PROJECT_ROOT}/lib:${PROJECT_ROOT}/src/PORD/lib:${LD_LIBRARY_PATH}"

# Run tests
"$SCRIPT_DIR/run_tests.sh" tests/ -v --junit-xml="junit_${VENDOR}.xml" 2>&1 | tee "test_${VENDOR}.log"

# Show summary
echo ""
echo "Results for $VENDOR:"
grep -E "passed|failed|error" "test_${VENDOR}.log" | tail -1

# Optionally analyze results
if [ -f mumps.sqlite ]; then
    echo ""
    echo "Latest test session:"
    uv run python scripts/analyze_test_results.py --latest 2>/dev/null || true
fi
