#!/bin/bash
# Compare test results across all BLAS vendors
# Analyzes vendor_test_results.txt and test logs

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

echo "MUMPS Vendor Comparison Report"
echo "==============================="
echo ""

# Check if results file exists
if [ ! -f vendor_test_results.txt ]; then
    echo "Error: vendor_test_results.txt not found"
    echo "Run ./scripts/test_all_vendors.sh first"
    exit 1
fi

# Display results summary
echo "Test Results Summary:"
echo "---------------------"
cat vendor_test_results.txt
echo ""

# Compare performance if logs exist
echo "Performance Comparison:"
echo "----------------------"

for vendor in openblas blis reference mkl atlas; do
    if [ -f "test_${vendor}.log" ]; then
        # Extract timing information
        duration=$(grep -E "in [0-9]+\.[0-9]+s" "test_${vendor}.log" | tail -1 | grep -oE "[0-9]+\.[0-9]+s")
        passed=$(grep -E "passed" "test_${vendor}.log" | tail -1 | grep -oE "[0-9]+ passed" || echo "0 passed")

        printf "%-12s: %s (%s)\n" "$vendor" "$duration" "$passed"
    fi
done

echo ""

# Analyze SQLite database for vendor comparison
if [ -f mumps.sqlite ] && command -v uv &> /dev/null; then
    echo "Database Analysis:"
    echo "------------------"
    uv run python scripts/analyze_test_results.py --vendors 2>/dev/null || echo "Vendor analysis not available"
fi

# Check for failures
echo ""
echo "Failures/Errors:"
echo "----------------"

has_failures=false
for vendor in openblas blis reference mkl atlas; do
    if [ -f "test_${vendor}.log" ]; then
        failures=$(grep -E "FAILED|ERROR" "test_${vendor}.log" | grep -v "^=" | head -5)
        if [ -n "$failures" ]; then
            echo "[$vendor]"
            echo "$failures"
            echo ""
            has_failures=true
        fi
    fi
done

if [ "$has_failures" = false ]; then
    echo "No failures detected across all vendors ✓"
fi

echo ""
echo "Individual test logs: test_<vendor>.log"
echo "JUnit XML reports: junit_<vendor>.xml"
