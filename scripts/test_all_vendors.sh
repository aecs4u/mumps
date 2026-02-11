#!/bin/bash
# Script to run pytest tests for all BLAS vendor versions
# Auto-detects vendor libraries with C interface support
# Integrates with CMake build system for dependency configuration

set -e  # Exit on error

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# ──────────────────────────────────────────────────────────────
# CMake Integration: Extract BLAS config from CMake cache
# ──────────────────────────────────────────────────────────────

CMAKE_BLAS_LIBS=""
CMAKE_BLAS_VENDOR=""

extract_cmake_config() {
    local build_dir="$SCRIPT_DIR/build"
    local cache_file="$build_dir/CMakeCache.txt"

    if [ ! -f "$cache_file" ]; then
        return 1
    fi

    echo -e "${CYAN}Found CMake build cache at $cache_file${NC}"

    # Extract BLAS configuration
    CMAKE_BLAS_LIBS=$(grep "^BLAS_LIBRARIES:" "$cache_file" 2>/dev/null | cut -d= -f2-)
    if [ -z "$CMAKE_BLAS_LIBS" ]; then
        CMAKE_BLAS_LIBS=$(grep "^BLAS_LIBRARIES=" "$cache_file" 2>/dev/null | cut -d= -f2-)
    fi

    CMAKE_BLAS_VENDOR=$(grep "^BLA_VENDOR:" "$cache_file" 2>/dev/null | cut -d= -f2-)
    if [ -z "$CMAKE_BLAS_VENDOR" ]; then
        CMAKE_BLAS_VENDOR=$(grep "^BLAS_VENDOR:" "$cache_file" 2>/dev/null | cut -d= -f2-)
    fi

    if [ -n "$CMAKE_BLAS_LIBS" ]; then
        echo -e "${CYAN}  CMake BLAS vendor: ${CMAKE_BLAS_VENDOR:-auto}${NC}"
        echo -e "${CYAN}  CMake BLAS libs: $CMAKE_BLAS_LIBS${NC}"
        return 0
    fi

    return 1
}

# Try to extract CMake config
if extract_cmake_config; then
    echo -e "${GREEN}CMake integration active${NC}"
    echo ""
fi

# ──────────────────────────────────────────────────────────────
# Add BLAS vendor runtime library paths to LD_LIBRARY_PATH
# Needed so that ldd can resolve MKL/OpenBLAS/BLIS at runtime
# ──────────────────────────────────────────────────────────────

add_vendor_lib_path() {
    local vendor="$1"
    local lib_path=""

    case "$vendor" in
        default|mkl)
            # MKL: try pkg-config first, then common paths
            if command -v pkg-config >/dev/null 2>&1; then
                for pc in mkl-dynamic-lp64-seq mkl-dynamic-ilp64-seq mkl-sdl; do
                    if pkg-config --exists "$pc" 2>/dev/null; then
                        lib_path="$(pkg-config --variable=libdir "$pc" 2>/dev/null)"
                        break
                    fi
                done
            fi
            if [ -z "$lib_path" ]; then
                for mkl_root in "${MKLROOT:-}" /opt/intel/oneapi/mkl/latest /opt/intel/mkl; do
                    [ -z "$mkl_root" ] && continue
                    if [ -f "$mkl_root/lib/libmkl_rt.so" ]; then
                        lib_path="$mkl_root/lib"
                        break
                    elif [ -f "$mkl_root/lib/intel64/libmkl_rt.so" ]; then
                        lib_path="$mkl_root/lib/intel64"
                        break
                    fi
                done
            fi
            ;;
        blis|amd-vitis)
            if command -v pkg-config >/dev/null 2>&1; then
                for pc in blis-mt amdblis aocl-blis blis; do
                    if pkg-config --exists "$pc" 2>/dev/null; then
                        lib_path="$(pkg-config --variable=libdir "$pc" 2>/dev/null)"
                        break
                    fi
                done
            fi
            ;;
        openblas)
            if command -v pkg-config >/dev/null 2>&1 && pkg-config --exists openblas 2>/dev/null; then
                lib_path="$(pkg-config --variable=libdir openblas 2>/dev/null)"
            fi
            ;;
    esac

    if [ -n "$lib_path" ] && [ -d "$lib_path" ]; then
        lib_path="$(cd "$lib_path" && pwd)"
        export LD_LIBRARY_PATH="$lib_path${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    fi
}

# Function to check if a vendor library has C interface
has_c_interface() {
    local vendor_dir=$1
    local lib_path="$vendor_dir/libdmumps.so"

    if [ ! -f "$lib_path" ]; then
        return 1
    fi

    if nm "$lib_path" 2>/dev/null | grep -q "mumps_initialize"; then
        return 0
    else
        return 1
    fi
}

# Detect available vendors with C interface
VENDORS=()
VENDORS_WITHOUT_C_INTERFACE=()

# Check default build first
if [ -f "lib/libdmumps.so" ]; then
    if has_c_interface "lib"; then
        VENDORS+=("default")
    fi
fi

# If CMake build exists with libraries, check cmake build directory
if [ -n "$CMAKE_BLAS_LIBS" ] && [ -d "build/lib" ]; then
    if has_c_interface "build/lib"; then
        VENDORS+=("cmake")
    fi
fi

# Check vendor-specific builds
if [ -d "lib/vendors" ]; then
    for vendor_dir in lib/vendors/*; do
        if [ -d "$vendor_dir" ]; then
            vendor=$(basename "$vendor_dir")
            if has_c_interface "$vendor_dir"; then
                VENDORS+=("$vendor")
            else
                VENDORS_WITHOUT_C_INTERFACE+=("$vendor")
            fi
        fi
    done
fi

# Show status
echo -e "${BLUE}=== MUMPS Multi-Vendor Test Suite ===${NC}"
echo ""

if [ ${#VENDORS[@]} -eq 0 ]; then
    echo -e "${RED}✗ No MUMPS libraries with C interface found${NC}"
    echo ""
    echo "To build libraries with C interface:"
    echo "  1. Default build:  make allshared"
    echo "  2. Vendor builds:  ./scripts/rebuild_vendor_libraries.sh"
    if [ -n "$CMAKE_BLAS_LIBS" ]; then
        echo "  3. CMake build:    cd build && cmake --build . --target allshared"
    fi
    echo ""
    exit 1
fi

echo -e "${GREEN}Found ${#VENDORS[@]} vendor(s) with C interface:${NC} ${VENDORS[*]}"

if [ ${#VENDORS_WITHOUT_C_INTERFACE[@]} -gt 0 ]; then
    echo -e "${YELLOW}⚠  Found ${#VENDORS_WITHOUT_C_INTERFACE[@]} vendor(s) without C interface:${NC} ${VENDORS_WITHOUT_C_INTERFACE[*]}"
    echo -e "${YELLOW}⚠  To rebuild with C interface: ./scripts/rebuild_vendor_libraries.sh${NC}"
fi

echo ""

# Test results summary
RESULTS_FILE="vendor_test_results.txt"
echo "MUMPS BLAS Vendor Test Suite" > "$RESULTS_FILE"
echo "=============================" >> "$RESULTS_FILE"
echo "Started: $(date)" >> "$RESULTS_FILE"
echo "Vendors with C interface: ${VENDORS[*]}" >> "$RESULTS_FILE"
if [ ${#VENDORS_WITHOUT_C_INTERFACE[@]} -gt 0 ]; then
    echo "Vendors without C interface: ${VENDORS_WITHOUT_C_INTERFACE[*]}" >> "$RESULTS_FILE"
fi
if [ -n "$CMAKE_BLAS_LIBS" ]; then
    echo "CMake BLAS config: $CMAKE_BLAS_LIBS" >> "$RESULTS_FILE"
fi
echo "" >> "$RESULTS_FILE"

# Function to test a specific vendor
test_vendor() {
    local vendor=$1

    echo -e "${YELLOW}Testing $vendor...${NC}"

    # Set library path
    if [ "$vendor" = "default" ]; then
        local lib_path="${SCRIPT_DIR}/lib"
    elif [ "$vendor" = "cmake" ]; then
        local lib_path="${SCRIPT_DIR}/build/lib"
    else
        local lib_path="${SCRIPT_DIR}/lib/vendors/${vendor}"
    fi

    # Save and reset LD_LIBRARY_PATH for each vendor to avoid cross-contamination
    local base_ld_path="${SCRIPT_DIR}/src/PORD/lib"
    export LD_LIBRARY_PATH="${lib_path}:${base_ld_path}"

    # Add BLAS vendor runtime library paths (MKL, OpenBLAS, BLIS)
    add_vendor_lib_path "$vendor"

    # Set BLAS vendor environment for pytest to record
    export MUMPS_BLAS_VENDOR="$vendor"

    # Run pytest with vendor-specific output
    if python3 -m pytest tests/ --tb=short -v --junit-xml="junit_${vendor}.xml" 2>&1 | tee "test_${vendor}.log"; then
        # Extract test summary
        local summary=$(grep -E "passed|failed|error" "test_${vendor}.log" | tail -1)
        echo -e "${GREEN}✓ Tests completed for $vendor${NC}"
        echo "  $summary"
        echo "$vendor: $summary" >> "$RESULTS_FILE"
        return 0
    else
        echo -e "${RED}✗ Tests failed for $vendor${NC}"
        local summary=$(grep -E "passed|failed|error" "test_${vendor}.log" | tail -1)
        echo "  $summary"
        echo "$vendor: $summary (FAILED)" >> "$RESULTS_FILE"
        return 1
    fi
}

# Main test loop
TOTAL_VENDORS=${#VENDORS[@]}
SUCCESSFUL_TESTS=0
FAILED_TESTS=()

for vendor in "${VENDORS[@]}"; do
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Testing BLAS vendor: $vendor${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""

    if test_vendor "$vendor"; then
        SUCCESSFUL_TESTS=$((SUCCESSFUL_TESTS + 1))
    else
        FAILED_TESTS+=("$vendor")
    fi

    echo ""
    sleep 0.5  # Brief pause between vendors
done

# Final summary
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}FINAL SUMMARY${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Total vendors tested: $TOTAL_VENDORS"
echo -e "${GREEN}Successful tests: $SUCCESSFUL_TESTS/$TOTAL_VENDORS${NC}"

if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo -e "${RED}Failed tests: ${FAILED_TESTS[*]}${NC}"
fi

echo ""
echo "Detailed results saved to: $RESULTS_FILE"
echo "Individual logs: test_<vendor>.log"
echo "JUnit XML reports: junit_<vendor>.xml"

# Append summary to results file
echo "" >> "$RESULTS_FILE"
echo "Completed: $(date)" >> "$RESULTS_FILE"
echo "Successful: $SUCCESSFUL_TESTS/$TOTAL_VENDORS" >> "$RESULTS_FILE"

# Analyze test database
echo ""
echo -e "${YELLOW}Analyzing test results database...${NC}"
if [ -f mumps.sqlite ]; then
    uv run python scripts/analyze_test_results.py --latest 2>/dev/null || echo "Database analysis not available"
fi

# Exit with appropriate code
if [ $SUCCESSFUL_TESTS -eq $TOTAL_VENDORS ]; then
    echo -e "${GREEN}All vendor tests passed! ✓${NC}"
    exit 0
else
    echo -e "${RED}Some vendor tests failed!${NC}"
    exit 1
fi
