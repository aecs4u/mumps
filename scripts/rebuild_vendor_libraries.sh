#!/bin/bash
#
# Rebuild vendor-specific MUMPS libraries with C interface
# This creates vendor-specific libraries in lib/vendors/<vendor>/
#
# Uses dynamic library detection: pkg-config → ldconfig → env → filesystem
#

# Note: No 'set -e' so we can continue building other vendors if one fails

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" || exit 1
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)" || exit 1
cd "$PROJECT_ROOT" || exit 1

# Available BLAS vendors
VENDORS=("reference" "openblas" "blis" "mkl")

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m'

# ──────────────────────────────────────────────────────────────
# Parse command-line arguments
# ──────────────────────────────────────────────────────────────
FORCE=0
for arg in "$@"; do
    case "$arg" in
        --force|-f)
            FORCE=1
            ;;
        --help|-h)
            echo "Usage: $(basename "$0") [--force|-f] [--help|-h]"
            echo ""
            echo "Build vendor-specific MUMPS libraries with C interface."
            echo "By default, skips vendors whose libraries already exist."
            echo ""
            echo "Options:"
            echo "  --force, -f   Rebuild all vendors even if libraries exist"
            echo "  --help, -h    Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $arg" >&2
            echo "Usage: $(basename "$0") [--force|-f] [--help|-h]" >&2
            exit 1
            ;;
    esac
done

echo -e "${BLUE}=== MUMPS Vendor Library Rebuild ===${NC}"
if [ "$FORCE" -eq 1 ]; then
    echo -e "${BLUE}Rebuilding ALL vendor libraries (--force)${NC}"
else
    echo -e "${BLUE}Building missing vendor libraries (use --force to rebuild all)${NC}"
fi
echo ""

# ──────────────────────────────────────────────────────────────
# Dynamic BLAS library detection
# Priority: pkg-config → ldconfig → environment → filesystem
# ──────────────────────────────────────────────────────────────

has_pkg_config() {
    command -v pkg-config >/dev/null 2>&1
}

has_ldconfig() {
    command -v ldconfig >/dev/null 2>&1
}

# Generic pkg-config probe: returns link flags or empty string
pkg_config_probe() {
    local pkg_name=$1
    if has_pkg_config && pkg-config --exists "$pkg_name" 2>/dev/null; then
        pkg-config --libs "$pkg_name" 2>/dev/null
        return 0
    fi
    return 1
}

# Generic ldconfig probe: searches for library name, returns path
ldconfig_probe() {
    local lib_name=$1
    if has_ldconfig; then
        ldconfig -p 2>/dev/null | grep "$lib_name" | head -1 | sed 's/.*=> //'
    fi
}

detect_blas_lib() {
    local vendor=$1
    case "$vendor" in
        reference)
            detect_reference_blas
            ;;
        openblas)
            detect_openblas
            ;;
        blis)
            detect_blis
            ;;
        mkl)
            detect_mkl
            ;;
        *)
            echo ""
            ;;
    esac
}

detect_reference_blas() {
    local method=""

    # 1. pkg-config
    local flags
    for pkg in blas blas-netlib; do
        flags=$(pkg_config_probe "$pkg" 2>/dev/null)
        if [ -n "$flags" ]; then
            local lapack_flags
            lapack_flags=$(pkg_config_probe "lapack" 2>/dev/null) || true
            echo "$flags $lapack_flags"
            echo -e "${CYAN}  Detection: pkg-config ($pkg)${NC}" >&2
            return 0
        fi
    done

    # 2. ldconfig
    local blas_path lapack_path
    blas_path=$(ldconfig_probe "libblas.so")
    lapack_path=$(ldconfig_probe "liblapack.so")
    if [ -n "$blas_path" ] && [ -n "$lapack_path" ]; then
        local blas_dir lapack_dir
        blas_dir=$(dirname "$blas_path")
        lapack_dir=$(dirname "$lapack_path")
        if [ "$blas_dir" = "$lapack_dir" ]; then
            echo "-L$blas_dir -lblas -llapack"
        else
            echo "-L$blas_dir -lblas -L$lapack_dir -llapack"
        fi
        echo -e "${CYAN}  Detection: ldconfig${NC}" >&2
        return 0
    fi

    # 3. Filesystem search
    for path in /usr/lib/x86_64-linux-gnu /usr/lib64 /usr/lib; do
        if [ -f "$path/libblas.so" ] && [ -f "$path/liblapack.so" ]; then
            echo "-L$path -lblas -llapack"
            echo -e "${CYAN}  Detection: filesystem ($path)${NC}" >&2
            return 0
        fi
    done

    echo ""
}

detect_openblas() {
    # 1. pkg-config
    local flags
    for pkg in openblas openblas-pthread openblas-openmp; do
        flags=$(pkg_config_probe "$pkg" 2>/dev/null)
        if [ -n "$flags" ]; then
            echo "$flags"
            echo -e "${CYAN}  Detection: pkg-config ($pkg)${NC}" >&2
            return 0
        fi
    done

    # 2. ldconfig
    local lib_path
    lib_path=$(ldconfig_probe "libopenblas.so")
    if [ -n "$lib_path" ] && [ -f "$lib_path" ]; then
        local lib_dir
        lib_dir=$(dirname "$lib_path")
        echo "-L$lib_dir -lopenblas"
        echo -e "${CYAN}  Detection: ldconfig${NC}" >&2
        return 0
    fi

    # 3. Filesystem search
    for path in /usr/lib/x86_64-linux-gnu/openblas-pthread /usr/lib/x86_64-linux-gnu /usr/lib64 /usr/lib /usr/local/lib; do
        if [ -f "$path/libopenblas.so" ]; then
            echo "-L$path -lopenblas"
            echo -e "${CYAN}  Detection: filesystem ($path)${NC}" >&2
            return 0
        fi
    done

    echo ""
}

detect_blis() {
    # 1. pkg-config
    local flags
    for pkg in blis blis-mt blis-pthread; do
        flags=$(pkg_config_probe "$pkg" 2>/dev/null)
        if [ -n "$flags" ]; then
            echo "$flags"
            echo -e "${CYAN}  Detection: pkg-config ($pkg)${NC}" >&2
            return 0
        fi
    done

    # 2. ldconfig (prefer multi-threaded)
    local lib_path
    for lib_name in libblis-mt.so libblis.so; do
        lib_path=$(ldconfig_probe "$lib_name")
        if [ -n "$lib_path" ] && [ -f "$lib_path" ]; then
            local lib_dir lib_flag
            lib_dir=$(dirname "$lib_path")
            if echo "$lib_path" | grep -q "blis-mt"; then
                lib_flag="-lblis-mt"
            else
                lib_flag="-lblis"
            fi
            echo "-L$lib_dir $lib_flag"
            echo -e "${CYAN}  Detection: ldconfig${NC}" >&2
            return 0
        fi
    done

    # 3. Filesystem search (prefer multi-threaded)
    for lib_name in libblis-mt.so libblis.so; do
        for path in /usr/local/lib /usr/lib/x86_64-linux-gnu /usr/lib64 /usr/lib; do
            if [ -f "$path/$lib_name" ]; then
                local lib_flag
                if [ "$lib_name" = "libblis-mt.so" ]; then
                    lib_flag="-lblis-mt"
                else
                    lib_flag="-lblis"
                fi
                echo "-L$path $lib_flag"
                echo -e "${CYAN}  Detection: filesystem ($path)${NC}" >&2
                return 0
            fi
        done
    done

    echo ""
}

detect_mkl() {
    # 1. pkg-config (Intel oneAPI sets up pkg-config for MKL)
    local flags
    for pkg in mkl-dynamic-lp64-seq mkl-dynamic-ilp64-seq mkl-sdl; do
        flags=$(pkg_config_probe "$pkg" 2>/dev/null)
        if [ -n "$flags" ]; then
            echo "$flags"
            echo -e "${CYAN}  Detection: pkg-config ($pkg)${NC}" >&2
            return 0
        fi
    done

    # 2. ldconfig
    local lib_path
    lib_path=$(ldconfig_probe "libmkl_rt.so")
    if [ -n "$lib_path" ] && [ -f "$lib_path" ]; then
        local lib_dir
        lib_dir=$(dirname "$lib_path")
        echo "-L$lib_dir -lmkl_rt"
        echo -e "${CYAN}  Detection: ldconfig${NC}" >&2
        return 0
    fi

    # 3. MKLROOT environment variable
    if [ -n "$MKLROOT" ]; then
        for subdir in lib/intel64 lib; do
            if [ -f "$MKLROOT/$subdir/libmkl_rt.so" ]; then
                echo "-L$MKLROOT/$subdir -lmkl_rt"
                echo -e "${CYAN}  Detection: MKLROOT env ($MKLROOT/$subdir)${NC}" >&2
                return 0
            fi
        done
    fi

    # 4. CMake cache (reuse CMake's detection if available)
    if [ -f "$PROJECT_ROOT/build/CMakeCache.txt" ]; then
        local cmake_blas
        cmake_blas=$(grep "^BLAS_LIBRARIES" "$PROJECT_ROOT/build/CMakeCache.txt" 2>/dev/null | cut -d= -f2-)
        if [ -n "$cmake_blas" ] && echo "$cmake_blas" | grep -qi "mkl"; then
            echo "$cmake_blas"
            echo -e "${CYAN}  Detection: CMake cache${NC}" >&2
            return 0
        fi
    fi

    # 5. Expanded filesystem search (includes Intel oneAPI paths)
    for path in \
        /opt/intel/oneapi/mkl/latest/lib \
        /opt/intel/oneapi/mkl/latest/lib/intel64 \
        /opt/intel/oneapi/mkl/*/lib \
        /opt/intel/oneapi/mkl/*/lib/intel64 \
        /opt/intel/mkl/lib/intel64 \
        /usr/lib/x86_64-linux-gnu \
        /usr/lib64; do
        # Handle glob expansion
        for expanded_path in $path; do
            if [ -f "$expanded_path/libmkl_rt.so" ]; then
                echo "-L$expanded_path -lmkl_rt"
                echo -e "${CYAN}  Detection: filesystem ($expanded_path)${NC}" >&2
                return 0
            fi
        done
    done

    echo ""
}

# Check if a vendor is available
check_vendor_available() {
    local vendor=$1
    local blas_flags=$(detect_blas_lib "$vendor" 2>/dev/null)

    if [ -z "$blas_flags" ]; then
        return 1
    fi
    return 0
}

# Check if a vendor's libraries are already built and valid
vendor_libs_exist() {
    local vendor=$1
    local vendor_dir="lib/vendors/$vendor"

    # Check that vendor dir exists and has the main library
    if [ ! -f "$vendor_dir/libdmumps.so" ]; then
        return 1
    fi

    # Verify C interface symbols are present
    if ! nm "$vendor_dir/libdmumps.so" 2>/dev/null | grep -q "mumps_initialize"; then
        return 1
    fi

    return 0
}

# Build libraries for a specific vendor
build_vendor() {
    local vendor=$1
    local vendor_dir="lib/vendors/$vendor"

    echo -e "${YELLOW}Building $vendor libraries...${NC}"

    # Detect BLAS flags (stderr shows detection method)
    local blas_flags=$(detect_blas_lib "$vendor")
    if [ -z "$blas_flags" ]; then
        echo -e "${RED}✗ Cannot detect BLAS library for $vendor${NC}"
        echo -e "${RED}  Skipping $vendor${NC}"
        return 1
    fi

    echo -e "  BLAS flags: ${blas_flags}"

    # Create vendor directory
    mkdir -p "$vendor_dir"

    # Clean previous build
    echo "  Cleaning previous build..."
    make clean > /dev/null 2>&1 || true

    # Build with vendor-specific BLAS
    echo "  Building shared libraries with $vendor BLAS..."
    if make allshared LIBBLAS="$blas_flags" LAPACK="" > "build_${vendor}.log" 2>&1; then
        # Copy libraries to vendor directory
        echo "  Copying libraries to $vendor_dir..."
        cp lib/lib*.so "$vendor_dir/" 2>/dev/null || true

        # Verify C interface symbols
        if nm "$vendor_dir/libdmumps.so" | grep -q "mumps_initialize"; then
            echo -e "${GREEN}✓ Successfully built $vendor libraries with C interface${NC}"
            rm "build_${vendor}.log"
            return 0
        else
            echo -e "${RED}✗ Built libraries missing C interface symbols${NC}"
            echo -e "${YELLOW}  The C interface source files may not be compiled${NC}"
            echo -e "${YELLOW}  Try: make clean && make allshared${NC}"
            return 1
        fi
    else
        echo -e "${RED}✗ Build failed for $vendor${NC}"
        echo ""
        echo -e "${YELLOW}Build error details:${NC}"

        # Extract and show key error information
        if grep -q "cannot find.*\.o" "build_${vendor}.log"; then
            echo -e "${YELLOW}  → Linker cannot find object files${NC}"
            echo -e "${YELLOW}  → This may be a build system issue${NC}"
            echo -e "${YELLOW}  → Try building manually: make clean && make allshared LIBBLAS=\"$blas_flags\"${NC}"
        elif grep -q "undefined reference" "build_${vendor}.log"; then
            echo -e "${YELLOW}  → Undefined references found${NC}"
            echo -e "${YELLOW}  → The BLAS library may be incomplete${NC}"
            grep "undefined reference" "build_${vendor}.log" | head -5 | sed 's/^/    /'
        elif grep -q "No such file" "build_${vendor}.log"; then
            echo -e "${YELLOW}  → Missing files or libraries${NC}"
            grep "No such file" "build_${vendor}.log" | head -5 | sed 's/^/    /'
        else
            echo -e "${YELLOW}  → Check build_${vendor}.log for full details${NC}"
            tail -20 "build_${vendor}.log" | sed 's/^/    /'
        fi
        echo ""
        echo -e "${YELLOW}Full log saved to: build_${vendor}.log${NC}"
        return 1
    fi
}

# ──────────────────────────────────────────────────────────────
# Main build loop
# ──────────────────────────────────────────────────────────────

# First, show detection summary
echo -e "${BLUE}Detecting BLAS libraries...${NC}"
echo ""
for vendor in "${VENDORS[@]}"; do
    if check_vendor_available "$vendor"; then
        blas_flags=$(detect_blas_lib "$vendor" 2>/dev/null)
        echo -e "  ${GREEN}✓ $vendor${NC}: $blas_flags"
    else
        echo -e "  ${YELLOW}✗ $vendor${NC}: not found"
    fi
done
echo ""

SUCCESSFUL_BUILDS=0
FAILED_BUILDS=()
SKIPPED_VENDORS=()

for vendor in "${VENDORS[@]}"; do
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Vendor: $vendor${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

    if ! check_vendor_available "$vendor"; then
        echo -e "${YELLOW}⊘ $vendor BLAS library not found - skipping${NC}"
        SKIPPED_VENDORS+=("$vendor")
        continue
    fi

    # Skip if libraries already exist (unless --force)
    if [ "$FORCE" -eq 0 ] && vendor_libs_exist "$vendor"; then
        echo -e "${GREEN}✓ $vendor libraries already exist - skipping${NC}"
        echo -e "  To rebuild: ${CYAN}$(basename "$0") --force${NC}  or  ${CYAN}make allshared LIBBLAS=\"$(detect_blas_lib "$vendor" 2>/dev/null)\"${NC}"
        ((SUCCESSFUL_BUILDS++))
        continue
    fi

    # Build vendor (script will continue even if this fails)
    if build_vendor "$vendor"; then
        ((SUCCESSFUL_BUILDS++))
    else
        FAILED_BUILDS+=("$vendor")
    fi
done

# Final summary
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}REBUILD SUMMARY${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Vendors attempted: ${#VENDORS[@]}"
echo "Successful builds: $SUCCESSFUL_BUILDS"

if [ ${#SKIPPED_VENDORS[@]} -gt 0 ]; then
    echo -e "${YELLOW}Skipped vendors (${#SKIPPED_VENDORS[@]}): ${SKIPPED_VENDORS[*]}${NC} (not installed)"
fi

if [ ${#FAILED_BUILDS[@]} -gt 0 ]; then
    echo -e "${RED}Failed builds (${#FAILED_BUILDS[@]}): ${FAILED_BUILDS[*]}${NC}"
    echo ""
    echo -e "${YELLOW}Common failure causes:${NC}"
    echo "  1. Build system issues (PORD object file linking)"
    echo "  2. Incomplete BLAS library installation"
    echo "  3. Missing C interface source files"
    echo ""
    echo -e "${YELLOW}Troubleshooting steps:${NC}"
    echo "  1. Try building default first: make clean && make allshared"
    echo "  2. Check build logs: build_<vendor>.log"
    echo "  3. Verify BLAS installation:"
    for failed_vendor in "${FAILED_BUILDS[@]}"; do
        case "$failed_vendor" in
            openblas)
                echo "     - OpenBLAS: dpkg -l | grep openblas"
                ;;
            blis)
                echo "     - BLIS: ls -l /usr/local/lib/libblis*.so"
                ;;
            mkl)
                echo "     - MKL: dpkg -l | grep intel-oneapi-mkl"
                echo "     - MKL: ls /opt/intel/oneapi/mkl/*/lib/libmkl_rt.so"
                ;;
        esac
    done
    echo ""
fi

echo ""
echo "Available vendor libraries with C interface:"
AVAILABLE_COUNT=0
for vendor_dir in lib/vendors/*/; do
    if [ -d "$vendor_dir" ]; then
        vendor=$(basename "$vendor_dir")
        if [ -f "$vendor_dir/libdmumps.so" ]; then
            if nm "$vendor_dir/libdmumps.so" 2>/dev/null | grep -q "mumps_initialize"; then
                echo -e "  ${GREEN}✓ $vendor${NC} (ready for testing)"
                ((AVAILABLE_COUNT++))
            else
                echo -e "  ${RED}✗ $vendor${NC} (C interface missing - rebuild required)"
            fi
        fi
    fi
done

echo ""
if [ $SUCCESSFUL_BUILDS -gt 0 ] || [ $AVAILABLE_COUNT -gt 0 ]; then
    echo -e "${GREEN}${AVAILABLE_COUNT} vendor librar$([ $AVAILABLE_COUNT -eq 1 ] && echo 'y' || echo 'ies') ready for testing!${NC}"
    echo ""
    echo "Next steps:"
    echo "  1. Test all available vendors: ./scripts/test_all_vendors.sh"
    echo "  2. View test results in webapp: http://localhost:8000/pytest"
    echo "  3. Compare vendor performance: ./scripts/compare_vendors.sh"
    exit 0
else
    echo -e "${RED}No vendor libraries are currently available${NC}"
    echo ""
    echo "To create default build:"
    echo "  make clean && make allshared"
    echo ""
    echo "For help, see: VENDOR_TESTING.md"
    exit 1
fi
