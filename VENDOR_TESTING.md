# MUMPS Vendor Testing Guide

## Overview

The MUMPS Python interface supports testing with multiple BLAS vendor implementations (OpenBLAS, BLIS, MKL, Reference BLAS). This allows comparing performance and correctness across different BLAS libraries.

## Quick Start

### 1. Check Current Status

```bash
./scripts/test_all_vendors.sh
```

Output shows:
- ✅ Vendors **with** C interface (ready for testing)
- ⚠️  Vendors **without** C interface (need rebuilding)

### 2. Rebuild Vendor Libraries

To rebuild vendor libraries with the new C interface:

```bash
./scripts/rebuild_vendor_libraries.sh
```

This script will:
- Auto-detect available BLAS libraries on your system
- Rebuild MUMPS for each vendor with C interface support
- Install libraries to `lib/vendors/<vendor>/`
- Verify C interface symbols are present

### 3. Run Vendor Tests

After rebuilding, run tests for all available vendors:

```bash
./scripts/test_all_vendors.sh
```

## Current Status

Based on the latest detection:

| Vendor | Status | Notes |
|--------|--------|-------|
| **default** | ✅ Ready | Built with `make allshared` |
| **blis** | ✅ Ready | BLIS library with C interface |
| **mkl** | ⚠️ Needs rebuild | Intel MKL (if installed) |
| **openblas** | ⚠️ Needs rebuild | OpenBLAS library |
| **reference** | ⚠️ Needs rebuild | Netlib reference BLAS |

## Vendor Library Requirements

### OpenBLAS
```bash
# Ubuntu/Debian
sudo apt-get install libopenblas-dev

# Fedora/RHEL
sudo dnf install openblas-devel
```

### BLIS
```bash
# From source
git clone https://github.com/flame/blis
cd blis
./configure auto
make -j
sudo make install
```

### Intel MKL
```bash
# Download from Intel:
# https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-download.html

# Or use conda:
conda install -c intel mkl mkl-devel
```

### Reference BLAS
```bash
# Ubuntu/Debian
sudo apt-get install libblas-dev liblapack-dev

# Fedora/RHEL
sudo dnf install blas-devel lapack-devel
```

## Rebuild Script Details

### What It Does

`scripts/rebuild_vendor_libraries.sh`:

1. **Detects Available BLAS**: Checks system for installed BLAS libraries
2. **Builds Per-Vendor**: Runs `make allshared LIBBLAS=<vendor-flags>` for each
3. **Installs Libraries**: Copies shared libraries to `lib/vendors/<vendor>/`
4. **Verifies C Interface**: Checks for `mumps_initialize` symbol

### Manual Rebuild (Alternative)

If the script doesn't detect your BLAS library correctly:

```bash
# Example: Build with specific OpenBLAS path
make clean
make allshared LIBBLAS="-L/custom/path -lopenblas"

# Copy libraries to vendor directory
mkdir -p lib/vendors/openblas_custom
cp lib/lib*.so lib/vendors/openblas_custom/
```

## Test Script Features

`scripts/test_all_vendors.sh`:

- **Auto-Detection**: Finds vendors with C interface automatically
- **Smart Warnings**: Only warns about vendors needing rebuild
- **Parallel Testing**: Tests each vendor sequentially
- **Result Tracking**: Saves to `vendor_test_results.txt` and SQLite
- **JUnit Output**: Generates `junit_<vendor>.xml` for CI/CD

### Test Output

```
=== MUMPS Multi-Vendor Test Suite ===

Found 2 vendor(s) with C interface: default blis
⚠  Found 3 vendor(s) without C interface: mkl openblas reference
⚠  To rebuild with C interface: ./scripts/rebuild_vendor_libraries.sh

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Testing BLAS vendor: default
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Testing default...
...
✓ Tests completed for default
  48 passed in 10.23s

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Testing BLAS vendor: blis
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Testing blis...
...
✓ Tests completed for blis
  48 passed in 9.87s

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
FINAL SUMMARY
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Total vendors tested: 2
Successful tests: 2/2

All vendor tests passed! ✓
```

## Comparing Vendor Performance

After running tests, compare performance:

```bash
./scripts/compare_vendors.sh
```

Or query the SQLite database directly:

```bash
sqlite3 mumps.sqlite "
SELECT vendor, test_name, duration_ms
FROM test_results
WHERE test_name LIKE '%performance%'
ORDER BY vendor, test_name;
"
```

## Troubleshooting

### "No MUMPS libraries with C interface found"

**Problem**: No libraries with C interface exist

**Solution**:
```bash
# Build default libraries first
make allshared

# Then rebuild vendor-specific
./scripts/rebuild_vendor_libraries.sh
```

### "Cannot detect BLAS library for <vendor>"

**Problem**: Rebuild script can't find BLAS library

**Solutions**:
1. Install the BLAS library (see requirements above)
2. Set library path: `export LD_LIBRARY_PATH=/custom/path:$LD_LIBRARY_PATH`
3. Manually specify in Makefile.inc: `LIBBLAS = -L/path -l<blas>`

### "Built libraries missing C interface symbols"

**Problem**: Libraries built but don't have C interface

**Cause**: Source files with C interface (`*mumps_c_interface.f90`) missing or not compiled

**Solution**:
```bash
# Verify C interface source files exist
ls src/*mumps_c_interface.f90

# Should show:
# src/cmumps_c_interface.f90
# src/dmumps_c_interface.f90
# src/smumps_c_interface.f90
# src/zmumps_c_interface.f90

# Rebuild if missing
make clean
make allshared
```

### Tests fail with "undefined symbol: mumps_initialize"

**Problem**: Python trying to load wrong library version

**Solution**: Check LD_LIBRARY_PATH priority:
```bash
# Verify library path
echo $LD_LIBRARY_PATH

# Should show vendor directory first
LD_LIBRARY_PATH=lib/vendors/openblas:lib:src/PORD/lib
```

## Performance Comparison Example

Typical performance results (1000×1000 sparse system):

| Vendor | Time (ms) | GFLOPS | Notes |
|--------|-----------|--------|-------|
| **MKL** | 85 | 12.5 | Fastest (Intel optimized) |
| **OpenBLAS** | 120 | 8.9 | Good general performance |
| **BLIS** | 135 | 7.9 | AMD/portable optimized |
| **Reference** | 450 | 2.4 | Baseline (no optimization) |

*Note: Actual performance depends on CPU architecture, system load, and matrix properties*

## CI/CD Integration

### GitHub Actions Example

```yaml
name: MUMPS Vendor Tests

on: [push, pull_request]

jobs:
  test-vendors:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        vendor: [openblas, blis, reference]

    steps:
      - uses: actions/checkout@v3

      - name: Install BLAS library
        run: |
          if [ "${{ matrix.vendor }}" = "openblas" ]; then
            sudo apt-get install -y libopenblas-dev
          elif [ "${{ matrix.vendor }}" = "blis" ]; then
            # Build BLIS from source
            git clone https://github.com/flame/blis
            cd blis && ./configure auto && make -j && sudo make install
          else
            sudo apt-get install -y libblas-dev liblapack-dev
          fi

      - name: Build MUMPS for vendor
        run: |
          make clean
          make allshared

      - name: Run tests
        run: ./scripts/test_all_vendors.sh

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: vendor-results-${{ matrix.vendor }}
          path: |
            junit_*.xml
            vendor_test_results.txt
```

## Best Practices

1. **Test with Reference First**: Always verify correctness with reference BLAS before performance testing
2. **Run Multiple Times**: BLAS performance can vary due to CPU throttling, cache effects
3. **Compare on Same Hardware**: Performance comparisons only meaningful on identical systems
4. **Check Numerical Accuracy**: Use `test_mumps_matches_scipy_*` tests to verify correctness
5. **Monitor Database**: Track performance regressions with SQLite database over time

## Files and Directories

```
mumps/
├── lib/
│   ├── libdmumps.so                 # Default build
│   ├── libsmumps.so
│   ├── libcmumps.so
│   ├── libzmumps.so
│   ├── libmumps_common.so
│   ├── libmpiseq.so
│   ├── libpord.so
│   └── vendors/                     # Vendor-specific libraries
│       ├── blis/
│       │   ├── libdmumps.so        # BLIS-linked version
│       │   └── ...
│       ├── mkl/
│       │   └── ...
│       ├── openblas/
│       │   └── ...
│       └── reference/
│           └── ...
├── scripts/
│   ├── rebuild_vendor_libraries.sh  # Rebuild script
│   ├── test_all_vendors.sh          # Main test runner
│   └── compare_vendors.sh           # Performance comparison
└── vendor_test_results.txt          # Latest results
```

## References

- **BLAS Implementations**: https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms#Implementations
- **OpenBLAS**: https://www.openblas.net/
- **BLIS**: https://github.com/flame/blis
- **Intel MKL**: https://software.intel.com/mkl
- **MUMPS**: http://mumps.enseeiht.fr/
