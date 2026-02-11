# Testing MUMPS Python Interface

## Overview

The MUMPS Python interface has been successfully implemented with comprehensive pytest testing:
- **30 tests** covering all functionality
- **100% test pass rate**
- **Hybrid validation** against scipy.sparse reference solvers
- **SQLite result tracking** for regression detection

## Quick Start

### Run All Tests

```bash
./scripts/run_tests.sh tests/ -v
```

### Run Specific Test Categories

```bash
# Basic functionality
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSBasic -v

# SciPy cross-validation
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSvsSciPy -v

# Performance benchmarks
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSPerformance -v
```

## Test Categories

1. **TestMUMPSBasic** (5 tests)
   - Simple 2x2 system
   - Identity matrix
   - Diagonal matrix
   - Tridiagonal matrix
   - Context manager

2. **TestMUMPSPrecision** (4 tests)
   - Random SPD matrices (10, 50, 100)
   - Ill-conditioned matrices

3. **TestMUMPSSymmetry** (2 tests)
   - Symmetric positive definite (sym=1)
   - General symmetric (sym=2)

4. **TestMUMPSvsSciPy** (6 tests)
   - Validation against scipy.sparse.linalg.spsolve
   - Validation against scipy.sparse.linalg.splu
   - Symmetry validation (sym=0, 1, 2)
   - Large sparse system (500x500)

5. **TestMUMPSErrorHandling** (3 tests)
   - Singular matrix detection
   - Dimension mismatch
   - Finalized instance

6. **TestMUMPSControl** (5 tests)
   - ICNTL parameters
   - CNTL parameters
   - INFO parameters
   - RINFO parameters

7. **TestMUMPSPerformance** (3 benchmarks)
   - Solve time (100, 200)
   - Performance vs scipy

## Key Implementation Details

### Triangle Filtering for Symmetric Matrices

For symmetric matrices (sym > 0), MUMPS requires only the upper triangle:

```python
# Automatic triangle filtering in set_matrix()
if self.sym > 0:
    mask = irn <= jcn
    irn, jcn, a = irn[mask], jcn[mask], a[mask]
```

This is handled automatically by both:
- `MUMPS.set_matrix()` method
- `solve_system()` convenience function

### Library Loading

MUMPS shared libraries require `LD_LIBRARY_PATH` to be set **before** Python starts:

```bash
export LD_LIBRARY_PATH=/path/to/mumps/lib:/path/to/PORD/lib:$LD_LIBRARY_PATH
python3 -m pytest tests/
```

The `scripts/run_tests.sh` wrapper script handles this automatically.

## Test Results Database

All test results are stored in `mumps.sqlite` for tracking:

```bash
# View latest results
uv run python scripts/analyze_test_results.py --latest

# Performance regression detection
uv run python scripts/analyze_test_results.py --regression
```

## BLAS Vendor Testing

Test MUMPS with different BLAS implementations to ensure compatibility and performance:

### Test All Vendors (Full Suite)

```bash
# Rebuild and test all BLAS vendors (openblas, blis, reference)
./scripts/test_all_vendors.sh
```

This script will:
- Build MUMPS for each vendor
- Run complete test suite (30 tests)
- Generate detailed logs and JUnit XML reports
- Store results in SQLite database
- Create summary report

**Output files:**
- `vendor_test_results.txt` - Summary of all vendors
- `build_<vendor>.log` - Build logs for each vendor
- `test_<vendor>.log` - Test output for each vendor
- `junit_<vendor>.xml` - JUnit XML reports

### Quick Test (Current Build)

```bash
# Test current build without rebuilding
./scripts/quick_vendor_test.sh openblas
```

### Compare Vendor Results

```bash
# Analyze and compare results across vendors
./scripts/compare_vendors.sh
```

Shows:
- Test pass/fail rates per vendor
- Performance comparison (execution time)
- Failure details
- Database analysis

### Manual Vendor Testing

```bash
# Build for specific vendor
make clean
make BLAS_VENDOR=openblas dshared

# Run tests
./scripts/run_tests.sh tests/ -v

# Analyze results
uv run python scripts/analyze_test_results.py --vendors
```

### Supported BLAS Vendors

1. **OpenBLAS** (default) - `BLAS_VENDOR=openblas`
2. **BLIS** - `BLAS_VENDOR=blis`
3. **Reference BLAS** - `BLAS_VENDOR=reference`
4. **MKL** - `BLAS_VENDOR=mkl` (Intel Math Kernel Library)
5. **ATLAS** - `BLAS_VENDOR=atlas`

## Known Issues

1. **uv run pytest** doesn't preserve LD_LIBRARY_PATH properly
   - **Solution**: Use `./scripts/run_tests.sh` wrapper script

2. **Numerical precision** differences between MUMPS and scipy
   - Expected: Differences < 1e-9 (relative tolerance)
   - Actual: All tests pass with rtol=1e-8

## Coverage

Current test coverage: **30 tests, 100% pass rate**

Target areas fully covered:
- ✅ Basic linear system solving
- ✅ All symmetry modes (0, 1, 2)
- ✅ Error handling and validation
- ✅ Control parameters (ICNTL, CNTL)
- ✅ SciPy cross-validation
- ✅ Performance benchmarking

## CI/CD Integration

The test suite is ready for CI/CD integration:

```yaml
# Example GitHub Actions workflow
- name: Run MUMPS tests
  run: |
    make dshared  # Build shared libraries
    ./scripts/run_tests.sh tests/ -v
```

## Future Enhancements

- [ ] Multi-precision support (S/C/Z variants)
- [ ] Distributed memory (MPI) testing
- [ ] Out-of-core factorization tests
- [ ] Schur complement tests
