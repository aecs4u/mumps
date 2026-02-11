# MUMPS Python Interface

Python interface to the MUMPS sparse direct solver library using `ctypes` and Fortran's `iso_c_binding`.

## Features

- **Clean Python API**: Simple, Pythonic interface to MUMPS
- **NumPy Integration**: Works seamlessly with NumPy arrays
- **SciPy Compatibility**: Accepts `scipy.sparse` matrices
- **Automatic Memory Management**: Context manager handles cleanup
- **Comprehensive Testing**: pytest test suite with 30+ tests
- **SQLite Result Tracking**: All test results stored in database
- **scipy.sparse Validation**: Cross-validated against reference solvers

## Installation

### Build Requirements

1. **Build MUMPS shared libraries** with the C interface:
   ```bash
   make dshared  # Double precision
   ```

2. **Install Python dependencies**:
   ```bash
   uv pip install numpy scipy pytest pytest-cov pytest-benchmark
   ```

### Set Library Paths

The Python interface needs to find the MUMPS shared libraries:

```bash
export LD_LIBRARY_PATH="/path/to/mumps/lib:/path/to/mumps/src/PORD/lib:$LD_LIBRARY_PATH"
```

## Quick Start

### Basic Example

```python
import numpy as np
from pymumps import MUMPS, solve_system

# Simple 2x2 system: [2 3; 3 2] * x = [8; 13]
with MUMPS(n=2) as mumps:
    # Matrix in COO format (1-indexed)
    irn = np.array([1, 1, 2, 2], dtype=np.int32)
    jcn = np.array([1, 2, 1, 2], dtype=np.int32)
    a = np.array([2.0, 3.0, 3.0, 2.0])

    mumps.set_matrix(irn, jcn, a)
    mumps.set_rhs(np.array([8.0, 13.0]))

    mumps.analyze()
    mumps.factorize()
    mumps.solve()

    solution = mumps.get_solution()
    print(f"Solution: {solution}")  # [4.6, -0.4]
```

### Using scipy.sparse

```python
import scipy.sparse as sp
from pymumps import solve_system

# Create a sparse matrix
A = sp.random(100, 100, density=0.1, format='coo')
A = A + A.T + 100 * sp.eye(100)  # Make SPD
b = np.random.rand(100)

# One-line solve
x = solve_system(A, b)

# Verify solution
residual = np.linalg.norm(A @ x - b) / np.linalg.norm(b)
print(f"Relative residual: {residual:.2e}")
```

### Symmetric Matrices

```python
from pymumps import MUMPS

# Symmetric positive definite matrix
with MUMPS(n=n, sym=1) as mumps:  # sym=1 for SPD
    mumps.set_matrix(irn, jcn, a)  # Automatically filters to upper triangle
    mumps.set_rhs(b)
    mumps.analyze()
    mumps.factorize()
    mumps.solve()
    x = mumps.get_solution()
```

Symmetry modes:
- `sym=0`: Unsymmetric (default)
- `sym=1`: Symmetric positive definite (SPD)
- `sym=2`: General symmetric

### Control Parameters

```python
with MUMPS(n=n) as mumps:
    # Suppress output
    mumps.set_icntl(1, -1)  # Error messages
    mumps.set_icntl(2, -1)  # Diagnostics
    mumps.set_icntl(3, -1)  # Global info
    mumps.set_icntl(4, 0)   # Verbosity level

    # Set relative threshold for numerical pivoting
    mumps.set_cntl(1, 0.01)

    # Get solver info
    info = mumps.get_info(1)  # Error code
    rinfo = mumps.get_rinfo(1)  # Estimated flops
```

## Running Tests

### Run All Tests

```bash
# Using test wrapper script
./scripts/run_tests.sh

# Or directly with pytest (requires LD_LIBRARY_PATH set)
pytest tests/ -v
```

### Run Specific Tests

```bash
# Basic functionality
pytest tests/test_mumps.py::TestMUMPSBasic -v

# scipy validation
pytest tests/test_mumps.py::TestMUMPSvsSciPy -v

# Performance benchmarks
pytest tests/test_mumps.py::TestMUMPSPerformance --benchmark-only
```

### Test Coverage

```bash
pytest tests/ --cov=python --cov-report=html
# Open htmlcov/index.html
```

## Test Results Database

All test results are stored in `mumps.sqlite` for historical tracking:

```bash
# Analyze latest test session
uv run python scripts/analyze_test_results.py --latest

# Compare vendor performance
uv run python scripts/analyze_test_results.py --vendors

# Show test history
sqlite3 mumps.sqlite "SELECT * FROM test_sessions ORDER BY timestamp DESC LIMIT 10;"
```

## BLAS Vendor Testing

**Note:** Vendor testing requires rebuilding vendor-specific libraries with the C interface.

### Current Status

The pre-built vendor libraries in `lib/vendors/` were built before the C interface was added. To enable vendor testing:

1. **Rebuild vendor libraries** with C interface:
   ```bash
   # This will rebuild all vendor versions with the C interface
   # (This feature is not yet implemented - placeholder for future work)
   make allshared-vendors
   ```

2. **Run vendor tests**:
   ```bash
   ./scripts/test_all_vendors.sh
   ```

3. **Compare results**:
   ```bash
   ./scripts/compare_vendors.sh
   ```

### Vendor Testing Architecture

The vendor testing system:
- Detects available vendors from `lib/vendors/` directories
- Sets `LD_LIBRARY_PATH` to vendor-specific library directory
- Runs pytest suite for each vendor
- Stores results in SQLite database with vendor tag
- Compares performance and correctness across vendors

Supported vendors:
- **OpenBLAS**: Multi-threaded optimized BLAS
- **BLIS**: AMD/portable BLAS implementation
- **MKL**: Intel Math Kernel Library (if available)
- **Reference**: Netlib reference BLAS (baseline)

## API Reference

### MUMPS Class

```python
class MUMPS(n, sym=0, par=1, comm=0)
```

Main interface to MUMPS solver.

**Parameters:**
- `n` (int): Matrix dimension
- `sym` (int): Symmetry mode (0=unsymmetric, 1=SPD, 2=general symmetric)
- `par` (int): Parallel mode (1=host working, default)
- `comm` (int): MPI communicator (0=sequential, default)

**Methods:**
- `set_matrix(irn, jcn, a)`: Set matrix in COO format (1-indexed)
- `set_rhs(rhs)`: Set right-hand side vector
- `analyze()`: Symbolic factorization (analysis phase)
- `factorize()`: Numerical factorization
- `solve()`: Solve the linear system
- `get_solution()`: Get solution vector
- `set_icntl(idx, val)`: Set integer control parameter
- `get_icntl(idx)`: Get integer control parameter
- `set_cntl(idx, val)`: Set real control parameter
- `get_info(idx)`: Get info parameter
- `get_rinfo(idx)`: Get real info parameter
- `finalize()`: Clean up MUMPS instance

### solve_system Function

```python
def solve_system(A, b, sym=0, **kwargs)
```

Convenience function to solve `Ax=b` using MUMPS.

**Parameters:**
- `A`: Sparse matrix (any scipy.sparse format)
- `b`: Right-hand side vector
- `sym`: Symmetry mode (0, 1, or 2)
- `**kwargs`: Additional arguments passed to MUMPS constructor

**Returns:**
- `x`: Solution vector (NumPy array)

## Architecture

### Three-Layer Design

1. **Fortran iso_c_binding Interface** (`src/*mumps_c_interface.f90`):
   - C-compatible Fortran functions using `iso_c_binding`
   - Opaque handle pattern hides Fortran structures
   - Precision-specific (smumps, dmumps, cmumps, zmumps)

2. **Python ctypes Wrapper** (`python/pymumps/mumps.py`):
   - Loads shared libraries via `ctypes.CDLL`
   - Defines function signatures with `argtypes`/`restype`
   - Provides Pythonic interface with error handling

3. **pytest Test Suite** (`tests/test_mumps.py`):
   - 30+ comprehensive tests
   - scipy.sparse validation for correctness
   - Performance benchmarks vs scipy
   - SQLite result tracking

### Library Loading

The Python interface loads shared libraries in this order:
1. `libpord.so` (optional - PORD ordering)
2. `libmpiseq.so` (optional - MPI stubs)
3. `libmumps_common.so` (common functions)
4. `libdmumps.so` (double precision with C interface)

Libraries are found via `LD_LIBRARY_PATH` environment variable.

## Benefits

### For Testing
- **Hybrid Validation**: Compare against scipy.sparse reference solvers
- **Fast Iteration**: Python tests run faster than compiling Fortran tests
- **Rich Assertions**: pytest provides better assertion messages
- **Fixtures**: Easy setup/teardown with pytest fixtures
- **Parametrization**: Test many cases with `@pytest.mark.parametrize`
- **Coverage**: Easy code coverage with pytest-cov
- **Historical Tracking**: SQLite database stores all test results

### For Users
- **Python Ecosystem**: Integrate with NumPy, SciPy, Matplotlib, Pandas
- **Easy to Use**: Pythonic API with sensible defaults
- **Documentation**: Comprehensive examples and docstrings
- **Error Handling**: Clear error messages with context
- **Memory Safe**: Automatic cleanup with context managers

### For Development
- **CI/CD**: pytest integrates with GitHub Actions
- **Type Hints**: Full type annotations (Python 3.8+)
- **Debugging**: Python debugger (pdb) easier than gdb
- **Profiling**: Python profilers (cProfile, line_profiler)

## Implementation Details

### COO Format

MUMPS uses COO (Coordinate) format with **1-based indexing** (Fortran convention):

```python
# scipy.sparse uses 0-based indexing
A_coo = sp.coo_matrix(A)
irn = A_coo.row + 1  # Convert to 1-indexed
jcn = A_coo.col + 1
a = A_coo.data

mumps.set_matrix(irn, jcn, a)
```

### Symmetric Matrix Storage

For symmetric matrices (`sym > 0`), MUMPS only needs the **upper triangle**:

```python
# Automatic filtering in set_matrix()
if sym > 0:
    mask = irn <= jcn  # Keep only upper triangle
    irn = irn[mask]
    jcn = jcn[mask]
    a = a[mask]
```

### Error Handling

MUMPS errors are detected via `INFO[1]` parameter:

```python
info = mumps.get_info(1)
if info < 0:
    raise RuntimeError(f"MUMPS error (INFO[1]={info}): {error_message}")
```

Common error codes:
- `-1`: Index out of range
- `-6`: Matrix is singular in structure
- `-10`: Numerically singular matrix
- `-13`: Problem allocating memory

## Limitations

### Current Version

- **Double Precision Only**: Currently only `dmumps` (double precision real) is exposed
- **Sequential Mode**: Only sequential (non-MPI) mode supported
- **Single RHS**: Multiple right-hand sides not yet supported
- **No Schur Complement**: Advanced features not yet exposed
- **Vendor Testing**: Requires rebuilding vendor libraries with C interface

### Future Enhancements

1. **Multi-Precision**: Expose smumps (single), cmumps (complex), zmumps (double complex)
2. **MPI Support**: Enable parallel distributed-memory solves
3. **Multiple RHS**: Support solving with multiple right-hand sides simultaneously
4. **Advanced Features**: Schur complement, null space computation, out-of-core
5. **Async Support**: Python asyncio for concurrent solves
6. **Zero-Copy**: NumPy array interface for zero-copy data passing
7. **Vendor Rebuilds**: Automated vendor library rebuilding with C interface

## Troubleshooting

### ImportError: Failed to load MUMPS libraries

**Problem:** Cannot find `libdmumps.so`, `libmpiseq.so`, or `libmumps_common.so`

**Solution:**
```bash
# Set library paths
export LD_LIBRARY_PATH="/path/to/mumps/lib:/path/to/mumps/src/PORD/lib:$LD_LIBRARY_PATH"

# Verify libraries exist
ls -la lib/*.so src/PORD/lib/*.so
```

### AttributeError: undefined symbol: mumps_initialize

**Problem:** MUMPS libraries were built without the C interface

**Solution:**
```bash
# Rebuild shared libraries with C interface
make clean
make dshared
```

### MUMPS error (INFO[1]=-10): Numerically singular matrix

**Problem:** Matrix is singular or nearly singular

**Solution:**
- Check matrix condition number
- Use iterative refinement
- Add diagonal perturbation
- Check for zero diagonal elements

### Test failures with "Cannot open shared object file"

**Problem:** `LD_LIBRARY_PATH` not set when running pytest

**Solution:**
```bash
# Use test wrapper script (sets LD_LIBRARY_PATH automatically)
./scripts/run_tests.sh

# Or set manually before pytest
export LD_LIBRARY_PATH="lib:src/PORD/lib:$LD_LIBRARY_PATH"
pytest tests/
```

## Contributing

### Adding Tests

Tests use pytest with fixtures:

```python
def test_my_feature(small_matrix):
    """Test description."""
    n, irn, jcn, a, b, x_expected = small_matrix

    with MUMPS(n=n) as mumps:
        mumps.set_matrix(irn, jcn, a)
        mumps.set_rhs(b)
        mumps.analyze()
        mumps.factorize()
        mumps.solve()
        x = mumps.get_solution()

        assert_allclose(x, x_expected, rtol=1e-10)
```

### Code Style

```bash
# Format code
black python/ tests/

# Lint code
ruff python/ tests/
```

## License

This Python interface is part of the MUMPS project and follows the same license terms as MUMPS.

## References

- **MUMPS**: http://mumps.enseeiht.fr/
- **MUMPS User Guide**: http://mumps.enseeiht.fr/doc/userguide_5.8.2.pdf
- **iso_c_binding**: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
- **Python ctypes**: https://docs.python.org/3/library/ctypes.html
- **pytest**: https://docs.pytest.org/
