# MUMPS Python Test Suite

Comprehensive pytest-based test suite for MUMPS Python interface with scipy validation and SQLite result tracking.

## Quick Start

```bash
# From project root - easiest method
./scripts/run_tests.sh tests/ -v

# Or manually set LD_LIBRARY_PATH
LD_LIBRARY_PATH=lib:PORD/lib uv run pytest tests/ -v

# Run specific test
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSBasic -v

# Run with coverage
./scripts/run_tests.sh tests/ --cov=python --cov-report=html
```

## Test Categories

### Basic Tests (`test_mumps.py::TestMUMPSBasic`)
- Identity matrix
- Diagonal matrix
- Tridiagonal matrix
- Context manager

```bash
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSBasic -v
```

### Precision Tests (`test_mumps.py::TestMUMPSPrecision`)
- Random SPD matrices
- Ill-conditioned matrices

```bash
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSPrecision -v
```

### Symmetry Tests (`test_mumps.py::TestMUMPSSymmetry`)
- Symmetric positive definite (sym=1)
- General symmetric (sym=2)

```bash
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSSymmetry -v
```

### SciPy Validation (`test_mumps.py::TestMUMPSvsSciPy`)
Cross-validation against scipy.sparse reference solvers:
- spsolve
- splu

```bash
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSvsSciPy -v
```

### Error Handling (`test_mumps.py::TestMUMPSErrorHandling`)
- Singular matrix detection
- Dimension mismatch
- Finalized instance

```bash
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSErrorHandling -v
```

### Control Parameters (`test_mumps.py::TestMUMPSControl`)
- ICNTL/CNTL setting
- INFO/RINFO reading

```bash
./scripts/run_tests.sh tests/test_mumps.py::TestMUMPSControl -v
```

### Vendor Tests (`test_vendors.py`)
BLAS vendor compatibility tests

```bash
./scripts/run_tests.sh tests/test_vendors.py -v
```

## SQLite Results Database

All test results are automatically saved to `mumps.sqlite`.

### Analyze Results

```bash
# Latest session
uv run python scripts/analyze_test_results.py --latest

# Overall statistics
uv run python scripts/analyze_test_results.py --stats

# Compare two sessions
uv run python scripts/analyze_test_results.py --compare 1 2

# Vendor comparison
uv run python scripts/analyze_test_results.py --vendors

# Performance regression detection
uv run python scripts/analyze_test_results.py --regression

# Test history
uv run python scripts/analyze_test_results.py --history "test_mumps.py::TestMUMPSBasic::test_simple_2x2_system"

# All analyses
uv run python scripts/analyze_test_results.py --all
```

## Testing Different BLAS Vendors

```bash
# Test with different BLAS vendors
for vendor in openblas mkl blis reference; do
  echo "Testing with $vendor"
  make clean
  make BLAS_VENDOR=$vendor dshared
  ./scripts/run_tests.sh tests/ -v
done

# Compare vendor performance
uv run python scripts/analyze_test_results.py --vendors
```

## Test Markers

```bash
# Run only fast tests (exclude slow)
./scripts/run_tests.sh tests/ -m "not slow"

# Run only benchmark tests
./scripts/run_tests.sh tests/ -m benchmark

# Run only vendor tests
./scripts/run_tests.sh tests/ -m vendor
```

## Continuous Integration

The test suite integrates with CI/CD through SQLite tracking:

1. Run tests in CI: `./scripts/run_tests.sh tests/ -v`
2. Database is updated: `mumps.sqlite`
3. Analyze: `uv run python scripts/analyze_test_results.py --latest`
4. Check for regressions: `uv run python scripts/analyze_test_results.py --regression`

## Troubleshooting

### Library Not Found

If you see `libmpiseq.so: cannot open shared object file`:

```bash
# Use the wrapper script
./scripts/run_tests.sh tests/ -v

# Or set LD_LIBRARY_PATH manually
export LD_LIBRARY_PATH=lib:PORD/lib:$LD_LIBRARY_PATH
uv run pytest tests/ -v
```

### Import Errors

Ensure libraries are built:

```bash
make dshared
ls -lh lib/*.so
```

## File Structure

```
tests/
├── README.md                    # This file
├── conftest.py                  # pytest configuration & fixtures
├── pytest.ini                   # pytest settings
├── pytest_sqlite_plugin.py      # SQLite result storage plugin
├── test_mumps.py               # Main test suite (320 lines)
└── test_vendors.py             # BLAS vendor tests (90 lines)
```

## Coverage

Generate coverage report:

```bash
./scripts/run_tests.sh tests/ --cov=python --cov-report=html
# Open htmlcov/index.html
```

Target: >85% code coverage

## Database Schema

```sql
-- Sessions table
CREATE TABLE test_sessions (
    id INTEGER PRIMARY KEY,
    timestamp TEXT,
    python_version TEXT,
    platform TEXT,
    blas_vendor TEXT,
    num_tests INTEGER,
    num_passed INTEGER,
    num_failed INTEGER,
    num_skipped INTEGER,
    total_duration REAL
);

-- Results table
CREATE TABLE test_results (
    id INTEGER PRIMARY KEY,
    session_id INTEGER,
    test_name TEXT,
    test_file TEXT,
    test_class TEXT,
    test_function TEXT,
    status TEXT,
    duration REAL,
    error_message TEXT,
    timestamp TEXT,
    FOREIGN KEY (session_id) REFERENCES test_sessions(id)
);
```

## Query Examples

```bash
# Slowest tests
sqlite3 mumps.sqlite "SELECT test_name, AVG(duration) as avg
                      FROM test_results GROUP BY test_name
                      ORDER BY avg DESC LIMIT 10"

# Failure rates
sqlite3 mumps.sqlite "SELECT test_name,
                      SUM(CASE WHEN status='failed' THEN 1 ELSE 0 END)*100.0/COUNT(*) as rate
                      FROM test_results GROUP BY test_name
                      HAVING rate > 0 ORDER BY rate DESC"

# Performance trends
sqlite3 mumps.sqlite "SELECT s.timestamp, AVG(r.duration)
                      FROM test_sessions s JOIN test_results r ON s.id = r.session_id
                      GROUP BY s.id ORDER BY s.timestamp"
```
