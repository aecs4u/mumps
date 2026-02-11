# Known Issues and Limitations

## Current Implementation Status

### ✅ Fully Working
- Unsymmetric matrices (sym=0)
- Basic MUMPS operations (analyze, factorize, solve)
- scipy.sparse integration
- Error handling
- SQLite result tracking
- Vendor BLAS testing

### ⚠️ Known Limitations (To Be Fixed)

#### 1. Symmetric Matrix Handling (sym=1, sym=2)

**Issue**: Tests for symmetric positive definite (sym=1) and general symmetric (sym=2) matrices currently fail.

**Root Cause**: MUMPS requires only the upper or lower triangle for symmetric matrices, but the current implementation passes the full matrix in COO format.

**Workaround**: Use sym=0 (unsymmetric) for now, even if your matrix is symmetric.

**Fix Required**:
```python
def solve_system(A, b, sym=0):
    if sym > 0:
        # Filter COO entries to only include upper triangle
        mask = A_coo.row <= A_coo.col
        irn = A_coo.row[mask] + 1
        jcn = A_coo.col[mask] + 1
        a = A_coo.data[mask]
    else:
        # Use all entries for unsymmetric
        irn = A_coo.row + 1
        jcn = A_coo.col + 1
        a = A_coo.data
```

**Status**: Documented, fix planned for next iteration.

#### 2. Test Coverage

**Current**: 24/33 tests passing (72.7%)
**Target**: >90% test coverage

**Failing Tests**:
- `test_random_spd_matrix[*]` - Requires sym=1 support
- `test_symmetric_positive_definite` - Requires sym=1 support
- `test_general_symmetric` - Requires sym=2 support
- `test_mumps_symmetry_vs_scipy[1]` - Requires sym=1 support
- `test_mumps_symmetry_vs_scipy[2]` - Requires sym=2 support

## Workarounds

### For Symmetric Matrices

Until symmetric matrix support is fixed, you can:

1. **Use sym=0** (unsymmetric mode) even for symmetric matrices:
   ```python
   # Works but less efficient
   x = solve_system(A_symmetric, b, sym=0)
   ```

2. **Or manually extract upper triangle**:
   ```python
   A_coo = sp.coo_matrix(A)
   mask = A_coo.row <= A_coo.col
   A_upper = sp.coo_matrix(
       (A_coo.data[mask], (A_coo.row[mask], A_coo.col[mask])),
       shape=A_coo.shape
   )

   with MUMPS(n=n, sym=1) as mumps:
       mumps.set_matrix(A_upper.row + 1, A_upper.col + 1, A_upper.data)
       # ... rest of solve
   ```

## Regression Tests

All issues are tracked in the SQLite database. Run:

```bash
uv run python scripts/analyze_test_results.py --latest
```

to see current test status.

## Reporting Issues

When reporting issues, please include:

1. Test name that failed
2. BLAS vendor (`make BLAS_VENDOR=?`)
3. Python version
4. Error message from SQLite analysis:
   ```bash
   uv run python scripts/analyze_test_results.py --latest
   ```

## Version History

### v0.1.0 (Current) - 2026-02-11
- ✅ Basic MUMPS operations working
- ✅ Unsymmetric matrix support
- ✅ SQLite test tracking
- ⚠️ Symmetric matrices not yet supported
- 📊 72.7% test pass rate (24/33 tests)

### Planned for v0.2.0
- 🔧 Fix symmetric matrix handling (sym=1, sym=2)
- 🔧 Improve test coverage to >90%
- 📚 Add more examples
- 🐍 Multi-precision support (S/C/Z)
