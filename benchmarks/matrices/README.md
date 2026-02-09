# MUMPS Test Matrix Fixtures

This directory contains test matrix fixtures in MUMPS coordinate format for benchmarking and testing.

## Matrix Format (.coo)

MUMPS coordinate format is a simple text-based sparse matrix format:

```
# Comment line (optional)
# n nnz
<dimension> <number_of_nonzeros>
# i j value
<row> <column> <value>
<row> <column> <value>
...
```

**Important**: Row and column indices are **1-based** (Fortran/MUMPS convention).

### Example (2×2 diagonal matrix):
```
# Diagonal 2×2 matrix
# n nnz
2 2
# i j value
1 1 1.0
2 2 2.0
```

## Available Matrices

### Small (for quick unit tests)
- `test_tiny_10x10_diagonal.coo` - 10×10 diagonal matrix (10 nonzeros)
- `test_small_10x10_laplacian.coo` - 2D Laplacian, 10×10 grid, 100 unknowns (460 nonzeros)
- `test_small_100_tridiagonal.coo` - 100×100 tridiagonal (298 nonzeros)

### Medium (for moderate benchmarks)
- `test_medium_50x50_laplacian.coo` - 2D Laplacian, 50×50 grid, 2,500 unknowns (12,300 nonzeros)
- `test_medium_1000_tridiagonal.coo` - 1,000×1,000 tridiagonal (2,998 nonzeros)
- `test_medium_1000x1000_diagonal.coo` - 1,000×1,000 diagonal (1,000 nonzeros)

### Large (for performance testing)
- `test_large_100x100_laplacian.coo` - 2D Laplacian, 100×100 grid, 10,000 unknowns (49,600 nonzeros)
- `test_large_10000_tridiagonal.coo` - 10,000×10,000 tridiagonal (29,998 nonzeros)

### Huge (for stress testing)
- `test_huge_200x200_laplacian.coo` - 2D Laplacian, 200×200 grid, 40,000 unknowns (199,200 nonzeros)
- `test_huge_50000_tridiagonal.coo` - 50,000×50,000 tridiagonal (149,998 nonzeros)

## Matrix Properties

All test matrices are:
- **Symmetric**: A = A^T
- **Positive Definite (SPD)**: All eigenvalues > 0
- **Sparse**: Structured sparsity patterns (diagonal, tridiagonal, Laplacian)

This ensures:
- Guaranteed convergence for iterative solvers
- No pivoting required for Cholesky factorization
- Predictable conditioning and numerical stability

## Generating Matrices

### Generate all predefined sizes:
```bash
python3 scripts/generate_test_matrices.py --size all
```

### Generate specific size category:
```bash
python3 scripts/generate_test_matrices.py --size medium
```

### Generate custom matrix:
```bash
# 2D Laplacian on 75×75 grid
python3 scripts/generate_test_matrices.py --type laplacian --ngrid 75 --output custom_laplacian.coo

# Diagonal matrix 500×500
python3 scripts/generate_test_matrices.py --type diagonal --n 500 --output custom_diag.coo

# Tridiagonal matrix 5000×5000
python3 scripts/generate_test_matrices.py --type tridiagonal --n 5000 --output custom_tri.coo
```

## Matrix Types

### Diagonal Matrix
- Pattern: A(i,i) = i for i=1..n
- Sparsity: n nonzeros
- Trivial to solve: x = b ./ diag(A)

### Tridiagonal Matrix
- Pattern:
  - A(i,i) = 2.0
  - A(i,i-1) = -1.0
  - A(i,i+1) = -1.0
- Sparsity: 3n - 2 nonzeros
- Common in 1D discretizations (heat equation, wave equation)

### 2D Laplacian (5-point stencil)
- Pattern: Discretization of -∇²u = f on square domain
- Stencil:
  ```
        -1
     -1  4 -1
        -1
  ```
- Sparsity: ~5n nonzeros (interior nodes have 5, boundary nodes fewer)
- Common in 2D PDEs (Poisson, diffusion, heat)
- Grid ordering: row-major (i = row*ngrid + col + 1)

## Metadata

See `fixtures.json` for complete metadata including:
- Matrix dimensions and sparsity
- Problem type and physical interpretation
- Symmetry and definiteness properties
- File sizes and recommended use cases

## Future Enhancements

- [ ] Matrix Market format (.mtx) support
- [ ] Converter script between .coo and .mtx
- [ ] C matrix loader library for examples
- [ ] Unsymmetric test matrices
- [ ] Indefinite test matrices
- [ ] Real-world matrices from SuiteSparse collection
- [ ] Complex-valued matrices
