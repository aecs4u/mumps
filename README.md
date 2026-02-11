# MUMPS 5.8.2 - Sparse Direct Solver

**MUltifrontal Massively Parallel sparse direct Solver**

MUMPS solves large sparse systems of linear equations **A x = b** using
Gaussian elimination with multifrontal techniques. It supports all four
arithmetic precisions (single/double real, single/double complex) and
runs on sequential, multithreaded, or distributed-memory (MPI) architectures.

This enhanced distribution adds a **Python/ctypes interface**, a **Typer CLI**,
a **FastAPI webapp dashboard**, and a comprehensive **benchmarking suite** with
pre-generated test matrices.

> Upstream: [mumps-solver.org](http://mumps-solver.org/) |
> License: [CeCILL-C](docs/CeCILL-C_V1-en.txt)

---

## Quick Start

```bash
# 1. Build all precisions (shared libraries)
make allshared

# 2. Run the benchmark with a test matrix
LD_LIBRARY_PATH=lib:src/PORD/lib \
  examples/dmumps_bench --matrix-file benchmarks/matrices/test_large_100x100_laplacian.coo

# 3. Install Python interface & CLI
pip install -e ".[cli,test]"

# 4. Use the CLI
mumps build all --shared
mumps test pytest
mumps vendor detect
mumps benchmark sparse

# 5. Start the webapp dashboard
mumps webapp start --port 9002
```

---

## Directory Layout

```
MUMPS/
├── src/                 Fortran/C source (80 templates + 23 hand-written files)
│   ├── templates/       Template files (.F.in) for precision generation
│   ├── PORD/            PORD ordering package
│   ├── libseq/          Sequential MPI stub library
│   ├── MATLAB/          MATLAB interface
│   └── SCILAB/          SCILAB interface
├── include/             C/Fortran header files (xmumps_c.h, etc.)
├── lib/                 Built libraries (libxmumps.so, libmumps_common.so)
├── examples/            Example programs and benchmark drivers
├── python/              Python/ctypes interface (pymumps)
├── cli/                 Typer CLI (mumps command)
├── webapp/              FastAPI dashboard with Jinja2 templates
├── tests/               pytest test suite with SQLite result storage
├── scripts/             Build, benchmark, and vendor testing scripts
├── benchmarks/          Test matrices (.coo) and benchmark fixtures
├── docs/                User guide (PDF) and license texts
├── Make.inc/            Platform-specific Makefile configurations
└── cmake/               CMake build support
```

---

## Building

See [INSTALL](INSTALL) for detailed instructions. The basic workflow:

```bash
# Copy a platform config
cp Make.inc/Makefile.debian.SEQ Makefile.inc

# Edit Makefile.inc to set compiler flags, BLAS library, orderings, etc.
# Then build:
make allshared        # All 4 precisions, shared libraries
make dshared          # Double precision only
make all              # Static libraries
```

### BLAS Vendor Detection

The CLI can auto-detect available BLAS libraries:

```bash
mumps vendor detect           # Show all detected BLAS libraries
mumps vendor rebuild          # Build MUMPS against each vendor (skips existing)
mumps vendor rebuild --force  # Force rebuild all vendors
```

Detection priority: **pkg-config** > **ldconfig** > **environment** (`$MKLROOT`) > **filesystem scan**.

---

## Python Interface

The `pymumps` package provides a ctypes wrapper for all 4 MUMPS precisions:

```python
from pymumps import MumpsSolver

solver = MumpsSolver(precision='d', sym=0)
solver.set_matrix(n=1000, irn=rows, jcn=cols, a=values)
solver.set_rhs(rhs)
solver.solve()               # JOB=6: analysis + factorization + solve
x = solver.get_solution()
solver.finalize()
```

Install with: `pip install -e .` or `uv pip install -e .`

---

## CLI

The `mumps` command wraps all build, test, benchmark, and vendor operations:

| Command | Description |
|---------|-------------|
| `mumps build library -p d` | Build double precision library |
| `mumps build all --shared` | Build all 4 precisions (shared) |
| `mumps build examples -p d` | Build example programs |
| `mumps test pytest` | Run Python test suite |
| `mumps test vendors` | Test all BLAS vendors |
| `mumps test fortran` | Run Fortran example tests |
| `mumps benchmark sparse` | Sparse solver benchmark (JOB=6) |
| `mumps benchmark dense` | Dense GEMM benchmark |
| `mumps benchmark all` | Full suite with SQLite storage |
| `mumps vendor detect` | Detect BLAS libraries |
| `mumps vendor rebuild` | Build per-vendor libraries |
| `mumps webapp start` | Start dashboard at localhost:9002 |
| `mumps info system` | Show compilers, BLAS, memory |
| `mumps clean` | Clean build artifacts |

Install CLI dependencies: `pip install -e ".[cli]"`

---

## Benchmarks

The benchmark drivers (`examples/[sdcz]mumps_bench`) support both auto-generated
Laplacian matrices and external `.coo` matrix files:

```bash
# Auto-generated 220x220 Laplacian (default)
examples/dmumps_bench --ngrid 220 --nrhs 4

# Load from .coo file
examples/dmumps_bench --matrix-file benchmarks/matrices/test_large_100x100_laplacian.coo

# With symmetry flag (0=unsym, 1=SPD, 2=general sym)
examples/dmumps_bench --matrix-file matrix.coo --sym 1 --ordering 5
```

### Pre-generated Test Matrices

10 SPD matrices in MUMPS coordinate format (`.coo`, 1-based indexing):

| Category | Matrix | Dimension | Non-zeros |
|----------|--------|-----------|-----------|
| Small | `test_tiny_10x10_diagonal.coo` | 10 | 10 |
| Small | `test_small_10x10_laplacian.coo` | 100 | 460 |
| Small | `test_small_100_tridiagonal.coo` | 100 | 298 |
| Medium | `test_medium_50x50_laplacian.coo` | 2,500 | 12,300 |
| Medium | `test_medium_1000_tridiagonal.coo` | 1,000 | 2,998 |
| Medium | `test_medium_1000x1000_diagonal.coo` | 1,000 | 1,000 |
| Large | `test_large_100x100_laplacian.coo` | 10,000 | 49,600 |
| Large | `test_large_10000_tridiagonal.coo` | 10,000 | 29,998 |
| Huge | `test_huge_200x200_laplacian.coo` | 40,000 | 199,200 |
| Huge | `test_huge_50000_tridiagonal.coo` | 50,000 | 149,998 |

Generate custom matrices:
```bash
python3 scripts/generate_test_matrices.py --size all
python3 scripts/generate_test_matrices.py --type laplacian --ngrid 300
```

---

## Testing

```bash
# Python tests (pytest)
mumps test pytest
# or directly:
uv run pytest tests/ -v

# Multi-vendor testing (builds & tests against each BLAS)
./scripts/test_all_vendors.sh

# Compare vendor results
./scripts/compare_vendors.sh

# BLAS correctness checks
./scripts/check_blas_correctness.sh
```

Test results are stored in SQLite (`mumps.sqlite`) and viewable in the webapp.

---

## Webapp Dashboard

A FastAPI web interface for monitoring benchmarks, test results, and system information:

```bash
mumps webapp start --port 9002 --reload
# Visit http://localhost:9002
```

Pages: Dashboard, Benchmarks (with test matrix catalog), Test Results,
System Info, CLI Reference, Documentation.

---

## Fortran Modernization

This distribution includes a fully modernized codebase:

- **103/103 source files** converted to free-form Fortran (100%)
- **80 template files** generating 320 precision-specific files via `scripts/generate_from_template.sh`
- **13 PORD C files** migrated to C23 (ANSI function prototypes)
- **Build system** with auto-detection of C23 (`-std=gnu23`) and Fortran 2023 (`-std=f2023`)

See [MODERNIZATION_STATUS.md](MODERNIZATION_STATUS.md) for full details.

---

## References

- MUMPS Users' Guide: [docs/userguide_5.8.2.pdf](docs/userguide_5.8.2.pdf)
- MUMPS website: [mumps-solver.org](http://mumps-solver.org/)
- License: [CeCILL-C v1](docs/CeCILL-C_V1-en.txt)

Copyright 1991-2026 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria,
Mumps Technologies, University of Bordeaux.
