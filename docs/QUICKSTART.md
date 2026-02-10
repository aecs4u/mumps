# MUMPS Quick Start Guide

## Choose Your Build System

MUMPS now supports two build systems. **CMake is recommended** for new projects.

---

## Option 1: CMake Build (Recommended)

### Why CMake?
- ✅ Modern, cross-platform
- ✅ One-line configuration
- ✅ Native IDE support
- ✅ Better error messages
- ✅ Automatic template generation

### Quick Build

```bash
# Configure (choose your BLAS)
cmake -B build -DBLAS_VENDOR=openblas -DPRECISIONS="s;d;c;z"

# Build (parallel)
cmake --build build -j$(nproc)

# Test
cd build && ctest

# Install
sudo cmake --install build
```

### Custom Configuration

```bash
cmake -B build \
    -DBLAS_VENDOR=blis \
    -DPRECISIONS="d" \
    -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=$HOME/.local

cmake --build build -j
cmake --install build
```

**Full Guide**: [CMAKE_BUILD_GUIDE.md](CMAKE_BUILD_GUIDE.md)

---

## Option 2: Makefile Build (Traditional)

### Why Makefile?
- ✅ Familiar to existing users
- ✅ No CMake dependency
- ✅ Works on legacy systems

### Standard Build

```bash
# Copy configuration template
cp Make.inc/Makefile.inc.generic Makefile.inc

# Edit Makefile.inc (set BLAS vendor, compilers, etc.)
vim Makefile.inc

# Build double precision
make d

# Run test
./examples/dsimpletest < examples/input_simpletest_real
```

### Smart Vendor Build

```bash
# One command for complete vendor build
make vendor-install VENDOR=blis PRECISIONS="s d c z"

# Libraries installed to: lib/vendors/blis/
ls lib/vendors/blis/
```

**Full Guide**: [INSTALL](INSTALL) (traditional) or [Makefile.vendor](Makefile.vendor) (new)

---

## BLAS Vendor Selection

Choose your BLAS implementation:

| Vendor | Best For | CMake Flag | Install |
|--------|----------|------------|---------|
| **OpenBLAS** | General purpose, open source | `-DBLAS_VENDOR=openblas` | `apt install libopenblas-dev` |
| **Intel MKL** | Intel CPUs (fastest) | `-DBLAS_VENDOR=mkl` | Download from Intel |
| **AMD BLIS** | AMD CPUs (optimized) | `-DBLAS_VENDOR=blis` | See [BLIS_SETUP.md](BLIS_SETUP.md) |
| **Reference** | Testing, debugging | `-DBLAS_VENDOR=reference` | `apt install libblas-dev` |

---

## Precision Selection

Build for specific arithmetic types:

| Precision | Type | CMake | Makefile |
|-----------|------|-------|----------|
| Single | `REAL` (float) | `s` | `make s` |
| Double | `DOUBLE PRECISION` | `d` | `make d` |
| Complex Single | `COMPLEX` | `c` | `make c` |
| Complex Double | `COMPLEX*16` | `z` | `make z` |
| All | All types | `s;d;c;z` | `make all` |

---

## Example Workflows

### Scientific Computing (Double Precision, OpenBLAS)

```bash
cmake -B build -DPRECISIONS="d" -DBLAS_VENDOR=openblas
cmake --build build -j
```

### Machine Learning (Mixed Precision, MKL)

```bash
cmake -B build -DPRECISIONS="s;d" -DBLAS_VENDOR=mkl
cmake --build build -j
```

### High-Performance Computing (All Precisions, BLIS)

```bash
cmake -B build -DPRECISIONS="s;d;c;z" -DBLAS_VENDOR=blis -DBUILD_SHARED_LIBS=ON
cmake --build build -j
```

### Development/Testing

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DPRECISIONS="d"
cmake --build build -j
```

---

## Benchmark Your BLAS

Compare BLAS vendor performance:

```bash
# Dense matrix benchmark
BENCH_PRECISIONS="d" BENCH_VENDORS="openblas mkl blis" \
  ./scripts/benchmark_blas_dense.sh

# Sparse MUMPS benchmark
BENCH_PRECISIONS="d" BENCH_VENDORS="openblas mkl blis" \
  ./scripts/benchmark_blas.sh

# View results in web dashboard
cd webapp && python3 -m uvicorn main:app
# Open http://localhost:8000
```

---

## Quick Tests

### Test Double Precision

```bash
# CMake
./build/examples/dsimpletest < examples/input_simpletest_real

# Makefile
./examples/dsimpletest < examples/input_simpletest_real
```

### Test All Precisions

```bash
for prec in s d c z; do
    echo "Testing ${prec}..."
    ./build/examples/${prec}simpletest < examples/input_simpletest_${prec} || echo "FAILED"
done
```

### Benchmark Performance

```bash
# MUMPS sparse solver benchmark
./build/examples/dmumps_bench --ngrid 100 --nrhs 4

# Dense GEMM benchmark
./build/examples/dgemm_bench --m 1000 --n 1000 --k 1000 --iters 10
```

---

## Troubleshooting

### CMake: BLAS not found

```bash
# Install OpenBLAS
sudo apt-get install libopenblas-dev

# Or specify custom location
cmake -B build -DCMAKE_PREFIX_PATH=/path/to/blas
```

### CMake: Fortran compiler error

```bash
# Install gfortran
sudo apt-get install gfortran

# Or specify compiler
cmake -B build -DCMAKE_Fortran_COMPILER=gfortran-11
```

### Makefile: Linking error

```bash
# Check BLAS installation
pkg-config --libs openblas

# Edit Makefile.inc and set correct LIBBLAS
vim Makefile.inc
```

### Runtime: Library not found

```bash
# Add library path
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Or use RPATH during build
cmake -B build -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON
```

---

## Getting Help

- **CMake Build**: See [CMAKE_BUILD_GUIDE.md](CMAKE_BUILD_GUIDE.md)
- **BLIS Setup**: See [BLIS_SETUP.md](BLIS_SETUP.md)
- **Template System**: See [PILOT_TEMPLATE_DEMO.md](PILOT_TEMPLATE_DEMO.md)
- **Code Analysis**: See [CODE_DEDUPLICATION_ANALYSIS.md](CODE_DEDUPLICATION_ANALYSIS.md)
- **Full Summary**: See [BUILD_SYSTEM_IMPROVEMENTS_SUMMARY.md](BUILD_SYSTEM_IMPROVEMENTS_SUMMARY.md)

---

## Comparison Chart

| Feature | CMake | Makefile |
|---------|-------|----------|
| **Setup** | One command | Edit Makefile.inc |
| **Configuration** | Command-line options | Manual editing |
| **Multi-precision** | `-DPRECISIONS="s;d;c;z"` | `make all` |
| **BLAS vendor** | `-DBLAS_VENDOR=openblas` | Edit LIBBLAS in Makefile.inc |
| **Parallel build** | Automatic | `make -j8` |
| **IDE support** | Native (CLion, VS Code) | Limited |
| **Template generation** | Automatic | Manual script |
| **Installation** | `cmake --install` | `make install` (limited) |
| **Package integration** | find_package(), FetchContent | Manual |
| **Error messages** | Clear, actionable | Cryptic linker errors |
| **Cross-platform** | Linux, macOS, Windows | Linux only |

---

## Recommendation

**For New Projects**: Use CMake
- Modern, maintainable, portable
- Better developer experience
- Future-proof

**For Existing Projects**: Either works
- Makefile still fully supported
- Can migrate gradually
- CMake and Makefile coexist

---

**Quick Start Complete!**

Next: Read [CMAKE_BUILD_GUIDE.md](CMAKE_BUILD_GUIDE.md) for advanced usage.
