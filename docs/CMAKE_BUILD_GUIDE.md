# MUMPS CMake Build System

Modern CMake-based build system for MUMPS with multi-precision and multi-vendor BLAS support.

## Quick Start

```bash
# Configure
cmake -B build \
    -DBLAS_VENDOR=openblas \
    -DPRECISIONS="s;d;c;z" \
    -DCMAKE_BUILD_TYPE=Release

# Build
cmake --build build -j$(nproc)

# Test
cd build && ctest

# Install
sudo cmake --install build
```

---

## Configuration Options

### Precisions

Build specific arithmetic precisions (default: `d`):

```bash
-DPRECISIONS="s;d;c;z"  # All precisions
-DPRECISIONS="d"        # Double precision only
-DPRECISIONS="s;d"      # Single and double
```

| Precision | Type | Description |
|-----------|------|-------------|
| `s` | `REAL` | Single precision (float) |
| `d` | `DOUBLE PRECISION` | Double precision (double) |
| `c` | `COMPLEX` | Complex single |
| `z` | `COMPLEX*16` | Complex double |

### BLAS Vendor

Choose BLAS implementation (default: `openblas`):

```bash
-DBLAS_VENDOR=openblas   # OpenBLAS
-DBLAS_VENDOR=mkl        # Intel MKL
-DBLAS_VENDOR=blis       # AMD BLIS
-DBLAS_VENDOR=reference  # Reference BLAS
-DBLAS_VENDOR=pkgconfig  # Auto-detect via pkg-config
```

### Build Type

```bash
-DCMAKE_BUILD_TYPE=Release  # Optimized build
-DCMAKE_BUILD_TYPE=Debug    # Debug symbols
-DCMAKE_BUILD_TYPE=RelWithDebInfo  # Optimized + debug info
```

### Library Type

```bash
-DBUILD_SHARED_LIBS=ON   # Build .so files
-DBUILD_SHARED_LIBS=OFF  # Build .a files (default)
```

### Optional Components

```bash
-DBUILD_EXAMPLES=ON      # Build example programs (default: ON)
-DBUILD_BENCHMARKS=ON    # Build benchmark programs (default: ON)
-DUSE_TEMPLATES=ON       # Generate from templates (default: ON)
```

### Ordering Libraries

```bash
-DUSE_METIS=ON    # Enable METIS ordering (default: ON)
-DUSE_SCOTCH=ON   # Enable SCOTCH ordering (default: ON)
-DUSE_PORD=ON     # Enable PORD ordering (default: ON)
```

---

## Build Examples

### Minimal Build (Double Precision, OpenBLAS)

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j$(nproc)
```

### Full Build (All Precisions, MKL)

```bash
cmake -B build \
    -DBLAS_VENDOR=mkl \
    -DPRECISIONS="s;d;c;z" \
    -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_BUILD_TYPE=Release
cmake --build build -j$(nproc)
```

### BLIS Build with Benchmarks

```bash
cmake -B build \
    -DBLAS_VENDOR=blis \
    -DPRECISIONS="s;d;c;z" \
    -DBUILD_BENCHMARKS=ON \
    -DCMAKE_BUILD_TYPE=Release
cmake --build build -j$(nproc)
```

### Development Build

```bash
cmake -B build \
    -DCMAKE_BUILD_TYPE=Debug \
    -DBUILD_EXAMPLES=ON \
    -DBUILD_BENCHMARKS=OFF
cmake --build build -j$(nproc)
```

---

## Vendor-Specific Builds

### Build for Multiple Vendors

```bash
# OpenBLAS
cmake -B build-openblas -DBLAS_VENDOR=openblas
cmake --build build-openblas -j$(nproc)
cmake --install build-openblas --prefix install/openblas

# MKL
cmake -B build-mkl -DBLAS_VENDOR=mkl
cmake --build build-mkl -j$(nproc)
cmake --install build-mkl --prefix install/mkl

# BLIS
cmake -B build-blis -DBLAS_VENDOR=blis
cmake --build build-blis -j$(nproc)
cmake --install build-blis --prefix install/blis
```

---

## Integration with Other Projects

### Using CMake's find_package

```cmake
# In your CMakeLists.txt
find_package(MUMPS 5.8 REQUIRED)

add_executable(myapp main.c)
target_link_libraries(myapp MUMPS::dmumps MUMPS::mumps_common)
```

### Using pkg-config

```bash
pkg-config --cflags --libs mumps-d
```

### Using CMake's FetchContent

```cmake
include(FetchContent)

FetchContent_Declare(
    MUMPS
    GIT_REPOSITORY https://github.com/mumps/mumps.git
    GIT_TAG v5.8.2
)

set(PRECISIONS "d" CACHE STRING "" FORCE)
set(BLAS_VENDOR "openblas" CACHE STRING "" FORCE)

FetchContent_MakeAvailable(MUMPS)

target_link_libraries(myapp MUMPS::dmumps)
```

---

## Build Targets

CMake creates the following targets:

### Library Targets

- `mumps_common` - Common MUMPS library (all precisions)
- `mpiseq` - Sequential MPI stub library
- `smumps` - Single precision MUMPS
- `dmumps` - Double precision MUMPS
- `cmumps` - Complex single precision MUMPS
- `zmumps` - Complex double precision MUMPS
- `pord` - PORD ordering library (if enabled)

### Executable Targets

- `ssimpletest`, `dsimpletest`, `csimpletest`, `zsimpletest` - Simple test programs
- `c_example` - C API example
- `smumps_bench`, `dmumps_bench`, `cmumps_bench`, `zmumps_bench` - MUMPS benchmarks
- `sgemm_bench`, `dgemm_bench`, `cgemm_bench`, `zgemm_bench` - GEMM benchmarks

### Utility Targets

- `generate_templates` - Generate precision-specific files from templates

---

## Installation

### System-Wide Installation

```bash
cmake -B build -DCMAKE_INSTALL_PREFIX=/usr/local
cmake --build build -j$(nproc)
sudo cmake --install build
```

### User Installation

```bash
cmake -B build -DCMAKE_INSTALL_PREFIX=$HOME/.local
cmake --build build -j$(nproc)
cmake --install build
```

### Custom Installation

```bash
cmake -B build -DCMAKE_INSTALL_PREFIX=/opt/mumps
cmake --build build -j$(nproc)
cmake --install build
```

### What Gets Installed

```
${CMAKE_INSTALL_PREFIX}/
├── lib/
│   ├── libmumps_common.a
│   ├── libsmumps.a
│   ├── libdmumps.a
│   ├── libcmumps.a
│   ├── libzmumps.a
│   ├── libmpiseq.a
│   ├── libpord.a
│   ├── pkgconfig/
│   │   ├── mumps-s.pc
│   │   ├── mumps-d.pc
│   │   ├── mumps-c.pc
│   │   └── mumps-z.pc
│   └── cmake/MUMPS/
│       ├── MUMPSConfig.cmake
│       ├── MUMPSConfigVersion.cmake
│       └── MUMPSTargets.cmake
├── include/mumps/
│   ├── cmumps_c.h
│   ├── dmumps_c.h
│   ├── smumps_c.h
│   ├── zmumps_c.h
│   └── pord/
└── bin/
    ├── dsimpletest
    ├── dmumps_bench
    └── dgemm_bench
```

---

## Template System Integration

The CMake build automatically generates precision-specific files from templates if `USE_TEMPLATES=ON` (default).

### How It Works

1. **Template files** in `src/templates/*.in` use placeholders:
   ```fortran
   SUBROUTINE @MUMPS_PREFIX@MUMPS_FUNC()
   @MUMPS_TYPE@ :: VAR
   ```

2. **CMake generates** 4 files (s/d/c/z) before compilation:
   ```bash
   src/smumps_func.f90 → SUBROUTINE SMUMPS_FUNC()
   src/dmumps_func.f90 → SUBROUTINE DMUMPS_FUNC()
   ```

3. **Dependencies** are tracked automatically - templates trigger regeneration

### Manual Template Generation

```bash
cmake --build build --target generate_templates
```

### Disable Template Generation

```bash
cmake -B build -DUSE_TEMPLATES=OFF
```

---

## Environment Variables

CMake respects these environment variables:

### Compilers

```bash
export CC=gcc-11
export FC=gfortran-11
cmake -B build
```

or

```bash
cmake -B build -DCMAKE_C_COMPILER=gcc-11 -DCMAKE_Fortran_COMPILER=gfortran-11
```

### BLAS/LAPACK Libraries

For custom BLAS locations:

```bash
export CMAKE_PREFIX_PATH="/opt/openblas:$CMAKE_PREFIX_PATH"
cmake -B build -DBLAS_VENDOR=openblas
```

### OpenMP Threads

```bash
export OMP_NUM_THREADS=16
./build/examples/dsimpletest
```

---

## Troubleshooting

### BLAS Not Found

```bash
# Install OpenBLAS
sudo apt-get install libopenblas-dev

# Or specify location
cmake -B build -DCMAKE_PREFIX_PATH=/path/to/openblas
```

### METIS Not Found

```bash
# Install METIS
sudo apt-get install libmetis-dev

# Or disable
cmake -B build -DUSE_METIS=OFF
```

### Fortran Module Errors

Clean build directory and rebuild:

```bash
rm -rf build
cmake -B build
cmake --build build
```

### Template Generation Fails

Disable template generation:

```bash
cmake -B build -DUSE_TEMPLATES=OFF
```

---

## Performance Tuning

### Optimization Flags

```bash
cmake -B build \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS_RELEASE="-O3 -march=native" \
    -DCMAKE_Fortran_FLAGS_RELEASE="-O3 -march=native"
```

### Thread Control

```bash
# Build time
cmake --build build -j16  # Use 16 cores

# Runtime
export OMP_NUM_THREADS=16
export OPENBLAS_NUM_THREADS=16
export MKL_NUM_THREADS=16
```

---

## Comparison: Makefile vs CMake

| Feature | Makefile | CMake |
|---------|----------|-------|
| **Configuration** | Manual editing | Command-line options |
| **Parallel builds** | `make -j8` | `cmake --build build -j` |
| **Dependencies** | Manual tracking | Automatic |
| **Cross-platform** | Linux only | Linux/macOS/Windows |
| **IDE support** | Limited | Native (CLion, VS Code, etc.) |
| **Template generation** | Manual script | Automatic |
| **Installation** | Manual cp | `cmake --install` |
| **pkg-config** | Manual generation | Automatic |
| **Find package** | Not supported | Native support |

---

## Migration from Makefile

### Old Workflow

```bash
# Makefile
cp Make.inc/Makefile.inc.generic Makefile.inc
vim Makefile.inc  # Edit manually
make d
make install
```

### New Workflow

```bash
# CMake
cmake -B build -DBLAS_VENDOR=openblas -DPRECISIONS="d"
cmake --build build -j
cmake --install build
```

### Equivalent Commands

| Makefile | CMake |
|----------|-------|
| `make d` | `cmake --build build --target dmumps` |
| `make all` | `cmake --build build` |
| `make clean` | `cmake --build build --target clean` |
| `make install` | `cmake --install build` |
| `BLAS_VENDOR=openblas make d` | `cmake -B build -DBLAS_VENDOR=openblas` |

---

## Advanced Usage

### Out-of-Source Builds

```bash
# Multiple configurations
cmake -B build-debug -DCMAKE_BUILD_TYPE=Debug
cmake -B build-release -DCMAKE_BUILD_TYPE=Release
cmake -B build-asan -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_FLAGS="-fsanitize=address"

# Build each independently
cmake --build build-debug
cmake --build build-release
```

### Cross-Compilation

```bash
cmake -B build-arm \
    -DCMAKE_TOOLCHAIN_FILE=arm-linux-gnueabihf.cmake \
    -DPRECISIONS="d"
```

### Static Analysis

```bash
cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
clang-tidy -p build src/*.c
```

---

## CMake Cache

View all CMake configuration options:

```bash
cmake -B build -L
```

View advanced options:

```bash
cmake -B build -LA
```

Modify cached value:

```bash
cmake -B build -DBLAS_VENDOR=mkl
```

Reset cache:

```bash
rm -rf build/CMakeCache.txt
```

---

## Benefits of CMake Build System

1. **✅ Modern and Portable** - Works on Linux, macOS, Windows
2. **✅ Automatic Dependency Tracking** - Rebuilds only what changed
3. **✅ Native IDE Support** - CLion, VS Code, Visual Studio
4. **✅ Package Management** - find_package, FetchContent support
5. **✅ Template Integration** - Automatic generation from templates
6. **✅ Easy Configuration** - No manual file editing
7. **✅ Better Error Messages** - Clear configuration errors
8. **✅ Installation Support** - Standard `cmake --install`
9. **✅ Testing Framework** - Integrated CTest support
10. **✅ Export/Import** - Other projects can easily use MUMPS

---

**Status**: ✅ CMake Build System Complete
**Version**: 1.0
**Date**: 2026-02-09
**Tested**: Ubuntu 22.04, gcc 11.4, gfortran 11.4
