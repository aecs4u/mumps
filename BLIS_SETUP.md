# AMD BLIS Setup for MUMPS Benchmarks

## Overview

AMD BLIS (BLAS-Like Library Instantiation Software) is a high-performance BLAS implementation optimized for AMD processors. This guide explains how to install and use BLIS with MUMPS benchmarks.

## Installation Options

### Option 1: Install from AMD AOCL (Recommended for AMD CPUs)

```bash
# Download AMD AOCL (AMD Optimizing CPU Libraries)
# Visit: https://www.amd.com/en/developer/aocl.html

# After downloading, extract and set up:
tar -xzf aocl-*.tar.gz
cd aocl-*

# Add to your environment
export LD_LIBRARY_PATH=/path/to/aocl/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/path/to/aocl/lib/pkgconfig:$PKG_CONFIG_PATH
```

### Option 2: Build from Source

```bash
# Install dependencies
sudo apt-get install build-essential python3

# Clone BLIS repository
git clone https://github.com/amd/blis.git
cd blis

# Configure for your CPU (auto-detect)
./configure --enable-threading=openmp --enable-cblas auto

# Build and install
make -j$(nproc)
sudo make install

# Update library cache
sudo ldconfig

# Verify installation
pkg-config --modversion blis
```

### Option 3: Install via Package Manager (if available)

```bash
# Ubuntu/Debian (check if available in your repo)
sudo apt-get install libblis-dev

# Or try AOCL packages
sudo apt-get install aocl-blis-dev
```

## Testing BLIS with MUMPS

Once BLIS is installed, test the build:

```bash
# Build MUMPS with BLIS
make clean
make BLAS_VENDOR=blis d

# Run a simple test
./examples/dsimpletest < examples/input_simpletest_real

# Build benchmark
make -C examples BLAS_VENDOR=blis dmumps_bench

# Run benchmark
./examples/dmumps_bench --ngrid 100 --nrhs 4
```

## Running Benchmarks with BLIS

```bash
# Single precision benchmark with BLIS
BENCH_PRECISIONS="s d" BENCH_VENDORS="openblas blis" BENCH_RUNS=3 \
  ./scripts/benchmark_blas.sh

# Dense GEMM benchmark with BLIS
BENCH_PRECISIONS="d" BENCH_VENDORS="openblas blis" \
  ./scripts/benchmark_blas_dense.sh
```

## Performance Tuning

### Thread Control

BLIS respects OpenMP thread settings:

```bash
export OMP_NUM_THREADS=16
export BLIS_NUM_THREADS=16
```

### CPU Affinity

For best performance on AMD CPUs:

```bash
export GOMP_CPU_AFFINITY="0-15"
export OMP_PROC_BIND=close
export OMP_PLACES=cores
```

## Expected Performance

BLIS is typically optimized for AMD processors and may show:
- **AMD EPYC/Ryzen**: Often competitive with or faster than OpenBLAS
- **Intel CPUs**: OpenBLAS or MKL typically perform better
- **ARM CPUs**: OpenBLAS is usually the better choice

## Troubleshooting

### BLIS not detected

If MUMPS doesn't detect BLIS:

```bash
# Check if pkg-config finds BLIS
pkg-config --exists blis && echo "Found" || echo "Not found"

# Check library installation
ldconfig -p | grep blis

# Manually specify library path if needed
export BLIS_LIBBLAS="-L/path/to/blis/lib -lblis"
export BLIS_LAPACK="-llapack"
make BLAS_VENDOR=blis d
```

### Linking errors

If you see undefined reference errors:

```bash
# Ensure both BLIS and LAPACK are available
pkg-config --libs blis lapack

# For multithreaded BLIS (libblis-mt.so)
export BLIS_LIBBLAS="-L/usr/lib -lblis-mt -lm -lpthread"
```

## Verification

To verify BLIS is being used:

```bash
# Check linked libraries
ldd ./examples/dmumps_bench | grep blis

# Should show something like:
#   libblis.so.3 => /usr/lib/x86_64-linux-gnu/libblis.so.3
# or
#   libblis-mt.so => /usr/local/lib/libblis-mt.so
```

## Integration with Benchmark Dashboard

After running benchmarks with BLIS, ingest results:

```bash
python3 scripts/ingest_benchmark_results_sqlite.py --precisions "s d c z"
```

The web dashboard at `/` will show BLIS results alongside OpenBLAS, MKL, and other vendors, allowing direct performance comparison across:
- BLAS vendors (OpenBLAS, MKL, BLIS, reference)
- Precisions (single, double, complex single, complex double)
- Problem sizes and configurations
