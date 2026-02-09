#!/usr/bin/env bash
set -euo pipefail

# Ensure consistent number formatting across locales.
export LC_NUMERIC=C

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"
export LD_LIBRARY_PATH="$ROOT_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

PRECISIONS="${CHECK_PRECISIONS:-s d c z}"
VENDORS="${CHECK_VENDORS:-openblas reference}"
NGRID="${CHECK_NGRID:-120}"
NRHS="${CHECK_NRHS:-2}"
MAKE_JOBS="${CHECK_MAKE_JOBS:-${BENCH_MAKE_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)}}"
THREADS="${CHECK_THREADS:-${BENCH_THREADS:-${OMP_NUM_THREADS:-1}}}"
MAX_RESIDUAL_RATIO="${CHECK_MAX_RESIDUAL_RATIO:-25}"

REL_TOL_S="${CHECK_REL_TOL_S:-1e-4}"
REL_TOL_D="${CHECK_REL_TOL_D:-1e-10}"
REL_TOL_C="${CHECK_REL_TOL_C:-1e-4}"
REL_TOL_Z="${CHECK_REL_TOL_Z:-1e-10}"

is_positive_int() {
  [[ "$1" =~ ^[1-9][0-9]*$ ]]
}

is_numeric() {
  [[ "$1" =~ ^[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?$ ]]
}

metric_from_output() {
  local output="$1"
  local key="$2"
  printf '%s\n' "$output" | awk -F= -v key="$key" '$1 == key { print $2; exit }'
}

if ! is_positive_int "$NGRID"; then
  echo "CHECK_NGRID must be a positive integer (got: $NGRID)" >&2
  exit 2
fi

if ! is_positive_int "$NRHS"; then
  echo "CHECK_NRHS must be a positive integer (got: $NRHS)" >&2
  exit 2
fi

if ! is_positive_int "$MAKE_JOBS"; then
  echo "CHECK_MAKE_JOBS/BENCH_MAKE_JOBS must be a positive integer (got: $MAKE_JOBS)" >&2
  exit 2
fi

if ! is_numeric "$MAX_RESIDUAL_RATIO"; then
  echo "CHECK_MAX_RESIDUAL_RATIO must be numeric (got: $MAX_RESIDUAL_RATIO)" >&2
  exit 2
fi

for tol_name in REL_TOL_S REL_TOL_D REL_TOL_C REL_TOL_Z; do
  tol_value="${!tol_name}"
  if ! is_numeric "$tol_value"; then
    echo "${tol_name} must be numeric (got: $tol_value)" >&2
    exit 2
  fi
done

export OMP_NUM_THREADS="$THREADS"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-$THREADS}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-$THREADS}"

echo "Sparse correctness regression configuration"
echo "  precisions: $PRECISIONS"
echo "  vendors: $VENDORS"
echo "  ngrid: $NGRID"
echo "  nrhs: $NRHS"
echo "  make jobs: $MAKE_JOBS"
echo "  threads: OMP=$OMP_NUM_THREADS OPENBLAS=$OPENBLAS_NUM_THREADS MKL=$MKL_NUM_THREADS"
echo "  tolerances: s=$REL_TOL_S d=$REL_TOL_D c=$REL_TOL_C z=$REL_TOL_Z"
echo "  max residual ratio across vendors: $MAX_RESIDUAL_RATIO"
echo

total_failures=0

for precision in $PRECISIONS; do
  prefix="${precision^^}MUMPS_BENCH_"
  bench_bin="${precision}mumps_bench"
  tol_name="REL_TOL_${precision^^}"
  tol="${!tol_name:-}"

  if [[ -z "$tol" ]]; then
    echo "Unsupported precision: $precision" >&2
    total_failures=$((total_failures + 1))
    continue
  fi

  best_residual=""
  worst_residual=""
  valid_vendor_count=0

  echo "===> Precision: $precision (tol=$tol)"

  for vendor in $VENDORS; do
    echo "==> Vendor: $vendor"

    if ! make -j"$MAKE_JOBS" BUILD=release BLAS_VENDOR="$vendor" "$precision" >/dev/null 2>&1; then
      echo "  FAIL: build failed for vendor=$vendor precision=$precision"
      total_failures=$((total_failures + 1))
      continue
    fi

    if ! make -C examples BUILD=release BLAS_VENDOR="$vendor" "$bench_bin" >/dev/null 2>&1; then
      echo "  FAIL: benchmark driver build failed for vendor=$vendor precision=$precision"
      total_failures=$((total_failures + 1))
      continue
    fi

    run_out="$(./examples/$bench_bin --ngrid "$NGRID" --nrhs "$NRHS" 2>&1 || true)"
    sec="$(metric_from_output "$run_out" "${prefix}JOB6_SECONDS")"
    infog1="$(metric_from_output "$run_out" "${prefix}INFOG1")"
    infog2="$(metric_from_output "$run_out" "${prefix}INFOG2")"
    residual="$(metric_from_output "$run_out" "${prefix}REL_RESIDUAL_INF")"

    if [[ -z "$sec" || -z "$infog1" || -z "$infog2" || -z "$residual" ]]; then
      echo "  FAIL: missing benchmark metrics in output"
      echo "$run_out"
      total_failures=$((total_failures + 1))
      continue
    fi

    if ! [[ "$infog1" =~ ^-?[0-9]+$ ]]; then
      echo "  FAIL: INFOG1 is not an integer: $infog1"
      total_failures=$((total_failures + 1))
      continue
    fi

    if (( infog1 < 0 )); then
      echo "  FAIL: solver returned error INFOG1=$infog1 INFOG2=$infog2"
      total_failures=$((total_failures + 1))
      continue
    fi

    residual_ok="$(awk -v r="$residual" -v t="$tol" 'BEGIN { print (r <= t) ? 1 : 0 }')"
    if [[ "$residual_ok" != "1" ]]; then
      echo "  FAIL: residual $residual exceeds tolerance $tol"
      total_failures=$((total_failures + 1))
      continue
    fi

    echo "  PASS: INFOG1=$infog1 INFOG2=$infog2 JOB6=${sec}s REL_RESIDUAL_INF=$residual"
    valid_vendor_count=$((valid_vendor_count + 1))

    if [[ -z "$best_residual" ]]; then
      best_residual="$residual"
      worst_residual="$residual"
    else
      best_residual="$(awk -v a="$best_residual" -v b="$residual" 'BEGIN { if (a < b) print a; else print b }')"
      worst_residual="$(awk -v a="$worst_residual" -v b="$residual" 'BEGIN { if (a > b) print a; else print b }')"
    fi
  done

  if (( valid_vendor_count >= 2 )); then
    ratio="$(awk -v lo="$best_residual" -v hi="$worst_residual" \
      'BEGIN { d=lo; if (d < 1e-30) d=1e-30; printf "%.6f", hi/d }')"
    ratio_ok="$(awk -v r="$ratio" -v maxr="$MAX_RESIDUAL_RATIO" 'BEGIN { print (r <= maxr) ? 1 : 0 }')"
    if [[ "$ratio_ok" != "1" ]]; then
      echo "  FAIL: residual spread too large across vendors (ratio=$ratio, max=$MAX_RESIDUAL_RATIO)"
      total_failures=$((total_failures + 1))
    else
      echo "  PASS: residual spread across vendors ratio=$ratio"
    fi
  fi

  echo
done

if (( total_failures > 0 )); then
  echo "Correctness regression FAILED with $total_failures issue(s)." >&2
  exit 1
fi

echo "Correctness regression PASSED."
