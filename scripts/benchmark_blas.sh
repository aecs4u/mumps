#!/usr/bin/env bash
set -euo pipefail

# Ensure consistent number formatting across locales
export LC_NUMERIC=C

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"
export LD_LIBRARY_PATH="$ROOT_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# Helper function to add BLAS vendor library paths to LD_LIBRARY_PATH
add_vendor_lib_path() {
    local vendor="$1"
    local lib_path=""

    case "$vendor" in
        mkl)
            # Try to get MKL library path from pkg-config
            if command -v pkg-config >/dev/null 2>&1; then
                for pc in mkl-dynamic-lp64-seq mkl-dynamic-ilp64-seq mkl-sdl; do
                    if pkg-config --exists "$pc" 2>/dev/null; then
                        lib_path="$(pkg-config --variable=libdir "$pc" 2>/dev/null)"
                        break
                    fi
                done
            fi
            # Fallback: Try common MKL installation paths
            if [[ -z "$lib_path" ]]; then
                for mkl_root in "$MKLROOT" /opt/intel/oneapi/mkl/latest /opt/intel/mkl /usr/lib/x86_64-linux-gnu; do
                    if [[ -d "$mkl_root/lib" ]] && [[ -f "$mkl_root/lib/libmkl_rt.so" || -f "$mkl_root/lib/intel64/libmkl_rt.so" ]]; then
                        if [[ -d "$mkl_root/lib/intel64" ]]; then
                            lib_path="$mkl_root/lib/intel64"
                        else
                            lib_path="$mkl_root/lib"
                        fi
                        break
                    fi
                done
            fi
            ;;
        blis|amd-vitis)
            # Try to get BLIS library path from pkg-config
            if command -v pkg-config >/dev/null 2>&1; then
                for pc in blis-mt amdblis aocl-blis blis; do
                    if pkg-config --exists "$pc" 2>/dev/null; then
                        lib_path="$(pkg-config --variable=libdir "$pc" 2>/dev/null)"
                        break
                    fi
                done
            fi
            ;;
        openblas)
            # Try to get OpenBLAS library path from pkg-config
            if command -v pkg-config >/dev/null 2>&1 && pkg-config --exists openblas 2>/dev/null; then
                lib_path="$(pkg-config --variable=libdir openblas 2>/dev/null)"
            fi
            ;;
    esac

    # Resolve relative paths and add to LD_LIBRARY_PATH if found
    if [[ -n "$lib_path" ]]; then
        # Resolve to canonical path (handles ../ and double slashes)
        if [[ -d "$lib_path" ]]; then
            lib_path="$(cd "$lib_path" && pwd)"
            export LD_LIBRARY_PATH="$lib_path${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
            echo "  Added library path: $lib_path" >&2
        fi
    fi
}

PRECISIONS="${BENCH_PRECISIONS:-d}"
VENDORS="${BENCH_VENDORS:-openblas mkl blis pkgconfig reference}"
ORDERINGS="${BENCH_ORDERINGS:-7}"
RUNS="${BENCH_RUNS:-3}"
WARMUP_RUNS="${BENCH_WARMUP_RUNS:-1}"
SKIP_CLEAN="${BENCH_SKIP_CLEAN:-1}"
NGRID="${BENCH_NGRID:-220}"
NRHS="${BENCH_NRHS:-4}"
MAKE_JOBS="${BENCH_MAKE_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)}"
THREADS="${BENCH_THREADS:-${OMP_NUM_THREADS:-1}}"
RESULTS_DIR="${BENCH_RESULTS_DIR:-benchmarks/results}"
RESULTS_BASENAME="${BENCH_RESULTS_BASENAME:-sparse_blas}"

# Ordering method names (ICNTL(7) values)
declare -A ORDERING_NAMES
ORDERING_NAMES[0]="AMD"
ORDERING_NAMES[2]="AMF"
ORDERING_NAMES[3]="SCOTCH"
ORDERING_NAMES[4]="PORD"
ORDERING_NAMES[5]="METIS"
ORDERING_NAMES[6]="QAMD"
ORDERING_NAMES[7]="Auto"

export OMP_NUM_THREADS="$THREADS"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-$THREADS}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-$THREADS}"

if ! [[ "$RUNS" =~ ^[1-9][0-9]*$ ]]; then
  echo "BENCH_RUNS must be a positive integer (got: $RUNS)" >&2
  exit 2
fi

if ! [[ "$WARMUP_RUNS" =~ ^[0-9]+$ ]]; then
  echo "BENCH_WARMUP_RUNS must be a non-negative integer (got: $WARMUP_RUNS)" >&2
  exit 2
fi

if ! [[ "$SKIP_CLEAN" =~ ^[01]$ ]]; then
  echo "BENCH_SKIP_CLEAN must be 0 or 1 (got: $SKIP_CLEAN)" >&2
  exit 2
fi

if ! [[ "$NGRID" =~ ^[1-9][0-9]*$ ]]; then
  echo "BENCH_NGRID must be a positive integer (got: $NGRID)" >&2
  exit 2
fi

if ! [[ "$NRHS" =~ ^[1-9][0-9]*$ ]]; then
  echo "BENCH_NRHS must be a positive integer (got: $NRHS)" >&2
  exit 2
fi

if ! [[ "$MAKE_JOBS" =~ ^[1-9][0-9]*$ ]]; then
  echo "BENCH_MAKE_JOBS must be a positive integer (got: $MAKE_JOBS)" >&2
  exit 2
fi

mkdir -p "$RESULTS_DIR"
TIMESTAMP_UTC="$(date -u +%Y%m%dT%H%M%SZ)"
RESULTS_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_${TIMESTAMP_UTC}.tsv"
RESULTS_META="$RESULTS_DIR/${RESULTS_BASENAME}_${TIMESTAMP_UTC}.meta"
LATEST_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_latest.tsv"
LATEST_META="$RESULTS_DIR/${RESULTS_BASENAME}_latest.meta"

median_from_list() {
  local values=("$@")
  local sorted
  local count
  local mid
  mapfile -t sorted < <(printf '%s\n' "${values[@]}" | sort -n)
  count="${#sorted[@]}"
  if (( count == 0 )); then
    echo "nan"
    return
  fi
  if (( count % 2 == 1 )); then
    mid=$(( count / 2 ))
    printf '%.6f\n' "${sorted[$mid]}"
  else
    mid=$(( count / 2 ))
    awk -v a="${sorted[$((mid-1))]}" -v b="${sorted[$mid]}" 'BEGIN { printf "%.6f\n", (a+b)/2.0 }'
  fi
}

mean_from_list() {
  printf '%s\n' "$@" | awk '{s+=$1} END { if (NR) printf "%.6f\n", s/NR; else print "nan" }'
}

stddev_from_list() {
  printf '%s\n' "$@" | awk '
    { s+=$1; ss+=$1*$1; n+=1 }
    END {
      if (n <= 1) {
        print "0.000000";
      } else {
        v=(ss - (s*s)/n)/(n-1);
        if (v < 0) v=0;
        printf "%.6f\n", sqrt(v);
      }
    }'
}

ci95_halfwidth_from_list() {
  printf '%s\n' "$@" | awk '
    { s+=$1; ss+=$1*$1; n+=1 }
    END {
      if (n <= 1) {
        print "0.000000";
      } else {
        v=(ss - (s*s)/n)/(n-1);
        if (v < 0) v=0;
        sd=sqrt(v);
        printf "%.6f\n", 1.96*sd/sqrt(n);
      }
    }'
}

min_from_list() {
  printf '%s\n' "$@" | sort -n | head -n 1
}

cpu_model_name() {
  awk -F: '/model name/ {gsub(/^[ \t]+/, "", $2); print $2; exit}' /proc/cpuinfo 2>/dev/null || true
}

mem_total_gb() {
  awk '/MemTotal:/ { printf "%.2f", $2 / 1024.0 / 1024.0; exit }' /proc/meminfo 2>/dev/null || true
}

scaling_governor() {
  if [[ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]]; then
    cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor 2>/dev/null || true
  fi
}

HOSTNAME_STR="$(hostname 2>/dev/null || echo unknown)"
KERNEL_STR="$(uname -sr 2>/dev/null || echo unknown)"
CPU_MODEL_STR="$(cpu_model_name)"
LOGICAL_CPUS_STR="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo unknown)"
MEM_TOTAL_GB_STR="$(mem_total_gb)"
CPU_GOVERNOR_STR="$(scaling_governor)"

echo "BLAS benchmark configuration"
echo "  precisions: $PRECISIONS"
echo "  vendors: $VENDORS"
echo "  orderings: $ORDERINGS"
echo "  runs: $RUNS"
echo "  warmup runs: $WARMUP_RUNS"
echo "  skip clean: $SKIP_CLEAN"
echo "  ngrid: $NGRID"
echo "  nrhs: $NRHS"
echo "  make jobs: $MAKE_JOBS"
echo "  threads: OMP=$OMP_NUM_THREADS OPENBLAS=$OPENBLAS_NUM_THREADS MKL=$MKL_NUM_THREADS"
echo "  host: $HOSTNAME_STR | kernel: $KERNEL_STR | logical cpus: $LOGICAL_CPUS_STR"
if [[ -n "${CPU_MODEL_STR:-}" ]]; then
  echo "  cpu model: $CPU_MODEL_STR"
fi
if [[ -n "${MEM_TOTAL_GB_STR:-}" ]]; then
  echo "  memory: ${MEM_TOTAL_GB_STR} GB"
fi
if [[ -n "${CPU_GOVERNOR_STR:-}" ]]; then
  echo "  cpu governor: $CPU_GOVERNOR_STR"
fi
echo

# Clean once at the beginning (different precisions build different libraries, so no need to clean between them).
if [[ "$SKIP_CLEAN" == "0" ]]; then
  if ! make clean >/dev/null 2>&1; then
    echo "WARNING: make clean failed"
  fi
else
  echo "Skipping initial clean step (BENCH_SKIP_CLEAN=1)."
fi

for precision in $PRECISIONS; do
  PRECISION_UPPER="${precision^^}"
  BENCH_BIN="${precision}mumps_bench"
  OUTPUT_PREFIX="${PRECISION_UPPER}MUMPS_BENCH_"

  echo "===> Precision: ${precision} (${PRECISION_UPPER}MUMPS)"
  echo

for ordering in $ORDERINGS; do
  ORDERING_NAME="${ORDERING_NAMES[$ordering]}"

  echo "==> Ordering: ${ORDERING_NAME} (ICNTL(7)=${ordering})"
  echo

  TIMESTAMP_UTC="$(date -u +%Y%m%dT%H%M%SZ)"
  RESULTS_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${ORDERING_NAME}_${TIMESTAMP_UTC}.tsv"
  RESULTS_META="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${ORDERING_NAME}_${TIMESTAMP_UTC}.meta"
  LATEST_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${ORDERING_NAME}_latest.tsv"
  LATEST_META="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${ORDERING_NAME}_latest.meta"

  RESULTS_FILE="$(mktemp)"
  trap 'rm -f "$RESULTS_FILE"' EXIT

for vendor in $VENDORS; do
  build_log="$(mktemp)"
  run_log="$(mktemp)"
  times=()

  echo "==> Vendor: $vendor"

  # Add vendor-specific library paths to LD_LIBRARY_PATH
  add_vendor_lib_path "$vendor"

  # Use smart incremental build system (only rebuilds if config/sources changed)
  export BUILD=release
  export BLAS_VENDOR="$vendor"
  export ARITH="$precision"
  export MAKE_JOBS="$MAKE_JOBS"

  if ! "$ROOT_DIR/scripts/smart_build.sh" benchmarks "$precision" >/dev/null 2>"$build_log"; then
    echo "  skipped: build failed"
    echo "  build log: $build_log"
    rm -f "$run_log"
    continue
  fi
  rm -f "$build_log" "$run_log"

  if (( WARMUP_RUNS > 0 )); then
    warmup_failed=0
    for warmup_id in $(seq 1 "$WARMUP_RUNS"); do
      warmup_out="$(./examples/$BENCH_BIN --ngrid "$NGRID" --nrhs "$NRHS" --ordering "$ordering" 2>&1 || true)"
      warmup_sec="$(printf '%s\n' "$warmup_out" | awk -F= "/^${OUTPUT_PREFIX}JOB6_SECONDS=/{print \$2; exit}")"
      if [[ -z "$warmup_sec" ]]; then
        echo "  warmup $warmup_id failed:"
        echo "$warmup_out"
        warmup_failed=1
        break
      fi
      echo "  warmup $warmup_id: ${warmup_sec}s"
    done
    if (( warmup_failed == 1 )); then
      echo "  skipped: warmup failed"
      rm -f "$run_log"
      continue
    fi
  fi

  for run_id in $(seq 1 "$RUNS"); do
    run_out="$(./examples/$BENCH_BIN --ngrid "$NGRID" --nrhs "$NRHS" --ordering "$ordering" 2>&1)"
    sec="$(printf '%s\n' "$run_out" | awk -F= "/^${OUTPUT_PREFIX}JOB6_SECONDS=/{print \$2; exit}")"
    if [[ -z "$sec" ]]; then
      echo "  run $run_id failed:"
      echo "$run_out"
      times=()
      break
    fi
    times+=("$sec")
    echo "  run $run_id: ${sec}s"
  done

  rm -f "$run_log"

  if (( ${#times[@]} == 0 )); then
    echo "  skipped: no successful runs"
    continue
  fi

  median="$(median_from_list "${times[@]}")"
  mean="$(mean_from_list "${times[@]}")"
  stddev="$(stddev_from_list "${times[@]}")"
  ci95_halfwidth="$(ci95_halfwidth_from_list "${times[@]}")"
  best="$(min_from_list "${times[@]}")"
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$vendor" "$median" "$mean" "$stddev" "$ci95_halfwidth" "$best" "${times[*]}" \
    >>"$RESULTS_FILE"
  echo "  summary: median=${median}s mean=${mean}s stddev=${stddev}s ci95=±${ci95_halfwidth}s best=${best}s"
  echo
done

if [[ ! -s "$RESULTS_FILE" ]]; then
  echo "No vendor completed successfully."
  exit 1
fi

echo "Benchmark results (sorted by median time)"
printf "%-12s %-12s %-12s %-12s %-12s %-12s %s\n" \
  "vendor" "median_s" "mean_s" "stddev_s" "ci95_s" "best_s" "runs_s"
printf "vendor\tmedian_s\tmean_s\tstddev_s\tci95_s\tbest_s\truns_s\n" > "$RESULTS_TSV"
sort -t$'\t' -k2,2n "$RESULTS_FILE" | while IFS=$'\t' read -r vendor median mean stddev ci95_halfwidth best runs; do
  printf "%-12s %-12s %-12s %-12s %-12s %-12s %s\n" \
    "$vendor" "$median" "$mean" "$stddev" "$ci95_halfwidth" "$best" "$runs"
  printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
    "$vendor" "$median" "$mean" "$stddev" "$ci95_halfwidth" "$best" "$runs" \
    >> "$RESULTS_TSV"
done

best_vendor="$(sort -t$'\t' -k2,2n "$RESULTS_FILE" | head -n 1 | cut -f1)"
best_median="$(sort -t$'\t' -k2,2n "$RESULTS_FILE" | head -n 1 | cut -f2)"
echo
echo "Recommended BLAS_VENDOR=${best_vendor} (median ${best_median}s on this benchmark)."

{
  echo "type=sparse"
  echo "precision=${precision}"
  echo "ordering=${ordering}"
  echo "ordering_name=${ORDERING_NAME}"
  echo "generated_at_utc=${TIMESTAMP_UTC}"
  echo "vendors=${VENDORS}"
  echo "runs=${RUNS}"
  echo "warmup_runs=${WARMUP_RUNS}"
  echo "ngrid=${NGRID}"
  echo "nrhs=${NRHS}"
  echo "threads=${THREADS}"
  echo "hostname=${HOSTNAME_STR}"
  echo "kernel=${KERNEL_STR}"
  echo "logical_cpus=${LOGICAL_CPUS_STR}"
  if [[ -n "${CPU_MODEL_STR:-}" ]]; then
    echo "cpu_model=${CPU_MODEL_STR}"
  fi
  if [[ -n "${MEM_TOTAL_GB_STR:-}" ]]; then
    echo "mem_total_gb=${MEM_TOTAL_GB_STR}"
  fi
  if [[ -n "${CPU_GOVERNOR_STR:-}" ]]; then
    echo "cpu_governor=${CPU_GOVERNOR_STR}"
  fi
  echo "recommended_vendor=${best_vendor}"
  echo "recommended_value=${best_median}"
  echo "recommended_metric=median_s"
} > "$RESULTS_META"

cp "$RESULTS_TSV" "$LATEST_TSV"
cp "$RESULTS_META" "$LATEST_META"
echo "Saved sparse benchmark results to $LATEST_TSV"
echo

done  # End ordering loop
done  # End precision loop
