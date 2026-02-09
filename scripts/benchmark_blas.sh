#!/usr/bin/env bash
set -euo pipefail

# Ensure consistent number formatting across locales
export LC_NUMERIC=C

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

PRECISIONS="${BENCH_PRECISIONS:-d}"
VENDORS="${BENCH_VENDORS:-openblas mkl blis pkgconfig reference}"
RUNS="${BENCH_RUNS:-3}"
NGRID="${BENCH_NGRID:-220}"
NRHS="${BENCH_NRHS:-4}"
MAKE_JOBS="${BENCH_MAKE_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)}"
THREADS="${BENCH_THREADS:-${OMP_NUM_THREADS:-1}}"
RESULTS_DIR="${BENCH_RESULTS_DIR:-benchmarks/results}"
RESULTS_BASENAME="${BENCH_RESULTS_BASENAME:-sparse_blas}"

export OMP_NUM_THREADS="$THREADS"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-$THREADS}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-$THREADS}"

if ! [[ "$RUNS" =~ ^[1-9][0-9]*$ ]]; then
  echo "BENCH_RUNS must be a positive integer (got: $RUNS)" >&2
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

min_from_list() {
  printf '%s\n' "$@" | sort -n | head -n 1
}

echo "BLAS benchmark configuration"
echo "  precisions: $PRECISIONS"
echo "  vendors: $VENDORS"
echo "  runs: $RUNS"
echo "  ngrid: $NGRID"
echo "  nrhs: $NRHS"
echo "  make jobs: $MAKE_JOBS"
echo "  threads: OMP=$OMP_NUM_THREADS OPENBLAS=$OPENBLAS_NUM_THREADS MKL=$MKL_NUM_THREADS"
echo

# Clean once at the beginning (different precisions build different libraries, so no need to clean between them)
if ! make clean >/dev/null 2>&1; then
  echo "WARNING: make clean failed"
fi

for precision in $PRECISIONS; do
  PRECISION_UPPER="${precision^^}"
  BENCH_BIN="${precision}mumps_bench"
  OUTPUT_PREFIX="${PRECISION_UPPER}MUMPS_BENCH_"

  TIMESTAMP_UTC="$(date -u +%Y%m%dT%H%M%SZ)"
  RESULTS_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${TIMESTAMP_UTC}.tsv"
  RESULTS_META="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${TIMESTAMP_UTC}.meta"
  LATEST_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_latest.tsv"
  LATEST_META="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_latest.meta"

  RESULTS_FILE="$(mktemp)"
  trap 'rm -f "$RESULTS_FILE"' EXIT

  echo "===> Precision: ${precision} (${PRECISION_UPPER}MUMPS)"
  echo

for vendor in $VENDORS; do
  build_log="$(mktemp)"
  run_log="$(mktemp)"
  times=()

  echo "==> Vendor: $vendor"

  if ! make -j"$MAKE_JOBS" BUILD=release BLAS_VENDOR="$vendor" "$precision" >/dev/null 2>"$build_log"; then
    echo "  skipped: build failed"
    echo "  build log: $build_log"
    rm -f "$run_log"
    continue
  fi
  rm -f "$build_log"

  if ! make -C examples BUILD=release BLAS_VENDOR="$vendor" "$BENCH_BIN" >/dev/null 2>"$run_log"; then
    echo "  skipped: benchmark driver build failed"
    echo "  build log: $run_log"
    continue
  fi

  for run_id in $(seq 1 "$RUNS"); do
    run_out="$(./examples/$BENCH_BIN --ngrid "$NGRID" --nrhs "$NRHS" 2>&1)"
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
  best="$(min_from_list "${times[@]}")"
  printf '%s\t%s\t%s\t%s\t%s\n' "$vendor" "$median" "$mean" "$best" "${times[*]}" >>"$RESULTS_FILE"
  echo "  summary: median=${median}s mean=${mean}s best=${best}s"
  echo
done

if [[ ! -s "$RESULTS_FILE" ]]; then
  echo "No vendor completed successfully."
  exit 1
fi

echo "Benchmark results (sorted by median time)"
printf "%-12s %-12s %-12s %-12s %s\n" "vendor" "median_s" "mean_s" "best_s" "runs_s"
printf "vendor\tmedian_s\tmean_s\tbest_s\truns_s\n" > "$RESULTS_TSV"
sort -t$'\t' -k2,2n "$RESULTS_FILE" | while IFS=$'\t' read -r vendor median mean best runs; do
  printf "%-12s %-12s %-12s %-12s %s\n" "$vendor" "$median" "$mean" "$best" "$runs"
  printf "%s\t%s\t%s\t%s\t%s\n" "$vendor" "$median" "$mean" "$best" "$runs" >> "$RESULTS_TSV"
done

best_vendor="$(sort -t$'\t' -k2,2n "$RESULTS_FILE" | head -n 1 | cut -f1)"
best_median="$(sort -t$'\t' -k2,2n "$RESULTS_FILE" | head -n 1 | cut -f2)"
echo
echo "Recommended BLAS_VENDOR=${best_vendor} (median ${best_median}s on this benchmark)."

{
  echo "type=sparse"
  echo "precision=${precision}"
  echo "generated_at_utc=${TIMESTAMP_UTC}"
  echo "vendors=${VENDORS}"
  echo "runs=${RUNS}"
  echo "ngrid=${NGRID}"
  echo "nrhs=${NRHS}"
  echo "threads=${THREADS}"
  echo "recommended_vendor=${best_vendor}"
  echo "recommended_value=${best_median}"
  echo "recommended_metric=median_s"
} > "$RESULTS_META"

cp "$RESULTS_TSV" "$LATEST_TSV"
cp "$RESULTS_META" "$LATEST_META"
echo "Saved sparse benchmark results to $LATEST_TSV"
echo

done  # End precision loop
