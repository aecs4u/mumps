#!/usr/bin/env bash
set -euo pipefail

# Ensure consistent number formatting across locales
export LC_NUMERIC=C

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

PRECISIONS="${BENCH_PRECISIONS:-d}"
VENDORS="${BENCH_VENDORS:-openblas mkl pkgconfig reference}"
RUNS="${BENCH_RUNS:-3}"
THREADS="${BENCH_THREADS:-${OMP_NUM_THREADS:-1}}"
MAKE_JOBS="${BENCH_MAKE_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)}"
CASES_FILE="${BENCH_CASES_FILE:-benchmarks/blas_large_matrix_cases.tsv}"
ITERS_OVERRIDE="${BENCH_ITERS:-}"
MAX_GB="${BENCH_MAX_MATRIX_GB:-8}"
MAX_MEM_FRAC="${BENCH_MAX_MEM_FRACTION:-0.6}"
RESULTS_DIR="${BENCH_RESULTS_DIR:-benchmarks/results}"
RESULTS_BASENAME="${BENCH_RESULTS_BASENAME:-dense_blas}"

export OMP_NUM_THREADS="$THREADS"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-$THREADS}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-$THREADS}"

for value_name in RUNS MAKE_JOBS; do
  value="${!value_name}"
  if ! [[ "$value" =~ ^[1-9][0-9]*$ ]]; then
    echo "$value_name must be a positive integer (got: $value)" >&2
    exit 2
  fi
done

if ! [[ "$MAX_GB" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
  echo "BENCH_MAX_MATRIX_GB must be numeric (got: $MAX_GB)" >&2
  exit 2
fi

if ! [[ "$MAX_MEM_FRAC" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
  echo "BENCH_MAX_MEM_FRACTION must be numeric (got: $MAX_MEM_FRAC)" >&2
  exit 2
fi

if [[ -n "$ITERS_OVERRIDE" ]] && ! [[ "$ITERS_OVERRIDE" =~ ^[1-9][0-9]*$ ]]; then
  echo "BENCH_ITERS must be a positive integer (got: $ITERS_OVERRIDE)" >&2
  exit 2
fi

if [[ ! -f "$CASES_FILE" ]]; then
  echo "Cases file not found: $CASES_FILE" >&2
  exit 2
fi

mkdir -p "$RESULTS_DIR"
TIMESTAMP_UTC="$(date -u +%Y%m%dT%H%M%SZ)"
CASES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_cases_${TIMESTAMP_UTC}.tsv"
SCORES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_scores_${TIMESTAMP_UTC}.tsv"
RESULTS_META="$RESULTS_DIR/${RESULTS_BASENAME}_${TIMESTAMP_UTC}.meta"
LATEST_CASES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_cases_latest.tsv"
LATEST_SCORES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_scores_latest.tsv"
LATEST_META="$RESULTS_DIR/${RESULTS_BASENAME}_latest.meta"

median_from_list() {
  local values=("$@")
  local sorted count mid
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
    awk -v a="${sorted[$((mid-1))]}" -v b="${sorted[$mid]}" \
      'BEGIN { printf "%.6f\n", (a+b)/2.0 }'
  fi
}

mean_from_list() {
  printf '%s\n' "$@" | awk '{s+=$1} END { if (NR) printf "%.6f\n", s/NR; else print "nan" }'
}

geo_mean_from_list() {
  printf '%s\n' "$@" | awk '
    $1 > 0 { s += log($1); n += 1 }
    END {
      if (n == 0) print "nan";
      else printf "%.6f\n", exp(s/n);
    }'
}

mem_available_gb() {
  awk '/MemAvailable:/ { printf "%.3f\n", $2 / 1024.0 / 1024.0; found=1; exit }
       END { if (!found) print "0.000" }' /proc/meminfo
}

required_gb_for_case() {
  local m="$1" n="$2" k="$3"
  awk -v m="$m" -v n="$n" -v k="$k" 'BEGIN {
    elems = (m*k) + (k*n) + (m*n);
    bytes = elems * 8.0;
    overhead = 1.15;
    printf "%.3f\n", (bytes * overhead) / (1024.0*1024.0*1024.0);
  }'
}

AVAILABLE_GB="$(mem_available_gb)"
ALLOWED_GB="$(awk -v a="$AVAILABLE_GB" -v f="$MAX_MEM_FRAC" -v m="$MAX_GB" \
  'BEGIN { x = a*f; if (x < m) printf "%.3f\n", x; else printf "%.3f\n", m }')"

echo "Dense BLAS benchmark configuration"
echo "  precisions: $PRECISIONS"
echo "  vendors: $VENDORS"
echo "  runs: $RUNS"
echo "  cases file: $CASES_FILE"
echo "  threads: OMP=$OMP_NUM_THREADS OPENBLAS=$OPENBLAS_NUM_THREADS MKL=$MKL_NUM_THREADS"
echo "  memory available: ${AVAILABLE_GB} GB"
echo "  allowed per-case matrix footprint: ${ALLOWED_GB} GB"
echo

for precision in $PRECISIONS; do
  PRECISION_UPPER="${precision^^}"
  BENCH_BIN="${precision}gemm_bench"
  OUTPUT_PREFIX="${PRECISION_UPPER}GEMM_BENCH_"

  TIMESTAMP_UTC="$(date -u +%Y%m%dT%H%M%SZ)"
  CASES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_cases_${precision}_${TIMESTAMP_UTC}.tsv"
  SCORES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_scores_${precision}_${TIMESTAMP_UTC}.tsv"
  RESULTS_META="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_${TIMESTAMP_UTC}.meta"
  LATEST_CASES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_cases_${precision}_latest.tsv"
  LATEST_SCORES_TSV="$RESULTS_DIR/${RESULTS_BASENAME}_scores_${precision}_latest.tsv"
  LATEST_META="$RESULTS_DIR/${RESULTS_BASENAME}_${precision}_latest.meta"

  RESULTS="$(mktemp)"
  trap 'rm -f "$RESULTS" "${RESULTS}.score"' EXIT

  echo "===> Precision: ${precision} (${PRECISION_UPPER}GEMM)"
  echo

  # Clean once per precision (BLAS vendor only affects linking)
  if ! make -C examples clean >/dev/null 2>&1; then
    echo "  ERROR: failed to clean examples build artifacts for precision ${precision}"
    continue
  fi

for vendor in $VENDORS; do
  echo "==> Vendor: $vendor"

  if ! make -C examples -j"$MAKE_JOBS" BUILD=release BLAS_VENDOR="$vendor" "$BENCH_BIN" >/dev/null 2>&1; then
    echo "  skipped: failed to build examples/$BENCH_BIN"
    continue
  fi

  vendor_case_count=0
  vendor_case_gmeans=()

  while IFS=$'\t' read -r case_id m n k iters_default source description; do
    [[ -z "${case_id:-}" ]] && continue
    [[ "$case_id" =~ ^# ]] && continue

    iters="$iters_default"
    if [[ -n "$ITERS_OVERRIDE" ]]; then
      iters="$ITERS_OVERRIDE"
    fi

    req_gb="$(required_gb_for_case "$m" "$n" "$k")"
    skip_case="$(awk -v r="$req_gb" -v a="$ALLOWED_GB" 'BEGIN { if (r > a) print 1; else print 0 }')"
    if [[ "$skip_case" == "1" ]]; then
      echo "  case $case_id skipped (needs ${req_gb} GB, limit ${ALLOWED_GB} GB)"
      continue
    fi

    times=()
    gflops=()
    echo "  case $case_id (m=$m n=$n k=$k iters=$iters)"
    for run_id in $(seq 1 "$RUNS"); do
      out="$(./examples/$BENCH_BIN --m "$m" --n "$n" --k "$k" --iters "$iters" 2>&1 || true)"
      sec="$(printf '%s\n' "$out" | awk -F= "/^${OUTPUT_PREFIX}SECONDS=/{print \$2; exit}")"
      gflop="$(printf '%s\n' "$out" | awk -F= "/^${OUTPUT_PREFIX}GFLOPS=/{print \$2; exit}")"
      if [[ -z "$sec" || -z "$gflop" ]]; then
        echo "    run $run_id failed:"
        echo "$out"
        times=()
        gflops=()
        break
      fi
      times+=("$sec")
      gflops+=("$gflop")
      echo "    run $run_id: ${sec}s ${gflop} GFLOP/s"
    done

    if (( ${#times[@]} == 0 )); then
      echo "    case result: failed"
      continue
    fi

    med_sec="$(median_from_list "${times[@]}")"
    med_gflops="$(median_from_list "${gflops[@]}")"
    mean_gflops="$(mean_from_list "${gflops[@]}")"
    vendor_case_count=$((vendor_case_count + 1))
    vendor_case_gmeans+=("$med_gflops")

    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
      "$vendor" "$case_id" "$m" "$n" "$k" "$med_sec" "$med_gflops" "$mean_gflops" \
      >>"$RESULTS"
    echo "    case result: median ${med_sec}s, median ${med_gflops} GFLOP/s"
  done < "$CASES_FILE"

  if (( vendor_case_count == 0 )); then
    echo "  no runnable cases for $vendor"
    continue
  fi

  vendor_score="$(geo_mean_from_list "${vendor_case_gmeans[@]}")"
  printf '%s\t%s\n' "$vendor" "$vendor_score" >> "${RESULTS}.score"
  echo "  vendor score (geomean of per-case median GFLOP/s): $vendor_score"
  echo
done

if [[ ! -f "${RESULTS}.score" ]]; then
  echo "No vendor produced runnable results."
  exit 1
fi

printf "vendor\tcase_id\tm\tn\tk\tmed_sec\tmed_gflops\tmean_gflops\n" > "$CASES_TSV"
sort -t$'\t' -k1,1 -k2,2 "$RESULTS" >> "$CASES_TSV"
printf "vendor\tscore_geomean\n" > "$SCORES_TSV"
sort -t$'\t' -k2,2gr "${RESULTS}.score" >> "$SCORES_TSV"

echo "Per-case results"
printf "%-10s %-30s %-7s %-7s %-7s %-12s %-14s %-14s\n" \
  "vendor" "case_id" "m" "n" "k" "med_sec" "med_gflops" "mean_gflops"
sort -t$'\t' -k1,1 -k2,2 "$RESULTS" | while IFS=$'\t' read -r vendor case_id m n k med_sec med_gflops mean_gflops; do
  printf "%-10s %-30s %-7s %-7s %-7s %-12s %-14s %-14s\n" \
    "$vendor" "$case_id" "$m" "$n" "$k" "$med_sec" "$med_gflops" "$mean_gflops"
done

echo
echo "Vendor ranking (higher is better)"
printf "%-10s %-14s\n" "vendor" "score_geomean"
sort -t$'\t' -k2,2gr "${RESULTS}.score" | while IFS=$'\t' read -r vendor score; do
  printf "%-10s %-14s\n" "$vendor" "$score"
done

best_vendor="$(sort -t$'\t' -k2,2gr "${RESULTS}.score" | head -n 1 | cut -f1)"
best_score="$(sort -t$'\t' -k2,2gr "${RESULTS}.score" | head -n 1 | cut -f2)"
echo
echo "Recommended BLAS_VENDOR=${best_vendor} (score ${best_score})."

{
  echo "type=dense"
  echo "precision=${precision}"
  echo "generated_at_utc=${TIMESTAMP_UTC}"
  echo "vendors=${VENDORS}"
  echo "runs=${RUNS}"
  echo "cases_file=${CASES_FILE}"
  echo "max_matrix_gb=${MAX_GB}"
  echo "max_mem_fraction=${MAX_MEM_FRAC}"
  echo "available_gb=${AVAILABLE_GB}"
  echo "allowed_gb=${ALLOWED_GB}"
  echo "threads=${THREADS}"
  echo "recommended_vendor=${best_vendor}"
  echo "recommended_value=${best_score}"
  echo "recommended_metric=score_geomean"
} > "$RESULTS_META"

cp "$CASES_TSV" "$LATEST_CASES_TSV"
cp "$SCORES_TSV" "$LATEST_SCORES_TSV"
cp "$RESULTS_META" "$LATEST_META"
echo "Saved dense benchmark results to $LATEST_CASES_TSV and $LATEST_SCORES_TSV"
echo

done  # End precision loop
