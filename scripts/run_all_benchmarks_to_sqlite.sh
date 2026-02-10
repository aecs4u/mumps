#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

RESULTS_DIR="${BENCH_RESULTS_DIR:-benchmarks/results}"
DB_PATH="${BLAS_BENCH_DB:-$RESULTS_DIR/blas_benchmarks.sqlite}"

echo "Running sparse benchmark..."
./scripts/benchmark_blas.sh

echo
echo "Running dense benchmark..."
./scripts/benchmark_blas_dense.sh

echo
echo "Ingesting benchmark results into SQLite..."
python3 ./scripts/ingest_benchmark_results_sqlite.py --results-dir "$RESULTS_DIR" --db "$DB_PATH"

echo
echo "Done. SQLite DB ready at: $DB_PATH"
