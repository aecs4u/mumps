#!/usr/bin/env bash
#
# Start the MUMPS benchmark results web interface
#
# Usage:
#   ./scripts/start_webapp.sh [--port PORT] [--host HOST] [--reload]
#
# Environment variables:
#   WEBAPP_PORT  - Port to run on (default: 9001)
#   WEBAPP_HOST  - Host to bind to (default: 127.0.0.1)
#   BLAS_BENCH_RESULTS_DIR - Results directory (default: benchmarks/results)
#   MUMPS_BENCH_DB - Database path (default: benchmarks/mumps_benchmarks.sqlite)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Check for Python and required packages
if ! command -v python3 &> /dev/null; then
    echo "Error: python3 not found" >&2
    exit 1
fi

# Check if FastAPI is installed
if ! python3 -c "import fastapi" 2>/dev/null; then
    echo "Error: FastAPI not installed. Install with:" >&2
    echo "  pip install fastapi uvicorn" >&2
    exit 1
fi

# Run the webapp
exec python3 -m webapp.main "$@"
