from __future__ import annotations

import csv
import html
import logging
import os
import sqlite3
import subprocess
import threading
import time
from pathlib import Path
from typing import Any

from fastapi import FastAPI, Request
from fastapi.concurrency import run_in_threadpool
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

PROJECT_ROOT = Path(__file__).resolve().parents[1]
RESULTS_DIR = Path(os.getenv("BLAS_BENCH_RESULTS_DIR", PROJECT_ROOT / "benchmarks" / "results"))
# Database path: prefer new location, fallback to old for backward compatibility
DB_PATH = Path(os.getenv("MUMPS_BENCH_DB", os.getenv("BLAS_BENCH_DB", PROJECT_ROOT / "benchmarks" / "mumps_benchmarks.sqlite")))
TEMPLATES_DIR = Path(__file__).resolve().parent / "templates"
STATIC_DIR = Path(__file__).resolve().parent / "static"
NOT_FOUND_TEMPLATE = "404.html"
MIGRATION_CACHE_TTL_SECONDS = max(1, int(os.getenv("MUMPS_MIGRATION_CACHE_TTL", "30")))
LOGGER = logging.getLogger(__name__)
_MIGRATION_CACHE_LOCK = threading.Lock()
_MIGRATION_CACHE: dict[str, Any] = {"data": None, "expires_at": 0.0, "last_error": None, "last_error_at": 0.0}


def _read_meta(path: Path) -> dict[str, str]:
    meta: dict[str, str] = {}
    if not path.exists():
        return meta
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        meta[key.strip()] = value.strip()
    return meta


def _read_tsv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f, delimiter="\t")
        return list(reader)


def _to_float(value: str | None) -> float | None:
    if value is None:
        return None
    try:
        return float(value)
    except ValueError:
        return None


def _parse_vendors_field(vendors: str | None) -> list[str]:
    if not vendors:
        return []
    return [v for v in vendors.split() if v]


def _count_lines(path: Path) -> int:
    if not path.exists():
        return 0
    try:
        return len(path.read_text(encoding="utf-8").splitlines())
    except (OSError, UnicodeDecodeError):
        return 0


def _render_not_found(request: Request, error: str, benchmark_code: str) -> HTMLResponse:
    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "error": error,
        "benchmark_code": benchmark_code,
    }
    if (TEMPLATES_DIR / NOT_FOUND_TEMPLATE).exists():
        return templates.TemplateResponse(NOT_FOUND_TEMPLATE, context, status_code=404)

    escaped_error = html.escape(error)
    escaped_code = html.escape(benchmark_code)
    body = (
        "<!doctype html><html><head><title>Not Found</title></head><body>"
        f"<h1>{escaped_error}</h1><p>Requested benchmark: <code>{escaped_code}</code></p>"
        '<p><a href="/benchmarks">Back to benchmarks</a></p></body></html>'
    )
    return HTMLResponse(content=body, status_code=404)


def _latest_run(conn: sqlite3.Connection, benchmark_type: str) -> sqlite3.Row | None:
    return conn.execute(
        """
        SELECT id, generated_at_utc, recommended_vendor, recommended_value, recommended_metric
        FROM benchmark_runs
        WHERE benchmark_type = ?
        ORDER BY generated_at_utc DESC, id DESC
        LIMIT 1
        """,
        (benchmark_type,),
    ).fetchone()


def _run_meta(conn: sqlite3.Connection, run_id: int) -> dict[str, str]:
    rows = conn.execute(
        "SELECT key, value FROM benchmark_meta WHERE run_id = ?",
        (run_id,),
    ).fetchall()
    meta = {str(row["key"]): str(row["value"]) for row in rows}
    return meta


def load_sparse_results_from_db(precision: str | None = None, ordering: str | None = None) -> dict[str, Any]:
    with sqlite3.connect(DB_PATH) as conn:
        conn.row_factory = sqlite3.Row
        run = _latest_run(conn, "sparse")
        if run is None:
            return {"rows": [], "meta": {}, "precisions": [], "orderings": []}

        # Get available precisions for this run
        precisions_query = """
            SELECT DISTINCT precision
            FROM sparse_results
            WHERE run_id = ?
            ORDER BY precision
        """
        precisions = [r[0] for r in conn.execute(precisions_query, (run["id"],)).fetchall()]

        # Get available orderings for this run
        orderings_query = """
            SELECT DISTINCT ordering
            FROM sparse_results
            WHERE run_id = ?
            ORDER BY ordering
        """
        orderings = [r[0] for r in conn.execute(orderings_query, (run["id"],)).fetchall()]

        # Query with optional precision and ordering filters
        query = """
            SELECT vendor, precision, ordering, ordering_id, median_s, mean_s, best_s, runs_s
            FROM sparse_results
            WHERE run_id = ?
        """
        params = [run["id"]]

        if precision:
            query += " AND precision = ?"
            params.append(precision)

        if ordering:
            query += " AND ordering = ?"
            params.append(ordering)

        query += " ORDER BY precision ASC, ordering ASC, median_s ASC"

        rows = [dict(row) for row in conn.execute(query, params).fetchall()]

        meta = _run_meta(conn, int(run["id"]))
        meta.setdefault("generated_at_utc", str(run["generated_at_utc"]))
        meta.setdefault("recommended_vendor", str(run["recommended_vendor"] or ""))
        meta.setdefault("recommended_value", str(run["recommended_value"] or ""))
        meta.setdefault("recommended_metric", str(run["recommended_metric"] or ""))
        requested = _parse_vendors_field(meta.get("vendors"))
        present = [str(r.get("vendor", "")) for r in rows]
        missing = [v for v in requested if v not in present]
        return {"rows": rows, "meta": meta, "missing_vendors": missing, "precisions": precisions, "orderings": orderings}


def load_dense_results_from_db(precision: str | None = None) -> dict[str, Any]:
    with sqlite3.connect(DB_PATH) as conn:
        conn.row_factory = sqlite3.Row
        run = _latest_run(conn, "dense")
        if run is None:
            return {"scores": [], "cases": [], "meta": {}, "precisions": []}

        # Get available precisions for this run
        precisions_query = """
            SELECT DISTINCT precision
            FROM dense_scores
            WHERE run_id = ?
            ORDER BY precision
        """
        precisions = [r[0] for r in conn.execute(precisions_query, (run["id"],)).fetchall()]

        # Query scores with optional precision filter
        scores_query = """
            SELECT vendor, precision, score_geomean
            FROM dense_scores
            WHERE run_id = ?
        """
        scores_params = [run["id"]]

        if precision:
            scores_query += " AND precision = ?"
            scores_params.append(precision)

        scores_query += " ORDER BY precision ASC, score_geomean DESC"

        scores = [dict(row) for row in conn.execute(scores_query, scores_params).fetchall()]

        # Query cases with optional precision filter
        cases_query = """
            SELECT vendor, precision, case_id, m, n, k, med_sec, med_gflops, mean_gflops
            FROM dense_cases
            WHERE run_id = ?
        """
        cases_params = [run["id"]]

        if precision:
            cases_query += " AND precision = ?"
            cases_params.append(precision)

        cases_query += " ORDER BY precision ASC, case_id ASC, vendor ASC"

        cases = [dict(row) for row in conn.execute(cases_query, cases_params).fetchall()]

        meta = _run_meta(conn, int(run["id"]))
        meta.setdefault("generated_at_utc", str(run["generated_at_utc"]))
        meta.setdefault("recommended_vendor", str(run["recommended_vendor"] or ""))
        meta.setdefault("recommended_value", str(run["recommended_value"] or ""))
        meta.setdefault("recommended_metric", str(run["recommended_metric"] or ""))
        requested = _parse_vendors_field(meta.get("vendors"))
        present = [str(r.get("vendor", "")) for r in scores]
        missing = [v for v in requested if v not in present]
        return {"scores": scores, "cases": cases, "meta": meta, "missing_vendors": missing, "precisions": precisions}


def load_sparse_results(precision: str | None = None, ordering: str | None = None) -> dict[str, Any]:
    if DB_PATH.exists():
        try:
            return load_sparse_results_from_db(precision=precision, ordering=ordering)
        except sqlite3.DatabaseError:
            pass
    # TSV fallback - try ordering-specific file first
    prec = precision or 'd'
    ord = ordering or 'Auto'
    tsv_path = RESULTS_DIR / f"sparse_blas_{prec}_{ord}_latest.tsv"
    if not tsv_path.exists():
        tsv_path = RESULTS_DIR / f"sparse_blas_{prec}_latest.tsv"  # Fallback to old naming
    if not tsv_path.exists():
        tsv_path = RESULTS_DIR / "sparse_blas_latest.tsv"  # Ultimate fallback
    rows = _read_tsv(tsv_path) if tsv_path.exists() else []
    for row in rows:
        row["median_s_num"] = _to_float(row.get("median_s"))
        row["mean_s_num"] = _to_float(row.get("mean_s"))
        row["best_s_num"] = _to_float(row.get("best_s"))
        row["precision"] = prec
        row.setdefault("ordering", ord)
    rows.sort(key=lambda r: (r.get("median_s_num") is None, r.get("median_s_num")))
    meta_path = RESULTS_DIR / f"sparse_blas_{prec}_{ord}_latest.meta"
    if not meta_path.exists():
        meta_path = RESULTS_DIR / f"sparse_blas_{prec}_latest.meta"
    if not meta_path.exists():
        meta_path = RESULTS_DIR / "sparse_blas_latest.meta"
    meta = _read_meta(meta_path) if meta_path.exists() else {}
    requested = _parse_vendors_field(meta.get("vendors"))
    present = [str(r.get("vendor", "")) for r in rows]
    missing = [v for v in requested if v not in present]
    return {"rows": rows, "meta": meta, "missing_vendors": missing, "precisions": [prec], "orderings": [ord]}


def load_dense_results(precision: str | None = None) -> dict[str, Any]:
    if DB_PATH.exists():
        try:
            return load_dense_results_from_db(precision=precision)
        except sqlite3.DatabaseError:
            pass
    # TSV fallback - load precision-specific file or default to 'd'
    prec = precision or 'd'
    scores_path = RESULTS_DIR / f"dense_blas_scores_{prec}_latest.tsv"
    cases_path = RESULTS_DIR / f"dense_blas_cases_{prec}_latest.tsv"
    if not scores_path.exists():
        scores_path = RESULTS_DIR / "dense_blas_scores_latest.tsv"
        cases_path = RESULTS_DIR / "dense_blas_cases_latest.tsv"
    scores = _read_tsv(scores_path) if scores_path.exists() else []
    cases = _read_tsv(cases_path) if cases_path.exists() else []
    for row in scores:
        row["score_num"] = _to_float(row.get("score_geomean"))
        row["precision"] = prec
    scores.sort(key=lambda r: (r.get("score_num") is None, -(r.get("score_num") or 0.0)))
    for row in cases:
        row["med_sec_num"] = _to_float(row.get("med_sec"))
        row["med_gflops_num"] = _to_float(row.get("med_gflops"))
        row["precision"] = prec
    meta_path = RESULTS_DIR / f"dense_blas_{prec}_latest.meta"
    if not meta_path.exists():
        meta_path = RESULTS_DIR / "dense_blas_latest.meta"
    meta = _read_meta(meta_path) if meta_path.exists() else {}
    requested = _parse_vendors_field(meta.get("vendors"))
    present = [str(r.get("vendor", "")) for r in scores]
    missing = [v for v in requested if v not in present]
    return {"scores": scores, "cases": cases, "meta": meta, "missing_vendors": missing, "precisions": [prec]}


app = FastAPI(title="BLAS Benchmark Results")
templates = Jinja2Templates(directory=str(TEMPLATES_DIR))
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


@app.get("/", response_class=HTMLResponse)
async def index(request: Request) -> HTMLResponse:
    """Landing page with overview and navigation."""
    # Get quick stats from database if available
    benchmark_count = 0
    vendor_count = 0
    precision_count = 4
    ordering_count = 7

    if DB_PATH.exists():
        try:
            with sqlite3.connect(DB_PATH) as conn:
                conn.row_factory = sqlite3.Row
                # Count distinct vendors
                result = conn.execute("SELECT COUNT(DISTINCT vendor) FROM sparse_results").fetchone()
                if result:
                    vendor_count = result[0]
                # Count benchmark runs
                result = conn.execute("SELECT COUNT(*) FROM benchmark_runs").fetchone()
                if result:
                    benchmark_count = result[0]
        except sqlite3.DatabaseError:
            pass

    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "benchmark_count": benchmark_count if benchmark_count > 0 else None,
        "vendor_count": vendor_count if vendor_count > 0 else None,
        "precision_count": precision_count,
        "ordering_count": ordering_count,
    }
    return templates.TemplateResponse("index.html", context)


@app.get("/results", response_class=HTMLResponse)
async def results(request: Request, precision: str = "d", ordering: str = "Auto") -> HTMLResponse:
    """Benchmark results page (moved from old index)."""
    # Validate precision
    if precision not in ['s', 'd', 'c', 'z']:
        precision = 'd'

    # Validate ordering
    valid_orderings = ["AMD", "AMF", "SCOTCH", "PORD", "METIS", "QAMD", "Auto"]
    if ordering not in valid_orderings:
        ordering = "Auto"

    sparse = load_sparse_results(precision=precision, ordering=ordering)
    dense = load_dense_results(precision=precision)

    # Get all available precisions (union of sparse and dense)
    all_precisions = sorted(set(sparse.get("precisions", []) + dense.get("precisions", [])))

    # Get all available orderings from sparse results
    all_orderings = sparse.get("orderings", ["Auto"])

    precision_names = {
        's': 'Single (float)',
        'd': 'Double (double)',
        'c': 'Complex Single',
        'z': 'Complex Double',
    }

    ordering_descriptions = {
        'AMD': 'Approximate Minimum Degree',
        'AMF': 'Approximate Minimum Fill',
        'SCOTCH': 'SCOTCH partitioner',
        'PORD': 'PORD ordering',
        'METIS': 'METIS partitioner',
        'QAMD': 'QAMD with quasi-dense detection',
        'Auto': 'Automatic selection by MUMPS',
    }

    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "sparse": sparse,
        "dense": dense,
        "current_precision": precision,
        "current_ordering": ordering,
        "all_precisions": all_precisions if all_precisions else ['d'],
        "all_orderings": all_orderings if all_orderings else ['Auto'],
        "precision_names": precision_names,
        "ordering_descriptions": ordering_descriptions,
    }
    return templates.TemplateResponse("results.html", context)


@app.get("/api/results", response_class=JSONResponse)
async def api_results() -> JSONResponse:
    return JSONResponse(
        {
            "results_dir": str(RESULTS_DIR),
            "db_path": str(DB_PATH),
            "sparse": load_sparse_results(),
            "dense": load_dense_results(),
        }
    )


@app.get("/modules", response_class=HTMLResponse)
async def modules(request: Request) -> HTMLResponse:
    """Sparse matrix modules information page."""
    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
    }
    return templates.TemplateResponse("modules.html", context)


@app.get("/benchmarks", response_class=HTMLResponse)
async def benchmarks(request: Request) -> HTMLResponse:
    """List of available benchmarks."""
    # Get execution counts and recent runs from database
    sparse_execution_count = 0
    dense_execution_count = 0
    recent_runs = []

    if DB_PATH.exists():
        try:
            with sqlite3.connect(DB_PATH) as conn:
                conn.row_factory = sqlite3.Row
                # Count executions
                result = conn.execute("SELECT COUNT(*) FROM sparse_results").fetchone()
                if result:
                    sparse_execution_count = result[0]
                result = conn.execute("SELECT COUNT(*) FROM dense_scores").fetchone()
                if result:
                    dense_execution_count = result[0]

                # Get recent runs
                recent_query = """
                    SELECT
                        br.generated_at_utc as date,
                        'sparse-mumps-job6' as benchmark_code,
                        'Sparse MUMPS JOB=6' as benchmark_name,
                        sr.precision,
                        sr.vendor,
                        sr.ordering,
                        printf('%.6f s', sr.median_s) as result
                    FROM benchmark_runs br
                    JOIN sparse_results sr ON br.id = sr.run_id
                    WHERE br.benchmark_type = 'sparse'
                    ORDER BY br.generated_at_utc DESC
                    LIMIT 10
                """
                recent_runs = [dict(row) for row in conn.execute(recent_query).fetchall()]
        except sqlite3.DatabaseError:
            pass

    # Load test matrix fixtures from fixtures.json
    import json

    fixtures_path = PROJECT_ROOT / "benchmarks" / "matrices" / "fixtures.json"
    matrix_fixtures = []
    total_matrices = 0
    if fixtures_path.exists():
        try:
            with open(fixtures_path) as f:
                fixtures_data = json.load(f)
            matrix_fixtures = fixtures_data.get("fixtures", [])
            total_matrices = sum(len(cat.get("matrices", [])) for cat in matrix_fixtures)
        except (json.JSONDecodeError, OSError):
            pass

    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "sparse_execution_count": sparse_execution_count if sparse_execution_count > 0 else None,
        "dense_execution_count": dense_execution_count if dense_execution_count > 0 else None,
        "recent_runs": recent_runs,
        "matrix_fixtures": matrix_fixtures,
        "total_matrices": total_matrices,
    }
    return templates.TemplateResponse("benchmarks.html", context)


@app.get("/benchmarks/{benchmark_code}", response_class=HTMLResponse)
async def benchmark_detail(request: Request, benchmark_code: str) -> HTMLResponse:
    """Detailed view of a specific benchmark."""
    # Define benchmark metadata
    benchmarks_info = {
        "sparse-mumps-job6": {
            "name": "Sparse MUMPS JOB=6",
            "code": "sparse-mumps-job6",
            "category": "Sparse Solver",
            "description": "Complete MUMPS solve cycle: analysis + factorization + solve",
            "badges": [
                {"type": "sparse", "label": "Sparse"},
                {"type": "solver", "label": "Solver"},
            ],
            "config_html": """
                <ul>
                    <li><strong>Test Matrix:</strong> 2D Laplacian (5-point stencil)</li>
                    <li><strong>Grid Size:</strong> Configurable (default: 220×220 = 48,400 equations)</li>
                    <li><strong>Matrix Size:</strong> N×N sparse matrix</li>
                    <li><strong>Right-Hand Sides:</strong> Configurable (default: 4)</li>
                    <li><strong>MUMPS JOB:</strong> JOB=6 (all-in-one: analyze, factor, solve)</li>
                    <li><strong>Metrics:</strong> Total execution time in seconds</li>
                </ul>
            """,
            "run_command": "./scripts/benchmark_blas.sh",
            "run_help": "Runs sparse benchmark with default settings. Customize with environment variables (NGRID, NRHS, etc.)",
            "related": [
                {"code": "dense-gemm", "name": "Dense GEMM"},
                {"code": "correctness", "name": "Correctness Tests"},
            ],
            "docs": [
                {"title": "MUMPS User Guide", "url": "/doc/userguide_5.8.2.pdf"},
                {"title": "Benchmark Guide", "url": "/docs/INCREMENTAL_BUILD_GUIDE.md"},
            ],
        },
        "dense-gemm": {
            "name": "Dense GEMM",
            "code": "dense-gemm",
            "category": "Dense BLAS",
            "description": "General Matrix-Matrix Multiplication performance test",
            "badges": [
                {"type": "dense", "label": "Dense"},
                {"type": "blas", "label": "BLAS"},
            ],
            "config_html": """
                <ul>
                    <li><strong>Operation:</strong> C = α·A·B + β·C</li>
                    <li><strong>Matrix Sizes:</strong> 1000×1000 to 10000×10000</li>
                    <li><strong>Layouts:</strong> Row-major (C) and Column-major (Fortran)</li>
                    <li><strong>Iterations:</strong> Multiple runs for statistical accuracy</li>
                    <li><strong>Metrics:</strong> GFLOPS, execution time, geometric mean</li>
                </ul>
            """,
            "run_command": "./scripts/benchmark_blas_dense.sh",
            "run_help": "Runs dense GEMM benchmarks across multiple matrix sizes",
            "related": [
                {"code": "sparse-mumps-job6", "name": "Sparse MUMPS JOB=6"},
            ],
            "docs": [
                {"title": "BLAS Reference", "url": "http://www.netlib.org/blas/"},
            ],
        },
        "correctness": {
            "name": "Correctness Tests",
            "code": "correctness",
            "category": "Validation",
            "description": "BLAS implementation correctness verification",
            "badges": [
                {"type": "validation", "label": "Validation"},
            ],
            "config_html": """
                <ul>
                    <li><strong>Test Coverage:</strong> BLAS Level 1, 2, and 3 routines</li>
                    <li><strong>Validation Method:</strong> Residual checks against reference</li>
                    <li><strong>Tolerance:</strong> Machine epsilon with safety factor</li>
                    <li><strong>Test Cases:</strong> Various matrix sizes and data patterns</li>
                </ul>
            """,
            "run_command": "./scripts/check_blas_correctness.sh",
            "run_help": "Validates BLAS implementations for numerical correctness",
            "related": [],
            "docs": [],
        },
    }

    if benchmark_code not in benchmarks_info:
        return _render_not_found(request, "Benchmark not found", benchmark_code)

    benchmark = benchmarks_info[benchmark_code]

    # Load execution data from database
    executions = []
    statistics = []
    execution_data = None
    execution_count = 0
    latest_run = None
    precisions_tested = "N/A"
    vendors_tested = "N/A"
    best_performance = {"vendor": "N/A", "value": "N/A"}
    worst_performance = {"vendor": "N/A", "value": "N/A"}
    speedup = "N/A"

    if DB_PATH.exists() and benchmark_code == "sparse-mumps-job6":
        try:
            with sqlite3.connect(DB_PATH) as conn:
                conn.row_factory = sqlite3.Row
                # Get executions
                exec_query = """
                    SELECT
                        br.generated_at_utc as timestamp,
                        sr.precision,
                        sr.vendor,
                        sr.ordering,
                        printf('%.6f s', sr.median_s) as result,
                        'success' as status
                    FROM benchmark_runs br
                    JOIN sparse_results sr ON br.id = sr.run_id
                    WHERE br.benchmark_type = 'sparse'
                    ORDER BY br.generated_at_utc DESC
                    LIMIT 50
                """
                executions = [dict(row) for row in conn.execute(exec_query).fetchall()]
                execution_count = len(executions)

                if execution_count > 0:
                    latest_run = executions[0]["timestamp"]

                # Get statistics
                stats_query = """
                    SELECT
                        vendor,
                        printf('%.6f', AVG(median_s)) as mean,
                        printf('%.6f', median_s) as median,
                        printf('%.6f', MIN(median_s)) as min,
                        printf('%.6f', MAX(median_s)) as max,
                        COUNT(*) as sample_count,
                        'N/A' as std_dev
                    FROM sparse_results
                    GROUP BY vendor
                    ORDER BY AVG(median_s)
                """
                statistics = [dict(row) for row in conn.execute(stats_query).fetchall()]

                # Get performance data for chart
                perf_query = """
                    SELECT vendor, AVG(median_s) as avg_time
                    FROM sparse_results
                    GROUP BY vendor
                    ORDER BY avg_time
                """
                perf_data = conn.execute(perf_query).fetchall()
                if perf_data:
                    execution_data = {
                        "labels": [row[0] for row in perf_data],
                        "values": [row[1] for row in perf_data],
                        "metric": "Average Time",
                        "y_label": "Time (seconds)",
                    }
                    best_performance = {"vendor": perf_data[0][0], "value": f"{perf_data[0][1]:.6f} s"}
                    worst_performance = {"vendor": perf_data[-1][0], "value": f"{perf_data[-1][1]:.6f} s"}
                    # Guard against divide-by-zero
                    if perf_data[0][1] > 0:
                        speedup = f"{perf_data[-1][1] / perf_data[0][1]:.2f}"
                    else:
                        speedup = "N/A"

                # Get unique precisions and vendors
                precisions = conn.execute("SELECT DISTINCT precision FROM sparse_results ORDER BY precision").fetchall()
                precisions_tested = ", ".join([row[0] for row in precisions])
                vendors = conn.execute("SELECT DISTINCT vendor FROM sparse_results").fetchall()
                vendors_tested = str(len(vendors))

        except sqlite3.DatabaseError as e:
            print(f"Database error: {e}")

    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "benchmark": benchmark,
        "executions": executions,
        "statistics": statistics,
        "execution_data": execution_data,
        "execution_count": execution_count,
        "latest_run": latest_run,
        "precisions_tested": precisions_tested,
        "vendors_tested": vendors_tested,
        "best_performance": best_performance,
        "worst_performance": worst_performance,
        "speedup": speedup,
    }
    return templates.TemplateResponse("benchmark_detail.html", context)


def _collect_migration_status() -> dict[str, Any]:
    """Collect migration status from git and filesystem."""
    # Fortran templates status
    templates_dir = PROJECT_ROOT / "src" / "templates"
    fortran_templates = sorted(templates_dir.glob("*.F.in")) if templates_dir.exists() else []
    modern_templates = sorted(templates_dir.glob("*.f90.in")) if templates_dir.exists() else []

    # C files status
    c_files: list[Path] = []
    for pattern in ["src/*.c", "PORD/lib/*.c", "libseq/*.c"]:
        c_files.extend(PROJECT_ROOT.glob(pattern))
    c_files = sorted(set(c_files))

    # Count modernized files by checking git log
    fortran_modernized = []
    c_modernized = []

    try:
        # Get list of modified template files from git log
        result = subprocess.run(
            ["git", "log", "--name-only", "--pretty=format:", "--all"],
            cwd=str(PROJECT_ROOT),
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode == 0:
            modified_files = set(result.stdout.strip().split('\n'))
            fortran_modernized = [f for f in fortran_templates if f"src/templates/{f.name}" in modified_files]
            c_modernized = [f for f in c_files if str(f.relative_to(PROJECT_ROOT)) in modified_files]
    except (subprocess.TimeoutExpired, subprocess.SubprocessError):
        pass

    total_fortran = len(fortran_templates) + len(modern_templates)
    total_c = len(c_files)

    fortran_done = len(modern_templates) + len(fortran_modernized)
    c_done = len(c_modernized)

    # File-level details
    files = []

    # Add Fortran templates
    for tmpl in fortran_templates:
        is_modern = tmpl in fortran_modernized or any(m.stem == tmpl.stem for m in modern_templates)
        lines = _count_lines(tmpl)
        files.append({
            "name": tmpl.name,
            "type": "Fortran Template",
            "category": "template",
            "status": "Completed" if is_modern else "Pending",
            "lines": lines,
            "path": str(tmpl.relative_to(PROJECT_ROOT)),
            "branch": "fortran-modernization" if is_modern else "-",
        })

    for tmpl in modern_templates:
        lines = _count_lines(tmpl)
        files.append({
            "name": tmpl.name,
            "type": "Fortran Template",
            "category": "template",
            "status": "Already Modern",
            "lines": lines,
            "path": str(tmpl.relative_to(PROJECT_ROOT)),
            "branch": "-",
        })

    # Add C files
    for c_file in c_files:
        is_modern = c_file in c_modernized
        lines = _count_lines(c_file)
        files.append({
            "name": c_file.name,
            "type": "C Source",
            "category": "c_source",
            "status": "Completed" if is_modern else "Pending",
            "lines": lines,
            "path": str(c_file.relative_to(PROJECT_ROOT)),
            "branch": "c23-migration" if is_modern else "-",
        })

    # Calculate statistics
    fortran_pct = round((fortran_done / total_fortran * 100), 1) if total_fortran > 0 else 0
    c_pct = round((c_done / total_c * 100), 1) if total_c > 0 else 0
    overall_pct = round(((fortran_done + c_done) / (total_fortran + total_c) * 100), 1) if (total_fortran + total_c) > 0 else 0

    return {
        "kpis": {
            "overall_progress": overall_pct,
            "fortran_progress": fortran_pct,
            "c_progress": c_pct,
            "total_files": total_fortran + total_c,
            "completed_files": fortran_done + c_done,
            "pending_files": (total_fortran + total_c) - (fortran_done + c_done),
            "fortran_total": total_fortran,
            "fortran_done": fortran_done,
            "c_total": total_c,
            "c_done": c_done,
        },
        "files": sorted(files, key=lambda x: (x["status"] == "Pending", x["type"], x["name"])),
        "categories": ["template", "c_source"],
        "statuses": ["Completed", "Pending", "Already Modern"],
    }


def _empty_migration_status() -> dict[str, Any]:
    return {
        "kpis": {
            "overall_progress": 0.0,
            "fortran_progress": 0.0,
            "c_progress": 0.0,
            "total_files": 0,
            "completed_files": 0,
            "pending_files": 0,
            "fortran_total": 0,
            "fortran_done": 0,
            "c_total": 0,
            "c_done": 0,
        },
        "files": [],
        "categories": ["template", "c_source"],
        "statuses": ["Completed", "Pending", "Already Modern"],
    }


def _collect_migration_status_cached(refresh: bool = False) -> dict[str, Any]:
    now = time.time()
    with _MIGRATION_CACHE_LOCK:
        cached_data = _MIGRATION_CACHE.get("data")
        cache_expiry = float(_MIGRATION_CACHE.get("expires_at", 0.0))
        if not refresh and cached_data is not None and now < cache_expiry:
            return cached_data

    try:
        data = _collect_migration_status()
    except Exception as exc:
        LOGGER.exception("Failed to collect migration status")
        with _MIGRATION_CACHE_LOCK:
            stale_data = _MIGRATION_CACHE.get("data")
            _MIGRATION_CACHE["last_error"] = str(exc)
            _MIGRATION_CACHE["last_error_at"] = now
        if stale_data is not None:
            return stale_data
        return _empty_migration_status()

    with _MIGRATION_CACHE_LOCK:
        _MIGRATION_CACHE["data"] = data
        _MIGRATION_CACHE["expires_at"] = now + MIGRATION_CACHE_TTL_SECONDS
        _MIGRATION_CACHE["last_error"] = None
        _MIGRATION_CACHE["last_error_at"] = 0.0
    return data


@app.get("/migration", response_class=HTMLResponse)
async def migration_status(
    request: Request,
    file_type: str | None = None,
    status: str | None = None,
    category: str | None = None,
    refresh: bool = False,
) -> HTMLResponse:
    """Migration status dashboard with KPIs and filters."""
    data = await run_in_threadpool(_collect_migration_status_cached, refresh)

    # Apply filters
    files = data["files"]
    if file_type:
        files = [f for f in files if f["type"] == file_type]
    if status:
        files = [f for f in files if f["status"] == status]
    if category:
        files = [f for f in files if f["category"] == category]

    context = {
        "request": request,
        "kpis": data["kpis"],
        "files": files,
        "all_files_count": len(data["files"]),
        "filtered_files_count": len(files),
        "file_types": sorted(set(f["type"] for f in data["files"])),
        "categories": data["categories"],
        "statuses": data["statuses"],
        "current_file_type": file_type,
        "current_status": status,
        "current_category": category,
    }
    return templates.TemplateResponse("migration.html", context)


@app.get("/api/migration", response_class=JSONResponse)
async def api_migration_status() -> JSONResponse:
    """API endpoint for migration status data."""
    data = await run_in_threadpool(_collect_migration_status_cached, False)
    return JSONResponse(data)


def _get_pytest_data() -> dict[str, Any]:
    """Query pytest database for test sessions and aggregate statistics."""
    pytest_db_path = PROJECT_ROOT / "mumps.sqlite"

    if not pytest_db_path.exists():
        return {
            "sessions": [],
            "latest_session": None,
            "total_tests": 0,
            "vendors": [],
            "test_status": {"passed": 0, "failed": 0, "skipped": 0},
        }

    try:
        conn = sqlite3.connect(pytest_db_path)
        conn.row_factory = sqlite3.Row

        # Query recent test sessions (last 50)
        sessions_cursor = conn.execute(
            """
            SELECT id, timestamp, python_version, platform, blas_vendor,
                   num_tests, num_passed, num_failed, num_skipped, total_duration
            FROM test_sessions
            ORDER BY timestamp DESC
            LIMIT 50
            """
        )
        sessions = [dict(row) for row in sessions_cursor.fetchall()]

        # Get latest session
        latest_session = sessions[0] if sessions else None

        # Aggregate statistics across all sessions
        stats_cursor = conn.execute(
            """
            SELECT
                SUM(num_tests) as total_tests,
                SUM(num_passed) as total_passed,
                SUM(num_failed) as total_failed,
                SUM(num_skipped) as total_skipped
            FROM test_sessions
            """
        )
        stats = dict(stats_cursor.fetchone())

        # Get unique vendors
        vendors_cursor = conn.execute(
            """
            SELECT DISTINCT blas_vendor
            FROM test_sessions
            WHERE blas_vendor IS NOT NULL
            ORDER BY blas_vendor
            """
        )
        vendors = [row[0] for row in vendors_cursor.fetchall()]

        conn.close()

        return {
            "sessions": sessions,
            "latest_session": latest_session,
            "total_tests": stats.get("total_tests") or 0,
            "vendors": vendors,
            "test_status": {
                "passed": stats.get("total_passed") or 0,
                "failed": stats.get("total_failed") or 0,
                "skipped": stats.get("total_skipped") or 0,
            },
        }
    except (sqlite3.Error, KeyError) as e:
        LOGGER.error(f"Error querying pytest database: {e}")
        return {
            "sessions": [],
            "latest_session": None,
            "total_tests": 0,
            "vendors": [],
            "test_status": {"passed": 0, "failed": 0, "skipped": 0},
        }


def _get_pytest_session_detail(session_id: int) -> dict[str, Any] | None:
    """Query detailed test results for a specific session."""
    pytest_db_path = PROJECT_ROOT / "mumps.sqlite"

    if not pytest_db_path.exists():
        return None

    try:
        conn = sqlite3.connect(pytest_db_path)
        conn.row_factory = sqlite3.Row

        # Query session metadata
        session_cursor = conn.execute(
            """
            SELECT id, timestamp, python_version, platform, blas_vendor,
                   num_tests, num_passed, num_failed, num_skipped, total_duration
            FROM test_sessions
            WHERE id = ?
            """,
            (session_id,)
        )
        session_row = session_cursor.fetchone()

        if session_row is None:
            conn.close()
            return None

        session = dict(session_row)

        # Query all test results for this session
        results_cursor = conn.execute(
            """
            SELECT id, test_name, test_file, test_class, test_function,
                   status, duration, error_message, timestamp
            FROM test_results
            WHERE session_id = ?
            ORDER BY test_name
            """,
            (session_id,)
        )
        test_results = [dict(row) for row in results_cursor.fetchall()]

        # Identify failed tests
        failed_tests = [test for test in test_results if test["status"] == "failed"]

        # Identify slow tests (duration > 1 second)
        slow_tests = [
            test for test in test_results
            if test["duration"] and test["duration"] > 1.0
        ]
        slow_tests.sort(key=lambda t: t["duration"] or 0, reverse=True)

        conn.close()

        return {
            "session": session,
            "test_results": test_results,
            "failed_tests": failed_tests,
            "slow_tests": slow_tests[:10],  # Top 10 slowest
        }
    except (sqlite3.Error, KeyError) as e:
        LOGGER.error(f"Error querying pytest session {session_id}: {e}")
        return None


@app.get("/pytest", response_class=HTMLResponse)
async def pytest_results(request: Request) -> HTMLResponse:
    """Display pytest test results dashboard."""
    pytest_data = await run_in_threadpool(_get_pytest_data)

    context = {
        "request": request,
        "sessions": pytest_data["sessions"],
        "latest_session": pytest_data["latest_session"],
        "total_tests": pytest_data["total_tests"],
        "vendors": pytest_data["vendors"],
        "test_status": pytest_data["test_status"],
    }
    return templates.TemplateResponse("pytest.html", context)


@app.get("/pytest/{session_id}", response_class=HTMLResponse)
async def pytest_session_detail(request: Request, session_id: int) -> HTMLResponse:
    """Display detailed results for a specific test session."""
    session_data = await run_in_threadpool(_get_pytest_session_detail, session_id)

    if session_data is None:
        return templates.TemplateResponse(
            "404.html",
            {
                "request": request,
                "error": f"Test session {session_id} not found",
                "details": "The requested test session does not exist in the database.",
            },
            status_code=404,
        )

    context = {
        "request": request,
        "session": session_data["session"],
        "test_results": session_data["test_results"],
        "failed_tests": session_data["failed_tests"],
        "slow_tests": session_data["slow_tests"],
    }
    return templates.TemplateResponse("pytest_session.html", context)


@app.get("/api/pytest", response_class=JSONResponse)
async def api_pytest_results() -> JSONResponse:
    """API endpoint for pytest results data."""
    data = await run_in_threadpool(_get_pytest_data)
    return JSONResponse(data)


def _get_systems_data() -> dict[str, Any]:
    """Query system information from pytest test sessions."""
    pytest_db_path = PROJECT_ROOT / "mumps.sqlite"

    if not pytest_db_path.exists():
        return {"systems": [], "total_sessions": 0}

    try:
        conn = sqlite3.connect(pytest_db_path)
        conn.row_factory = sqlite3.Row

        # Get unique systems (based on platform + python_version)
        systems_cursor = conn.execute(
            """
            SELECT DISTINCT
                platform,
                python_version,
                COUNT(*) as session_count,
                SUM(num_tests) as total_tests,
                SUM(num_passed) as total_passed,
                MIN(timestamp) as first_seen,
                MAX(timestamp) as last_seen,
                GROUP_CONCAT(DISTINCT blas_vendor) as blas_vendors
            FROM test_sessions
            WHERE platform IS NOT NULL
            GROUP BY platform, python_version
            ORDER BY last_seen DESC
            """
        )
        systems = [dict(row) for row in systems_cursor.fetchall()]

        # Get total session count
        total_cursor = conn.execute("SELECT COUNT(*) as total FROM test_sessions")
        total_sessions = dict(total_cursor.fetchone())["total"]

        conn.close()

        return {
            "systems": systems,
            "total_sessions": total_sessions,
        }
    except (sqlite3.Error, KeyError) as e:
        LOGGER.error(f"Error querying systems data: {e}")
        return {"systems": [], "total_sessions": 0}


def _get_system_detail(platform: str, python_version: str) -> dict[str, Any] | None:
    """Query detailed information for a specific system."""
    pytest_db_path = PROJECT_ROOT / "mumps.sqlite"

    if not pytest_db_path.exists():
        return None

    try:
        conn = sqlite3.connect(pytest_db_path)
        conn.row_factory = sqlite3.Row

        # Get system summary
        system_cursor = conn.execute(
            """
            SELECT
                platform,
                python_version,
                COUNT(*) as session_count,
                SUM(num_tests) as total_tests,
                SUM(num_passed) as total_passed,
                SUM(num_failed) as total_failed,
                MIN(timestamp) as first_seen,
                MAX(timestamp) as last_seen,
                GROUP_CONCAT(DISTINCT blas_vendor) as blas_vendors
            FROM test_sessions
            WHERE platform = ? AND python_version = ?
            GROUP BY platform, python_version
            """,
            (platform, python_version)
        )
        system_row = system_cursor.fetchone()

        if system_row is None:
            conn.close()
            return None

        system = dict(system_row)

        # Get recent test sessions for this system
        sessions_cursor = conn.execute(
            """
            SELECT id, timestamp, blas_vendor,
                   num_tests, num_passed, num_failed, num_skipped, total_duration
            FROM test_sessions
            WHERE platform = ? AND python_version = ?
            ORDER BY timestamp DESC
            LIMIT 20
            """,
            (platform, python_version)
        )
        sessions = [dict(row) for row in sessions_cursor.fetchall()]

        conn.close()

        return {
            "system": system,
            "sessions": sessions,
        }
    except (sqlite3.Error, KeyError) as e:
        LOGGER.error(f"Error querying system detail: {e}")
        return None


@app.get("/docs", response_class=HTMLResponse)
async def documentation(request: Request) -> HTMLResponse:
    """Display MUMPS project documentation."""
    context = {
        "request": request,
    }
    return templates.TemplateResponse("docs.html", context)


@app.get("/systems", response_class=HTMLResponse)
async def systems_list(request: Request) -> HTMLResponse:
    """Display list of systems that have run MUMPS tests."""
    systems_data = await run_in_threadpool(_get_systems_data)

    context = {
        "request": request,
        "systems": systems_data["systems"],
        "total_sessions": systems_data["total_sessions"],
    }
    return templates.TemplateResponse("systems.html", context)


@app.get("/systems/{platform}/{python_version}", response_class=HTMLResponse)
async def system_detail(request: Request, platform: str, python_version: str) -> HTMLResponse:
    """Display detailed information for a specific system."""
    system_data = await run_in_threadpool(_get_system_detail, platform, python_version)

    if system_data is None:
        return templates.TemplateResponse(
            "404.html",
            {
                "request": request,
                "error": f"System not found",
                "details": f"No test sessions found for {platform} with Python {python_version}.",
            },
            status_code=404,
        )

    context = {
        "request": request,
        "system": system_data["system"],
        "sessions": system_data["sessions"],
    }
    return templates.TemplateResponse("system_detail.html", context)


# ── CLI Dashboard Routes ──────────────────────────────────────

_cli_jobs: dict[str, dict[str, Any]] = {}
_cli_jobs_lock = threading.Lock()

_CLI_COMMANDS: dict[str, dict[str, Any]] = {
    "build_all": {"label": "Build All Libraries", "cmd": ["make", "allshared"]},
    "build_library": {"label": "Build Library", "cmd": ["make", "{precision}shared"]},
    "build_examples": {"label": "Build Examples", "cmd": ["bash", "smart_build.sh", "examples", "d"]},
    "build_benchmarks": {"label": "Build Benchmarks", "cmd": ["bash", "smart_build.sh", "benchmarks", "d"]},
    "test_pytest": {"label": "Run Pytest", "cmd": ["python3", "-m", "pytest", "tests/", "-v", "--tb=short"]},
    "test_vendors": {"label": "Test All Vendors", "cmd": ["bash", "scripts/test_all_vendors.sh"]},
    "test_fortran": {"label": "Fortran Tests", "cmd": ["bash", "examples/run_all_tests.sh"]},
    "test_correctness": {"label": "BLAS Correctness", "cmd": ["bash", "scripts/check_blas_correctness.sh"]},
    "benchmark_sparse": {"label": "Sparse Benchmark", "cmd": ["bash", "scripts/benchmark_blas.sh"]},
    "benchmark_dense": {"label": "Dense Benchmark", "cmd": ["bash", "scripts/benchmark_blas_dense.sh"]},
    "benchmark_all": {"label": "Full Benchmark Suite", "cmd": ["bash", "scripts/run_all_benchmarks_to_sqlite.sh"]},
    "vendor_detect": {"label": "Detect BLAS", "cmd": ["python3", "-m", "cli.lib_detect"]},
    "vendor_status": {"label": "Vendor Status", "cmd": ["make", "vendor-status"]},
    "vendor_rebuild": {"label": "Rebuild Vendors", "cmd": ["bash", "scripts/rebuild_vendor_libraries.sh"]},
    "info_config": {"label": "Build Config", "cmd": ["make", "showconfig"]},
    "info_system": {"label": "System Info", "cmd": ["uname", "-a"]},
    "clean": {"label": "Clean Build", "cmd": ["make", "clean"]},
    "webapp_start": {"label": "Start Webapp", "cmd": ["echo", "Webapp is already running"]},
}


def _execute_cli_job(job_id: str, command: str, params: dict[str, str] | None = None) -> None:
    """Execute a CLI job in background thread."""
    import time as _time

    cmd_info = _CLI_COMMANDS.get(command)
    if not cmd_info:
        with _cli_jobs_lock:
            _cli_jobs[job_id]["status"] = "failed"
            _cli_jobs[job_id]["output"] = f"Unknown command: {command}"
            _cli_jobs[job_id]["finished_at"] = _time.strftime("%Y-%m-%d %H:%M:%S")
        return

    cmd = list(cmd_info["cmd"])

    # Apply parameter substitutions
    if params:
        cmd = [part.format(**params) if "{" in part else part for part in cmd]

    env = os.environ.copy()
    lib_path = f"{PROJECT_ROOT}/lib:{PROJECT_ROOT}/src/PORD/lib"
    existing = env.get("LD_LIBRARY_PATH", "")
    env["LD_LIBRARY_PATH"] = f"{lib_path}:{existing}" if existing else lib_path

    output_lines: list[str] = []
    try:
        proc = subprocess.Popen(
            cmd,
            cwd=str(PROJECT_ROOT),
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
        )

        for line in proc.stdout:  # type: ignore[union-attr]
            output_lines.append(line)
            with _cli_jobs_lock:
                _cli_jobs[job_id]["output"] = "".join(output_lines)

        proc.wait()
        status = "completed" if proc.returncode == 0 else "failed"
    except Exception as exc:
        output_lines.append(f"\nError: {exc}\n")
        status = "failed"

    with _cli_jobs_lock:
        _cli_jobs[job_id]["status"] = status
        _cli_jobs[job_id]["output"] = "".join(output_lines)
        _cli_jobs[job_id]["finished_at"] = _time.strftime("%Y-%m-%d %H:%M:%S")

    # Auto-cleanup: keep max 50 jobs
    with _cli_jobs_lock:
        if len(_cli_jobs) > 50:
            oldest = sorted(_cli_jobs.keys())[: len(_cli_jobs) - 50]
            for k in oldest:
                del _cli_jobs[k]


@app.get("/cli", response_class=HTMLResponse)
async def cli_dashboard(request: Request) -> HTMLResponse:
    """CLI dashboard with flowchart and command execution."""
    flowchart_svg = ""
    try:
        from cli.flowchart import generate_flowchart_svg
        flowchart_svg = generate_flowchart_svg()
    except Exception as exc:
        LOGGER.warning(f"Could not generate flowchart: {exc}")
        flowchart_svg = "<p>Flowchart unavailable (graphviz not installed?)</p>"

    # Get recent jobs
    with _cli_jobs_lock:
        recent_jobs = sorted(
            _cli_jobs.values(),
            key=lambda j: j.get("started_at", ""),
            reverse=True,
        )[:10]

    context = {
        "request": request,
        "flowchart_svg": flowchart_svg,
        "recent_jobs": recent_jobs,
    }
    return templates.TemplateResponse("cli.html", context)


@app.post("/api/cli/execute", response_class=JSONResponse)
async def cli_execute(request: Request) -> JSONResponse:
    """Start a background CLI job."""
    body = await request.json()
    command = body.get("command", "")
    params = body.get("params")

    if command not in _CLI_COMMANDS:
        return JSONResponse({"error": f"Unknown command: {command}"}, status_code=400)

    job_id = f"{int(time.time() * 1000)}"
    label = _CLI_COMMANDS[command]["label"]

    with _cli_jobs_lock:
        _cli_jobs[job_id] = {
            "id": job_id,
            "command": label,
            "status": "running",
            "output": "",
            "started_at": time.strftime("%Y-%m-%d %H:%M:%S"),
            "finished_at": None,
        }

    thread = threading.Thread(
        target=_execute_cli_job, args=(job_id, command, params), daemon=True
    )
    thread.start()

    return JSONResponse({"job_id": job_id, "command": label, "status": "running"})


@app.get("/api/cli/status/{job_id}", response_class=JSONResponse)
async def cli_status(job_id: str) -> JSONResponse:
    """Poll a CLI job status."""
    with _cli_jobs_lock:
        job = _cli_jobs.get(job_id)
    if not job:
        return JSONResponse({"error": "Job not found"}, status_code=404)
    return JSONResponse(job)


@app.get("/health", response_class=JSONResponse)
async def health() -> JSONResponse:
    """Health check endpoint for monitoring webapp status."""
    db_exists = DB_PATH.exists()
    results_dir_exists = RESULTS_DIR.exists()
    pytest_db_path = PROJECT_ROOT / "mumps.sqlite"
    pytest_db_exists = pytest_db_path.exists()

    return JSONResponse(
        {
            "status": "healthy",
            "database": {
                "path": str(DB_PATH),
                "exists": db_exists,
            },
            "results_dir": {
                "path": str(RESULTS_DIR),
                "exists": results_dir_exists,
            },
            "pytest_database": {
                "path": str(pytest_db_path),
                "exists": pytest_db_exists,
            },
        }
    )


if __name__ == "__main__":
    import argparse
    import uvicorn

    parser = argparse.ArgumentParser(description="MUMPS Benchmark Results Web Interface")
    parser.add_argument(
        "--port",
        type=int,
        default=int(os.getenv("WEBAPP_PORT", "9002")),
        help="Port to run the web server on (default: 9002, or WEBAPP_PORT env var)",
    )
    parser.add_argument(
        "--host",
        type=str,
        default=os.getenv("WEBAPP_HOST", "127.0.0.1"),
        help="Host to bind to (default: 127.0.0.1, or WEBAPP_HOST env var)",
    )
    parser.add_argument(
        "--reload",
        action="store_true",
        help="Enable auto-reload for development",
    )

    args = parser.parse_args()

    print(f"Starting MUMPS Benchmark Results webapp at http://{args.host}:{args.port}")
    print(f"Results directory: {RESULTS_DIR}")
    print(f"Database: {DB_PATH}")

    uvicorn.run(
        "webapp.main:app",
        host=args.host,
        port=args.port,
        reload=args.reload,
    )
