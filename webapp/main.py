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

    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "sparse_execution_count": sparse_execution_count if sparse_execution_count > 0 else None,
        "dense_execution_count": dense_execution_count if dense_execution_count > 0 else None,
        "recent_runs": recent_runs,
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


@app.get("/health", response_class=JSONResponse)
async def health() -> JSONResponse:
    """Health check endpoint for monitoring webapp status."""
    db_exists = DB_PATH.exists()
    results_dir_exists = RESULTS_DIR.exists()

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
