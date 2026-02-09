from __future__ import annotations

import csv
import os
import sqlite3
from pathlib import Path
from typing import Any

from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

PROJECT_ROOT = Path(__file__).resolve().parents[1]
RESULTS_DIR = Path(os.getenv("BLAS_BENCH_RESULTS_DIR", PROJECT_ROOT / "benchmarks" / "results"))
DB_PATH = Path(os.getenv("BLAS_BENCH_DB", RESULTS_DIR / "blas_benchmarks.sqlite"))
TEMPLATES_DIR = Path(__file__).resolve().parent / "templates"
STATIC_DIR = Path(__file__).resolve().parent / "static"


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


def load_sparse_results_from_db(precision: str | None = None) -> dict[str, Any]:
    with sqlite3.connect(DB_PATH) as conn:
        conn.row_factory = sqlite3.Row
        run = _latest_run(conn, "sparse")
        if run is None:
            return {"rows": [], "meta": {}, "precisions": []}

        # Get available precisions for this run
        precisions_query = """
            SELECT DISTINCT precision
            FROM sparse_results
            WHERE run_id = ?
            ORDER BY precision
        """
        precisions = [r[0] for r in conn.execute(precisions_query, (run["id"],)).fetchall()]

        # Query with optional precision filter
        query = """
            SELECT vendor, precision, median_s, mean_s, best_s, runs_s
            FROM sparse_results
            WHERE run_id = ?
        """
        params = [run["id"]]

        if precision:
            query += " AND precision = ?"
            params.append(precision)

        query += " ORDER BY precision ASC, median_s ASC"

        rows = [dict(row) for row in conn.execute(query, params).fetchall()]

        meta = _run_meta(conn, int(run["id"]))
        meta.setdefault("generated_at_utc", str(run["generated_at_utc"]))
        meta.setdefault("recommended_vendor", str(run["recommended_vendor"] or ""))
        meta.setdefault("recommended_value", str(run["recommended_value"] or ""))
        meta.setdefault("recommended_metric", str(run["recommended_metric"] or ""))
        requested = _parse_vendors_field(meta.get("vendors"))
        present = [str(r.get("vendor", "")) for r in rows]
        missing = [v for v in requested if v not in present]
        return {"rows": rows, "meta": meta, "missing_vendors": missing, "precisions": precisions}


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


def load_sparse_results(precision: str | None = None) -> dict[str, Any]:
    if DB_PATH.exists():
        try:
            return load_sparse_results_from_db(precision=precision)
        except sqlite3.DatabaseError:
            pass
    # TSV fallback - load precision-specific file or default to 'd'
    prec = precision or 'd'
    tsv_path = RESULTS_DIR / f"sparse_blas_{prec}_latest.tsv"
    if not tsv_path.exists():
        tsv_path = RESULTS_DIR / "sparse_blas_latest.tsv"  # Fallback to old naming
    rows = _read_tsv(tsv_path) if tsv_path.exists() else []
    for row in rows:
        row["median_s_num"] = _to_float(row.get("median_s"))
        row["mean_s_num"] = _to_float(row.get("mean_s"))
        row["best_s_num"] = _to_float(row.get("best_s"))
        row["precision"] = prec
    rows.sort(key=lambda r: (r.get("median_s_num") is None, r.get("median_s_num")))
    meta_path = RESULTS_DIR / f"sparse_blas_{prec}_latest.meta"
    if not meta_path.exists():
        meta_path = RESULTS_DIR / "sparse_blas_latest.meta"
    meta = _read_meta(meta_path) if meta_path.exists() else {}
    requested = _parse_vendors_field(meta.get("vendors"))
    present = [str(r.get("vendor", "")) for r in rows]
    missing = [v for v in requested if v not in present]
    return {"rows": rows, "meta": meta, "missing_vendors": missing, "precisions": [prec]}


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
async def index(request: Request, precision: str = "d") -> HTMLResponse:
    # Validate precision
    if precision not in ['s', 'd', 'c', 'z']:
        precision = 'd'

    sparse = load_sparse_results(precision=precision)
    dense = load_dense_results(precision=precision)

    # Get all available precisions (union of sparse and dense)
    all_precisions = sorted(set(sparse.get("precisions", []) + dense.get("precisions", [])))

    precision_names = {
        's': 'Single (float)',
        'd': 'Double (double)',
        'c': 'Complex Single',
        'z': 'Complex Double',
    }

    context = {
        "request": request,
        "results_dir": str(RESULTS_DIR),
        "db_path": str(DB_PATH),
        "sparse": sparse,
        "dense": dense,
        "current_precision": precision,
        "all_precisions": all_precisions if all_precisions else ['d'],
        "precision_names": precision_names,
    }
    return templates.TemplateResponse("index.html", context)


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
