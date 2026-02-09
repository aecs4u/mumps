#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import sqlite3
from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_RESULTS_DIR = PROJECT_ROOT / "benchmarks" / "results"
DEFAULT_DB_PATH = DEFAULT_RESULTS_DIR / "blas_benchmarks.sqlite"


def read_meta(path: Path) -> dict[str, str]:
    meta: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        meta[key.strip()] = value.strip()
    return meta


def read_tsv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def ensure_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        PRAGMA foreign_keys = ON;

        CREATE TABLE IF NOT EXISTS benchmark_runs (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            benchmark_type TEXT NOT NULL CHECK (benchmark_type IN ('sparse', 'dense')),
            generated_at_utc TEXT NOT NULL,
            recommended_vendor TEXT,
            recommended_value TEXT,
            recommended_metric TEXT,
            created_at_utc TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now'))
        );

        CREATE TABLE IF NOT EXISTS benchmark_meta (
            run_id INTEGER NOT NULL,
            key TEXT NOT NULL,
            value TEXT NOT NULL,
            PRIMARY KEY (run_id, key),
            FOREIGN KEY (run_id) REFERENCES benchmark_runs(id) ON DELETE CASCADE
        );

        CREATE TABLE IF NOT EXISTS sparse_results (
            run_id INTEGER NOT NULL,
            vendor TEXT NOT NULL,
            precision TEXT NOT NULL CHECK (precision IN ('s', 'd', 'c', 'z')),
            ordering TEXT NOT NULL,
            ordering_id INTEGER NOT NULL,
            median_s REAL NOT NULL,
            mean_s REAL NOT NULL,
            best_s REAL NOT NULL,
            runs_s TEXT NOT NULL,
            PRIMARY KEY (run_id, vendor, precision, ordering),
            FOREIGN KEY (run_id) REFERENCES benchmark_runs(id) ON DELETE CASCADE
        );

        CREATE TABLE IF NOT EXISTS dense_scores (
            run_id INTEGER NOT NULL,
            vendor TEXT NOT NULL,
            precision TEXT NOT NULL CHECK (precision IN ('s', 'd', 'c', 'z')),
            score_geomean REAL NOT NULL,
            PRIMARY KEY (run_id, vendor, precision),
            FOREIGN KEY (run_id) REFERENCES benchmark_runs(id) ON DELETE CASCADE
        );

        CREATE TABLE IF NOT EXISTS dense_cases (
            run_id INTEGER NOT NULL,
            vendor TEXT NOT NULL,
            precision TEXT NOT NULL CHECK (precision IN ('s', 'd', 'c', 'z')),
            case_id TEXT NOT NULL,
            m INTEGER NOT NULL,
            n INTEGER NOT NULL,
            k INTEGER NOT NULL,
            med_sec REAL NOT NULL,
            med_gflops REAL NOT NULL,
            mean_gflops REAL NOT NULL,
            PRIMARY KEY (run_id, vendor, precision, case_id),
            FOREIGN KEY (run_id) REFERENCES benchmark_runs(id) ON DELETE CASCADE
        );

        CREATE INDEX IF NOT EXISTS idx_runs_type_generated
            ON benchmark_runs(benchmark_type, generated_at_utc DESC, id DESC);

        CREATE INDEX IF NOT EXISTS idx_sparse_precision
            ON sparse_results(precision, run_id);

        CREATE INDEX IF NOT EXISTS idx_sparse_ordering
            ON sparse_results(ordering, precision, run_id);

        CREATE INDEX IF NOT EXISTS idx_dense_scores_precision
            ON dense_scores(precision, run_id);

        CREATE INDEX IF NOT EXISTS idx_dense_cases_precision
            ON dense_cases(precision, run_id);
        """
    )


def insert_run(conn: sqlite3.Connection, benchmark_type: str, meta: dict[str, str]) -> int:
    cur = conn.execute(
        """
        INSERT INTO benchmark_runs
            (benchmark_type, generated_at_utc, recommended_vendor, recommended_value, recommended_metric)
        VALUES
            (?, ?, ?, ?, ?)
        """,
        (
            benchmark_type,
            meta.get("generated_at_utc", ""),
            meta.get("recommended_vendor"),
            meta.get("recommended_value"),
            meta.get("recommended_metric"),
        ),
    )
    run_id = int(cur.lastrowid)
    conn.executemany(
        "INSERT INTO benchmark_meta(run_id, key, value) VALUES (?, ?, ?)",
        [(run_id, k, v) for k, v in meta.items()],
    )
    return run_id


def ingest_sparse(conn: sqlite3.Connection, results_dir: Path, precisions: list[str]) -> list[int]:
    """Ingest sparse benchmark results with ordering dimension support.

    File naming pattern: sparse_blas_{precision}_{ordering_name}_latest.tsv/meta
    where ordering_name is one of: AMD, AMF, SCOTCH, PORD, METIS, QAMD, Auto
    """
    run_ids = []
    ordering_names = ["AMD", "AMF", "SCOTCH", "PORD", "METIS", "QAMD", "Auto"]

    for precision in precisions:
        # Try to find ordering-specific files first
        found_any = False
        for ordering_name in ordering_names:
            meta_path = results_dir / f"sparse_blas_{precision}_{ordering_name}_latest.meta"
            tsv_path = results_dir / f"sparse_blas_{precision}_{ordering_name}_latest.tsv"

            if not meta_path.exists() or not tsv_path.exists():
                continue

            found_any = True
            meta = read_meta(meta_path)
            rows = read_tsv(tsv_path)

            # Verify precision in metadata matches
            if meta.get("precision") != precision:
                print(f"Warning: Precision mismatch for {meta_path}: expected {precision}, got {meta.get('precision')}")

            # Extract ordering info from metadata
            ordering_id = int(meta.get("ordering", "7"))  # Default to Auto (7)
            ordering = meta.get("ordering_name", ordering_name)

            run_id = insert_run(conn, "sparse", meta)
            conn.executemany(
                """
                INSERT INTO sparse_results(run_id, vendor, precision, ordering, ordering_id, median_s, mean_s, best_s, runs_s)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                [
                    (
                        run_id,
                        row["vendor"],
                        precision,
                        ordering,
                        ordering_id,
                        float(row["median_s"]),
                        float(row["mean_s"]),
                        float(row["best_s"]),
                        row["runs_s"],
                    )
                    for row in rows
                ],
            )
            run_ids.append(run_id)
            print(f"  Ingested sparse precision {precision}, ordering {ordering}: run_id={run_id}")

        # Fallback: try old naming pattern without ordering (for backward compatibility)
        if not found_any:
            meta_path = results_dir / f"sparse_blas_{precision}_latest.meta"
            tsv_path = results_dir / f"sparse_blas_{precision}_latest.tsv"
            if meta_path.exists() and tsv_path.exists():
                meta = read_meta(meta_path)
                rows = read_tsv(tsv_path)

                # Default to Auto ordering if not specified
                ordering_id = int(meta.get("ordering", "7"))
                ordering = meta.get("ordering_name", "Auto")

                run_id = insert_run(conn, "sparse", meta)
                conn.executemany(
                    """
                    INSERT INTO sparse_results(run_id, vendor, precision, ordering, ordering_id, median_s, mean_s, best_s, runs_s)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    [
                        (
                            run_id,
                            row["vendor"],
                            precision,
                            ordering,
                            ordering_id,
                            float(row["median_s"]),
                            float(row["mean_s"]),
                            float(row["best_s"]),
                            row["runs_s"],
                        )
                        for row in rows
                    ],
                )
                run_ids.append(run_id)
                print(f"  Ingested sparse precision {precision} (legacy format): run_id={run_id}")
            else:
                print(f"Warning: No sparse benchmark files found for precision {precision}")

    return run_ids


def ingest_dense(conn: sqlite3.Connection, results_dir: Path, precisions: list[str]) -> list[int]:
    run_ids = []
    for precision in precisions:
        meta_path = results_dir / f"dense_blas_{precision}_latest.meta"
        scores_path = results_dir / f"dense_blas_scores_{precision}_latest.tsv"
        cases_path = results_dir / f"dense_blas_cases_{precision}_latest.tsv"
        if not meta_path.exists() or not scores_path.exists() or not cases_path.exists():
            print(f"Warning: Missing dense benchmark files for precision {precision}")
            continue

        meta = read_meta(meta_path)
        scores = read_tsv(scores_path)
        cases = read_tsv(cases_path)

        # Verify precision in metadata matches
        if meta.get("precision") != precision:
            print(f"Warning: Precision mismatch for {meta_path}: expected {precision}, got {meta.get('precision')}")

        run_id = insert_run(conn, "dense", meta)
        conn.executemany(
            """
            INSERT INTO dense_scores(run_id, vendor, precision, score_geomean)
            VALUES (?, ?, ?, ?)
            """,
            [(run_id, row["vendor"], precision, float(row["score_geomean"])) for row in scores],
        )
        conn.executemany(
            """
            INSERT INTO dense_cases(run_id, vendor, precision, case_id, m, n, k, med_sec, med_gflops, mean_gflops)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
                (
                    run_id,
                    row["vendor"],
                    precision,
                    row["case_id"],
                    int(row["m"]),
                    int(row["n"]),
                    int(row["k"]),
                    float(row["med_sec"]),
                    float(row["med_gflops"]),
                    float(row["mean_gflops"]),
                )
                for row in cases
            ],
        )
        run_ids.append(run_id)
        print(f"  Ingested dense precision {precision}: run_id={run_id}")
    return run_ids


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Ingest benchmark TSV/meta files into SQLite.")
    parser.add_argument(
        "--results-dir",
        type=Path,
        default=DEFAULT_RESULTS_DIR,
        help=f"Directory containing *_latest.tsv/meta files (default: {DEFAULT_RESULTS_DIR})",
    )
    parser.add_argument(
        "--db",
        type=Path,
        default=DEFAULT_DB_PATH,
        help=f"SQLite database path (default: {DEFAULT_DB_PATH})",
    )
    parser.add_argument(
        "--precisions",
        type=str,
        default="d",
        help="Space-separated list of precisions to ingest (s d c z) (default: d)",
    )
    parser.add_argument(
        "--skip-sparse",
        action="store_true",
        help="Skip ingesting sparse benchmark results.",
    )
    parser.add_argument(
        "--skip-dense",
        action="store_true",
        help="Skip ingesting dense benchmark results.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    args.db.parent.mkdir(parents=True, exist_ok=True)
    args.results_dir.mkdir(parents=True, exist_ok=True)

    precisions = args.precisions.split()
    print(f"Ingesting benchmark results for precisions: {precisions}")

    with sqlite3.connect(args.db) as conn:
        ensure_schema(conn)
        sparse_run_ids = []
        dense_run_ids = []
        if not args.skip_sparse:
            print("Ingesting sparse benchmarks...")
            sparse_run_ids = ingest_sparse(conn, args.results_dir, precisions)
        if not args.skip_dense:
            print("Ingesting dense benchmarks...")
            dense_run_ids = ingest_dense(conn, args.results_dir, precisions)
        conn.commit()

    print(f"\nSQLite database updated: {args.db}")
    if sparse_run_ids:
        print(f"  sparse run ids: {sparse_run_ids}")
    if dense_run_ids:
        print(f"  dense run ids: {dense_run_ids}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
