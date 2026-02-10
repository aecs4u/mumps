from __future__ import annotations

import contextlib
import importlib.util
from io import StringIO
from pathlib import Path
import sqlite3
import sys
import tempfile
import unittest
from unittest.mock import patch


REPO_ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = REPO_ROOT / "scripts" / "ingest_benchmark_results_sqlite.py"


def _load_module():
    spec = importlib.util.spec_from_file_location("ingest_benchmark_results_sqlite", MODULE_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Unable to load module at {MODULE_PATH}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ingest = _load_module()


class IngestBenchmarkResultsSQLiteTests(unittest.TestCase):
    def setUp(self) -> None:
        self.conn = sqlite3.connect(":memory:")
        self.conn.row_factory = sqlite3.Row
        ingest.ensure_schema(self.conn)

    def tearDown(self) -> None:
        self.conn.close()

    def test_read_meta_ignores_comments_and_invalid_lines(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "x.meta"
            path.write_text(
                "# comment\n"
                "generated_at_utc=2026-02-09T00:00:00Z\n"
                "invalid_line\n"
                "precision=d\n",
                encoding="utf-8",
            )
            meta = ingest.read_meta(path)

        self.assertEqual(meta["generated_at_utc"], "2026-02-09T00:00:00Z")
        self.assertEqual(meta["precision"], "d")
        self.assertNotIn("invalid_line", meta)

    def test_ingest_sparse_ordering_specific_files(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            results_dir = Path(tmpdir)
            (results_dir / "sparse_blas_d_AMD_latest.meta").write_text(
                "generated_at_utc=2026-02-09T00:00:00Z\n"
                "precision=d\n"
                "ordering=0\n"
                "ordering_name=AMD\n"
                "vendors=openblas blis\n",
                encoding="utf-8",
            )
            (results_dir / "sparse_blas_d_AMD_latest.tsv").write_text(
                "vendor\tmedian_s\tmean_s\tbest_s\truns_s\n"
                "openblas\t1.0\t1.1\t0.9\t1.0,1.1,0.9\n"
                "blis\t2.0\t2.1\t1.9\t2.0,2.1,1.9\n",
                encoding="utf-8",
            )

            run_ids = ingest.ingest_sparse(self.conn, results_dir, ["d"])

        self.assertEqual(len(run_ids), 1)
        count = self.conn.execute("SELECT COUNT(*) FROM sparse_results").fetchone()[0]
        self.assertEqual(count, 2)
        row = self.conn.execute("SELECT ordering, ordering_id FROM sparse_results LIMIT 1").fetchone()
        self.assertEqual(row["ordering"], "AMD")
        self.assertEqual(row["ordering_id"], 0)

    def test_ingest_sparse_legacy_files_default_to_auto_ordering(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            results_dir = Path(tmpdir)
            (results_dir / "sparse_blas_d_latest.meta").write_text(
                "generated_at_utc=2026-02-09T00:00:00Z\n"
                "precision=d\n",
                encoding="utf-8",
            )
            (results_dir / "sparse_blas_d_latest.tsv").write_text(
                "vendor\tmedian_s\tmean_s\tbest_s\truns_s\n"
                "openblas\t1.0\t1.1\t0.9\t1.0,1.1,0.9\n",
                encoding="utf-8",
            )

            run_ids = ingest.ingest_sparse(self.conn, results_dir, ["d"])

        self.assertEqual(len(run_ids), 1)
        row = self.conn.execute("SELECT ordering, ordering_id FROM sparse_results LIMIT 1").fetchone()
        self.assertEqual(row["ordering"], "Auto")
        self.assertEqual(row["ordering_id"], 7)

    def test_ingest_dense_missing_files_returns_no_runs(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            run_ids = ingest.ingest_dense(self.conn, Path(tmpdir), ["d"])
        self.assertEqual(run_ids, [])

    def test_ingest_dense_inserts_scores_and_cases(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            results_dir = Path(tmpdir)
            (results_dir / "dense_blas_d_latest.meta").write_text(
                "generated_at_utc=2026-02-09T00:00:00Z\n"
                "precision=d\n"
                "vendors=openblas blis\n",
                encoding="utf-8",
            )
            (results_dir / "dense_blas_scores_d_latest.tsv").write_text(
                "vendor\tscore_geomean\n"
                "openblas\t101.0\n"
                "blis\t95.0\n",
                encoding="utf-8",
            )
            (results_dir / "dense_blas_cases_d_latest.tsv").write_text(
                "vendor\tcase_id\tm\tn\tk\tmed_sec\tmed_gflops\tmean_gflops\n"
                "openblas\tm64n64k64\t64\t64\t64\t0.1\t10.0\t9.8\n"
                "blis\tm64n64k64\t64\t64\t64\t0.11\t9.2\t9.0\n",
                encoding="utf-8",
            )

            run_ids = ingest.ingest_dense(self.conn, results_dir, ["d"])

        self.assertEqual(len(run_ids), 1)
        self.assertEqual(self.conn.execute("SELECT COUNT(*) FROM dense_scores").fetchone()[0], 2)
        self.assertEqual(self.conn.execute("SELECT COUNT(*) FROM dense_cases").fetchone()[0], 2)

    def test_main_skip_sparse_ingests_dense_only(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            results_dir = root / "results"
            db_path = root / "out.sqlite"
            results_dir.mkdir(parents=True, exist_ok=True)

            (results_dir / "dense_blas_d_latest.meta").write_text(
                "generated_at_utc=2026-02-09T00:00:00Z\n"
                "precision=d\n",
                encoding="utf-8",
            )
            (results_dir / "dense_blas_scores_d_latest.tsv").write_text(
                "vendor\tscore_geomean\n"
                "openblas\t101.0\n",
                encoding="utf-8",
            )
            (results_dir / "dense_blas_cases_d_latest.tsv").write_text(
                "vendor\tcase_id\tm\tn\tk\tmed_sec\tmed_gflops\tmean_gflops\n"
                "openblas\tm64n64k64\t64\t64\t64\t0.1\t10.0\t9.8\n",
                encoding="utf-8",
            )

            argv = [
                "prog",
                "--results-dir",
                str(results_dir),
                "--db",
                str(db_path),
                "--precisions",
                "d",
                "--skip-sparse",
            ]
            with patch.object(sys, "argv", argv):
                stdout = StringIO()
                with contextlib.redirect_stdout(stdout):
                    rc = ingest.main()

            self.assertEqual(rc, 0)
            with sqlite3.connect(db_path) as conn:
                rows = conn.execute("SELECT benchmark_type FROM benchmark_runs").fetchall()
                self.assertEqual(len(rows), 1)
                self.assertEqual(rows[0][0], "dense")


if __name__ == "__main__":
    unittest.main()
