from __future__ import annotations

import contextlib
import importlib.util
from io import StringIO
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch


REPO_ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = REPO_ROOT / "scripts" / "generate_test_matrices.py"


def _load_module():
    spec = importlib.util.spec_from_file_location("generate_test_matrices", MODULE_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Unable to load module at {MODULE_PATH}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


gtm = _load_module()


class GenerateTestMatricesTests(unittest.TestCase):
    def test_generate_diagonal_writes_expected_shape_and_entries(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            output = Path(tmpdir) / "diag.coo"
            gtm.generate_diagonal(3, output)
            lines = output.read_text(encoding="utf-8").splitlines()

        self.assertEqual(lines[2], "3 3")
        self.assertEqual(lines[4], "1 1 1.0")
        self.assertEqual(lines[5], "2 2 2.0")
        self.assertEqual(lines[6], "3 3 3.0")

    def test_generate_laplacian_2d_2x2_writes_expected_nnz(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            output = Path(tmpdir) / "lap.coo"
            gtm.generate_laplacian_2d(2, output)
            lines = output.read_text(encoding="utf-8").splitlines()

        self.assertEqual(lines[2], "4 12")
        self.assertIn("1 1 4.0", lines)
        self.assertIn("1 2 -1.0", lines)
        self.assertIn("1 3 -1.0", lines)

    def test_generate_tridiagonal_writes_expected_pattern(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            output = Path(tmpdir) / "tri.coo"
            gtm.generate_tridiagonal(3, output)
            lines = output.read_text(encoding="utf-8").splitlines()

        self.assertEqual(lines[2], "3 7")
        self.assertIn("1 1 2.0", lines)
        self.assertIn("1 2 -1.0", lines)
        self.assertIn("2 1 -1.0", lines)
        self.assertIn("3 2 -1.0", lines)

    def test_main_returns_error_for_missing_output_with_type(self) -> None:
        with patch.object(sys, "argv", ["prog", "--type", "diagonal", "--n", "3"]):
            stderr = StringIO()
            with contextlib.redirect_stderr(stderr):
                rc = gtm.main()

        self.assertEqual(rc, 1)
        self.assertIn("--output required", stderr.getvalue())

    def test_main_returns_error_for_laplacian_missing_ngrid(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            output = Path(tmpdir) / "lap.coo"
            with patch.object(sys, "argv", ["prog", "--type", "laplacian", "--output", str(output)]):
                stderr = StringIO()
                with contextlib.redirect_stderr(stderr):
                    rc = gtm.main()

        self.assertEqual(rc, 1)
        self.assertIn("--ngrid required", stderr.getvalue())

    def test_main_generates_small_size_preset(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            outdir = Path(tmpdir)
            with patch.object(sys, "argv", ["prog", "--size", "small", "--output-dir", str(outdir)]):
                rc = gtm.main()

            self.assertEqual(rc, 0)
            self.assertTrue((outdir / "test_tiny_10x10_diagonal.coo").exists())
            self.assertTrue((outdir / "test_small_10x10_laplacian.coo").exists())
            self.assertTrue((outdir / "test_small_100_tridiagonal.coo").exists())


if __name__ == "__main__":
    unittest.main()
