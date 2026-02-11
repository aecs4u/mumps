"""
Comprehensive test suite for MUMPS Python interface with scipy validation.
"""

import pytest
import numpy as np
from numpy.testing import assert_allclose
import scipy.sparse as sp
import scipy.sparse.linalg as spla

from pymumps import MUMPS, solve_system


class TestMUMPSBasic:
    """Basic functionality tests."""

    def test_simple_2x2_system(self, small_matrix):
        """Test simple 2x2 system from examples."""
        n, irn, jcn, a, b, x_expected = small_matrix

        with MUMPS(n=n, sym=0, par=1) as mumps:
            mumps.set_matrix(irn, jcn, a)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x = mumps.get_solution()

        assert_allclose(x, x_expected, rtol=1e-10)

    def test_identity_matrix(self):
        """Test identity matrix."""
        n = 5
        I = sp.eye(n, format='coo')
        b = np.random.rand(n)

        x = solve_system(I, b)
        assert_allclose(x, b, rtol=1e-12)

    def test_diagonal_matrix(self):
        """Test diagonal matrix."""
        n = 10
        d = np.random.rand(n) + 1.0  # Ensure positive
        A = sp.diags(d, format='coo')
        b = np.random.rand(n)

        x = solve_system(A, b)
        expected = b / d
        assert_allclose(x, expected, rtol=1e-10)

    def test_tridiagonal_matrix(self):
        """Test tridiagonal matrix."""
        n = 100
        A = sp.diags([1.0, -2.0, 1.0], [-1, 0, 1], shape=(n, n), format='coo')
        b = np.ones(n)

        x = solve_system(A, b)
        # Verify Ax = b
        assert_allclose(A @ x, b, rtol=1e-8)

    def test_context_manager(self, small_matrix):
        """Test context manager protocol."""
        n, irn, jcn, a, b, x_expected = small_matrix

        # Should not raise
        with MUMPS(n=n) as mumps:
            mumps.set_matrix(irn, jcn, a)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x = mumps.get_solution()

        assert_allclose(x, x_expected, rtol=1e-10)


class TestMUMPSPrecision:
    """Precision and accuracy tests."""

    @pytest.mark.parametrize("n", [10, 50, 100])
    def test_random_spd_matrix(self, n, random_spd_matrix):
        """Test random symmetric positive definite matrices."""
        A = random_spd_matrix(n)
        b = np.random.rand(n)
        x = solve_system(A, b, sym=1)  # Use SPD solver

        # Verify solution
        assert_allclose(A.toarray() @ x, b, rtol=1e-8)

    def test_ill_conditioned_matrix(self):
        """Test ill-conditioned matrix."""
        n = 20
        cond_num = 1e6
        U = np.linalg.qr(np.random.rand(n, n))[0]
        s = np.logspace(0, -np.log10(cond_num), n)
        A_dense = U @ np.diag(s) @ U.T
        A = sp.coo_matrix(A_dense)

        b = np.random.rand(n)
        x = solve_system(A, b)

        # Check residual (not solution accuracy due to ill-conditioning)
        residual = np.linalg.norm(A @ x - b) / np.linalg.norm(b)
        assert residual < 1e-6


class TestMUMPSSymmetry:
    """Tests for symmetric matrices."""

    def test_symmetric_positive_definite(self):
        """Test SPD matrix with sym=1."""
        n = 10
        A_dense = np.random.rand(n, n)
        A_dense = A_dense @ A_dense.T + n * np.eye(n)
        A_coo = sp.coo_matrix(A_dense)

        b = np.random.rand(n)

        with MUMPS(n=n, sym=1) as mumps:  # SPD
            mumps.set_matrix(A_coo.row + 1, A_coo.col + 1, A_coo.data)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x = mumps.get_solution()

        assert_allclose(A_dense @ x, b, rtol=1e-8)

    def test_general_symmetric(self):
        """Test general symmetric matrix with sym=2."""
        n = 10
        A_dense = np.random.rand(n, n)
        A_dense = (A_dense + A_dense.T) / 2 + np.eye(n)  # Make symmetric and non-singular
        A_coo = sp.coo_matrix(A_dense)

        b = np.random.rand(n)

        with MUMPS(n=n, sym=2) as mumps:  # General symmetric
            mumps.set_matrix(A_coo.row + 1, A_coo.col + 1, A_coo.data)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x = mumps.get_solution()

        assert_allclose(A_dense @ x, b, rtol=1e-8)


class TestMUMPSvsSciPy:
    """Validate MUMPS against SciPy reference solvers."""

    @pytest.mark.parametrize("n", [10, 50, 100])
    def test_mumps_matches_scipy_spsolve(self, n):
        """Compare MUMPS with scipy.sparse.linalg.spsolve."""
        # Generate random sparse matrix
        A = sp.random(n, n, density=0.1, format='csr') + sp.eye(n)
        A = A + A.T  # Make symmetric for stability
        b = np.random.rand(n)

        # Solve with scipy (reference)
        x_scipy = spla.spsolve(A.tocsr(), b)

        # Solve with MUMPS
        x_mumps = solve_system(A, b)

        # Validate results match (relax tolerance for cross-solver comparison)
        assert_allclose(x_mumps, x_scipy, rtol=1e-9)
        # Also check residual
        residual = np.linalg.norm(A @ x_mumps - b) / np.linalg.norm(b)
        assert residual < 1e-10

    def test_mumps_matches_scipy_lu_factor(self):
        """Compare MUMPS with scipy.sparse.linalg.splu."""
        n = 100
        A = sp.random(n, n, density=0.05, format='csc') + 2 * sp.eye(n)
        b = np.random.rand(n)

        # Solve with scipy LU factorization
        lu = spla.splu(A.tocsc())
        x_scipy = lu.solve(b)

        # Solve with MUMPS
        x_mumps = solve_system(A, b)

        assert_allclose(x_mumps, x_scipy, rtol=1e-9)

    @pytest.mark.parametrize("sym", [0, 1, 2])
    def test_mumps_symmetry_vs_scipy(self, sym):
        """Validate symmetric solvers against scipy."""
        n = 50
        if sym == 0:  # Unsymmetric
            A = sp.random(n, n, density=0.1) + sp.eye(n)
        elif sym == 1:  # SPD
            B = sp.random(n, n, density=0.1)
            A = B @ B.T + n * sp.eye(n)
        else:  # sym == 2, general symmetric
            A = sp.random(n, n, density=0.1)
            A = (A + A.T) / 2 + sp.eye(n)

        b = np.random.rand(n)

        # Solve with scipy
        x_scipy = spla.spsolve(A.tocsr(), b)

        # Solve with MUMPS
        A_coo = sp.coo_matrix(A)
        with MUMPS(n=n, sym=sym) as mumps:
            mumps.set_matrix(A_coo.row + 1, A_coo.col + 1, A_coo.data)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x_mumps = mumps.get_solution()

        assert_allclose(x_mumps, x_scipy, rtol=1e-9)

    @pytest.mark.slow
    def test_mumps_large_sparse_system(self):
        """Test MUMPS on larger sparse system and compare with scipy."""
        n = 500
        A = sp.random(n, n, density=0.01, format='csr') + sp.eye(n)
        b = np.random.rand(n)

        # Solve with scipy
        x_scipy = spla.spsolve(A, b)

        # Solve with MUMPS
        x_mumps = solve_system(A, b)

        assert_allclose(x_mumps, x_scipy, rtol=1e-9)


class TestMUMPSErrorHandling:
    """Error handling tests."""

    def test_singular_matrix(self):
        """Test that singular matrix raises error."""
        n = 5
        A = sp.coo_matrix((n, n))  # Zero matrix (singular)
        b = np.ones(n)

        with pytest.raises(RuntimeError, match="MUMPS error"):
            solve_system(A, b)

    def test_dimension_mismatch(self):
        """Test dimension mismatch detection."""
        n = 5
        A = sp.eye(n, format='coo')
        b = np.ones(n + 1)  # Wrong dimension

        with pytest.raises(ValueError, match="dimension"):
            solve_system(A, b)

    def test_finalized_instance(self):
        """Test that using finalized instance raises error."""
        n = 2
        mumps = MUMPS(n=n)
        mumps.finalize()

        with pytest.raises(RuntimeError, match="finalized"):
            mumps.analyze()


class TestMUMPSControl:
    """Control parameter tests."""

    def test_suppress_output(self, small_matrix):
        """Test suppressing output with ICNTL."""
        n, irn, jcn, a, b, x_expected = small_matrix

        with MUMPS(n=n) as mumps:
            # Suppress all output
            mumps.set_icntl(1, -1)  # Error messages
            mumps.set_icntl(2, -1)  # Diagnostic messages
            mumps.set_icntl(3, -1)  # Global information
            mumps.set_icntl(4, 0)   # Errors, warnings, diagnostics

            mumps.set_matrix(irn, jcn, a)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x = mumps.get_solution()

        assert_allclose(x, x_expected, rtol=1e-12)

    def test_get_icntl(self):
        """Test getting ICNTL parameters."""
        with MUMPS(n=2) as mumps:
            mumps.set_icntl(1, -1)
            assert mumps.get_icntl(1) == -1

    def test_set_cntl(self, small_matrix):
        """Test setting real control parameters."""
        n, irn, jcn, a, b, x_expected = small_matrix

        with MUMPS(n=n) as mumps:
            mumps.set_cntl(1, 0.01)  # Pivoting threshold
            mumps.set_matrix(irn, jcn, a)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()
            mumps.solve()
            x = mumps.get_solution()

        assert_allclose(x, x_expected, rtol=1e-10)

    def test_get_info(self, small_matrix):
        """Test getting INFO parameters."""
        n, irn, jcn, a, b, _ = small_matrix

        with MUMPS(n=n) as mumps:
            mumps.set_matrix(irn, jcn, a)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()

            # INFO(1) should be 0 (success)
            assert mumps.get_info(1) == 0

    def test_get_rinfo(self, small_matrix):
        """Test getting RINFO parameters."""
        n, irn, jcn, a, b, _ = small_matrix

        with MUMPS(n=n) as mumps:
            mumps.set_matrix(irn, jcn, a)
            mumps.set_rhs(b)
            mumps.analyze()
            mumps.factorize()

            # RINFO should contain numerical statistics
            # Just check it returns a float
            rinfo_1 = mumps.get_rinfo(1)
            assert isinstance(rinfo_1, float)


@pytest.mark.benchmark
class TestMUMPSPerformance:
    """Performance benchmarks."""

    @pytest.mark.parametrize("n", [100, 200])
    def test_solve_time(self, benchmark, n):
        """Benchmark solve time for various matrix sizes."""
        # Generate sparse matrix (5 diagonals)
        A = sp.diags([1.0, 1.0, -4.0, 1.0, 1.0], [-2, -1, 0, 1, 2], shape=(n, n), format='coo')
        b = np.random.rand(n)

        def solve():
            return solve_system(A, b)

        result = benchmark(solve)
        # Ensure it works
        assert len(result) == n

    @pytest.mark.slow
    def test_performance_vs_scipy(self, benchmark):
        """Compare performance with scipy.sparse.spsolve."""
        n = 500
        A = sp.random(n, n, density=0.01, format='csr') + sp.eye(n)
        b = np.random.rand(n)

        # Benchmark MUMPS
        x_mumps = benchmark(lambda: solve_system(A, b))

        # Verify correctness against scipy (larger tolerance for 500x500 system)
        x_scipy = spla.spsolve(A, b)
        assert_allclose(x_mumps, x_scipy, rtol=1e-8)
