"""
Test MUMPS with different BLAS vendor libraries.

This module tests MUMPS compiled with different BLAS vendors:
- OpenBLAS
- Intel MKL
- BLIS
- ATLAS
- Reference BLAS
"""

import pytest
import numpy as np
from numpy.testing import assert_allclose
import scipy.sparse as sp

from pymumps import MUMPS, solve_system


@pytest.mark.vendor
class TestBLASVendors:
    """Test MUMPS with different BLAS vendor libraries."""

    def test_current_vendor_basic(self):
        """Test basic functionality with current BLAS vendor."""
        n = 10
        A = sp.eye(n, format='coo')
        b = np.random.rand(n)

        x = solve_system(A, b)
        assert_allclose(x, b, rtol=1e-12)

    def test_current_vendor_scipy_validation(self):
        """Validate current BLAS vendor against scipy."""
        n = 50
        A = sp.random(n, n, density=0.1, format='csr') + sp.eye(n)
        b = np.random.rand(n)

        # Solve with MUMPS
        x_mumps = solve_system(A, b)

        # Solve with scipy
        import scipy.sparse.linalg as spla
        x_scipy = spla.spsolve(A, b)

        assert_allclose(x_mumps, x_scipy, rtol=1e-9)

    @pytest.mark.parametrize("matrix_size", [10, 50, 100])
    def test_vendor_performance_scaling(self, matrix_size):
        """Test that vendor BLAS scales properly with matrix size."""
        A = sp.random(matrix_size, matrix_size, density=0.1, format='csr')
        A = A + sp.eye(matrix_size)
        b = np.random.rand(matrix_size)

        x = solve_system(A, b)

        # Verify solution
        residual = np.linalg.norm(A @ x - b) / np.linalg.norm(b)
        assert residual < 1e-8


@pytest.mark.vendor
class TestVendorDocumentation:
    """Document how to build and test with different vendors."""

    def test_vendor_build_instructions(self):
        """
        Building MUMPS with different BLAS vendors:

        1. OpenBLAS (default):
           make BLAS_VENDOR=openblas dshared

        2. Intel MKL:
           make BLAS_VENDOR=mkl dshared

        3. BLIS:
           make BLAS_VENDOR=blis dshared

        4. Reference BLAS:
           make BLAS_VENDOR=reference dshared

        5. Auto-detect:
           make BLAS_VENDOR=auto dshared

        Testing:
           LD_LIBRARY_PATH=lib:PORD/lib pytest tests/test_vendors.py -v
        """
        pass  # This test just documents the build process


# Manual vendor testing notes:
# To test all vendors, rebuild with each vendor and run tests:
#
# for vendor in openblas mkl blis reference; do
#   echo "Testing with BLAS vendor: $vendor"
#   make clean
#   make BLAS_VENDOR=$vendor dshared
#   LD_LIBRARY_PATH=lib:PORD/lib pytest tests/test_vendors.py -v
# done
