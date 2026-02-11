"""
pytest configuration and fixtures for MUMPS testing.
"""

import os
import sys
from pathlib import Path

import pytest
import numpy as np
import scipy.sparse as sp

# Add python directory to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root / "python"))

# Import SQLite plugin for test result storage
pytest_plugins = ['pytest_sqlite_plugin']

# Ensure LD_LIBRARY_PATH is set for library loading
lib_dir = project_root / "lib"
pord_dir = project_root / "src" / "PORD" / "lib"
ld_path = f"{lib_dir}:{pord_dir}:{os.environ.get('LD_LIBRARY_PATH', '')}"
os.environ["LD_LIBRARY_PATH"] = ld_path


@pytest.fixture
def small_matrix():
    """Simple 2x2 test matrix."""
    n = 2
    irn = np.array([1, 1, 2, 2], dtype=np.int32)
    jcn = np.array([1, 2, 1, 2], dtype=np.int32)
    a = np.array([2.0, 3.0, 3.0, 2.0], dtype=np.float64)
    b = np.array([8.0, 13.0])
    x_expected = np.array([4.6, -0.4])
    return n, irn, jcn, a, b, x_expected


@pytest.fixture
def random_spd_matrix():
    """Generate a random symmetric positive definite matrix."""
    def _generate(n, density=0.1):
        A = sp.random(n, n, density=density, format='csr')
        A = A @ A.T + n * sp.eye(n)  # Make SPD
        return sp.coo_matrix(A)
    return _generate


@pytest.fixture
def random_sparse_matrix():
    """Generate a random sparse matrix."""
    def _generate(n, density=0.1):
        A = sp.random(n, n, density=density, format='csr')
        A = A + sp.eye(n)  # Ensure non-singular
        return sp.coo_matrix(A)
    return _generate


@pytest.fixture(params=["openblas", "blis", "mkl", "atlas", "reference"])
def vendor(request):
    """Parametrize tests over different BLAS vendors."""
    return request.param
