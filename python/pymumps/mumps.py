"""
MUMPS Python interface using ctypes.

This module provides a simplified Python interface to MUMPS sparse direct solver
via the iso_c_binding Fortran interface.
"""

import ctypes
import numpy as np
from numpy.ctypeslib import ndpointer
from pathlib import Path




# Load dependencies first (order matters!)
try:
    # Load libraries by name, letting LD_LIBRARY_PATH determine location
    # This allows vendor-specific libraries to be loaded from lib/vendors/<vendor>/
    # Order matters: lowest-level dependencies first!
    try:
        ctypes.CDLL("libpord.so", mode=ctypes.RTLD_GLOBAL)
    except OSError:
        pass  # Optional dependency

    try:
        ctypes.CDLL("libmpiseq.so", mode=ctypes.RTLD_GLOBAL)
    except OSError:
        pass  # May use system MPI instead

    # Load libmumps_common first (provides common MUMPS functions)
    ctypes.CDLL("libmumps_common.so", mode=ctypes.RTLD_GLOBAL)

    # Load libdmumps last (has our C interface symbols, depends on libmumps_common)
    _mumps_lib = ctypes.CDLL("libdmumps.so", mode=ctypes.RTLD_GLOBAL)

except OSError as e:
    project_root = Path(__file__).parent.parent.parent
    lib_dir = project_root / "lib"
    pord_dir = project_root / "src" / "PORD" / "lib"
    raise ImportError(
        f"Failed to load MUMPS libraries: {e}\n\n"
        "Please ensure MUMPS is built with shared libraries and set LD_LIBRARY_PATH:\n"
        f"  export LD_LIBRARY_PATH={lib_dir}:{pord_dir}:$LD_LIBRARY_PATH\n\n"
        "For vendor-specific testing, use:\n"
        f"  export LD_LIBRARY_PATH={lib_dir}/vendors/<vendor>:{pord_dir}:$LD_LIBRARY_PATH\n\n"
        "Or run with the scripts/test_all_vendors.sh script."
    )


# Define opaque handle structure
class MUMPSHandle(ctypes.Structure):
    """Opaque handle to MUMPS internal structure."""
    _fields_ = [("ptr", ctypes.c_void_p)]


# Define function signatures
_mumps_lib.mumps_initialize.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
_mumps_lib.mumps_initialize.restype = MUMPSHandle

_mumps_lib.mumps_set_matrix.argtypes = [
    MUMPSHandle,
    ctypes.c_int,
    ndpointer(ctypes.c_int, flags="C_CONTIGUOUS"),
    ndpointer(ctypes.c_int, flags="C_CONTIGUOUS"),
    ndpointer(ctypes.c_double, flags="C_CONTIGUOUS"),
]
_mumps_lib.mumps_set_matrix.restype = None

_mumps_lib.mumps_set_rhs.argtypes = [
    MUMPSHandle,
    ndpointer(ctypes.c_double, flags="C_CONTIGUOUS"),
]
_mumps_lib.mumps_set_rhs.restype = None

_mumps_lib.mumps_get_solution.argtypes = [
    MUMPSHandle,
    ndpointer(ctypes.c_double, flags="C_CONTIGUOUS"),
]
_mumps_lib.mumps_get_solution.restype = None

_mumps_lib.mumps_analyze.argtypes = [MUMPSHandle]
_mumps_lib.mumps_analyze.restype = None

_mumps_lib.mumps_factorize.argtypes = [MUMPSHandle]
_mumps_lib.mumps_factorize.restype = None

_mumps_lib.mumps_solve.argtypes = [MUMPSHandle]
_mumps_lib.mumps_solve.restype = None

_mumps_lib.mumps_finalize.argtypes = [MUMPSHandle]
_mumps_lib.mumps_finalize.restype = None

_mumps_lib.mumps_get_info.argtypes = [MUMPSHandle, ctypes.c_int]
_mumps_lib.mumps_get_info.restype = ctypes.c_int

_mumps_lib.mumps_set_icntl.argtypes = [MUMPSHandle, ctypes.c_int, ctypes.c_int]
_mumps_lib.mumps_set_icntl.restype = None

_mumps_lib.mumps_get_icntl.argtypes = [MUMPSHandle, ctypes.c_int]
_mumps_lib.mumps_get_icntl.restype = ctypes.c_int

_mumps_lib.mumps_set_cntl.argtypes = [MUMPSHandle, ctypes.c_int, ctypes.c_double]
_mumps_lib.mumps_set_cntl.restype = None

_mumps_lib.mumps_get_rinfo.argtypes = [MUMPSHandle, ctypes.c_int]
_mumps_lib.mumps_get_rinfo.restype = ctypes.c_double


class MUMPS:
    """Python interface to MUMPS sparse direct solver.

    This class provides a simplified interface to MUMPS for solving sparse linear systems.

    Parameters
    ----------
    n : int
        Matrix dimension
    sym : int, optional
        Symmetry mode:
        - 0: Unsymmetric (default)
        - 1: Symmetric positive definite (SPD)
        - 2: General symmetric
    par : int, optional
        Parallel mode (default=1, host working)
    comm : int, optional
        MPI communicator (default=0, sequential mode)

    Examples
    --------
    >>> import numpy as np
    >>> from pymumps import MUMPS
    >>>
    >>> # Solve a simple 2x2 system
    >>> with MUMPS(n=2) as mumps:
    ...     irn = np.array([1, 1, 2, 2], dtype=np.int32)
    ...     jcn = np.array([1, 2, 1, 2], dtype=np.int32)
    ...     a = np.array([2.0, 3.0, 3.0, 2.0])
    ...     mumps.set_matrix(irn, jcn, a)
    ...     mumps.set_rhs(np.array([8.0, 13.0]))
    ...     mumps.analyze()
    ...     mumps.factorize()
    ...     mumps.solve()
    ...     sol = mumps.get_solution()
    >>> print(sol)  # Should be approximately [4.6, -0.4]
    """

    def __init__(self, n, sym=0, par=1, comm=0):
        """Initialize MUMPS instance."""
        self.n = n
        self.sym = sym
        self.par = par
        self.comm = comm
        self._finalized = False  # Set early in case __init__ fails
        self.handle = _mumps_lib.mumps_initialize(n, sym, par, comm)
        self._check_error()

    def set_matrix(self, irn, jcn, a):
        """Set matrix in COO format (1-indexed).

        For symmetric matrices (sym > 0), MUMPS requires only the upper triangle.
        If the full matrix is provided, it will be automatically filtered.

        Parameters
        ----------
        irn : array_like
            Row indices (1-indexed)
        jcn : array_like
            Column indices (1-indexed)
        a : array_like
            Matrix values
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        irn = np.asarray(irn, dtype=np.int32)
        jcn = np.asarray(jcn, dtype=np.int32)
        a = np.asarray(a, dtype=np.float64)

        if not (len(irn) == len(jcn) == len(a)):
            raise ValueError("irn, jcn, and a must have the same length")

        # For symmetric matrices, MUMPS only needs upper triangle
        if self.sym > 0:
            # Filter to upper triangle only (row <= col)
            mask = irn <= jcn
            irn = irn[mask]
            jcn = jcn[mask]
            a = a[mask]

        nz = len(a)
        _mumps_lib.mumps_set_matrix(self.handle, nz, irn, jcn, a)
        self._check_error()

    def set_rhs(self, rhs):
        """Set right-hand side vector.

        Parameters
        ----------
        rhs : array_like
            Right-hand side vector (length n)
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        rhs = np.asarray(rhs, dtype=np.float64)
        if len(rhs) != self.n:
            raise ValueError(f"RHS must have length {self.n}, got {len(rhs)}")

        _mumps_lib.mumps_set_rhs(self.handle, rhs)

    def analyze(self):
        """Perform symbolic factorization (analysis phase)."""
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        _mumps_lib.mumps_analyze(self.handle)
        self._check_error()

    def factorize(self):
        """Perform numerical factorization."""
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        _mumps_lib.mumps_factorize(self.handle)
        self._check_error()

    def solve(self):
        """Solve the linear system."""
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        _mumps_lib.mumps_solve(self.handle)
        self._check_error()

    def get_solution(self):
        """Get solution vector.

        Returns
        -------
        sol : ndarray
            Solution vector (length n)
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        sol = np.zeros(self.n, dtype=np.float64)
        _mumps_lib.mumps_get_solution(self.handle, sol)
        return sol

    def set_icntl(self, idx, val):
        """Set integer control parameter.

        Parameters
        ----------
        idx : int
            Parameter index (1-60)
        val : int
            Parameter value
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        _mumps_lib.mumps_set_icntl(self.handle, idx, val)

    def get_icntl(self, idx):
        """Get integer control parameter.

        Parameters
        ----------
        idx : int
            Parameter index (1-60)

        Returns
        -------
        val : int
            Parameter value
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        return _mumps_lib.mumps_get_icntl(self.handle, idx)

    def set_cntl(self, idx, val):
        """Set real control parameter.

        Parameters
        ----------
        idx : int
            Parameter index (1-15)
        val : float
            Parameter value
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        _mumps_lib.mumps_set_cntl(self.handle, idx, val)

    def get_info(self, idx):
        """Get info parameter.

        Parameters
        ----------
        idx : int
            Info index (1-80)

        Returns
        -------
        val : int
            Info value
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        return _mumps_lib.mumps_get_info(self.handle, idx)

    def get_rinfo(self, idx):
        """Get real info parameter.

        Parameters
        ----------
        idx : int
            RINFO index (1-40)

        Returns
        -------
        val : float
            RINFO value
        """
        if self._finalized:
            raise RuntimeError("MUMPS instance has been finalized")

        return _mumps_lib.mumps_get_rinfo(self.handle, idx)

    def _check_error(self):
        """Check for MUMPS errors."""
        info = self.get_info(1)
        if info < 0:
            error_messages = {
                -1: "Error in MUMPS (index out of range on processor)",
                -2: "NZ is out of range",
                -3: "MUMPS called with invalid JOB",
                -4: "Error in user-provided permutation array",
                -5: "Problem of real workspace allocation",
                -6: "Matrix is singular in structure",
                -7: "Problem of integer workspace allocation",
                -8: "Integer workarray too small",
                -9: "Real workarray too small",
                -10: "Numerically singular matrix",
                -11: "Error in internal integer array",
                -12: "Error in internal real array",
                -13: "Problem allocating memory within MUMPS",
                -14: "Integer workarray is too small for in-core",
                -15: "Integer workarray is too small for out-of-core",
                -16: "N is out of range",
                -17: "Internal send buffer too small",
                -20: "Internal reception buffer too small",
                -21: "Value of PAR is inconsistent",
                -22: "A pointer to an array is associated with a pointer to an array of incompatible size",
                -23: "MPI was not initialized",
                -24: "NELT is out of range",
                -25: "Problem with I/O",
                -26: "Problem of LRHS allocation",
                -27: "Problem with ICNTL",
                -28: "Problem with ordering",
                -29: "Problem with INFOG or INFO",
                -30: "Problem with KEEP",
                -31: "Problem with SYM",
                -32: "Problem with out-of-core files",
                -33: "Problem with NULL space computation",
                -34: "Problem with forward elimination in solve phase",
                -35: "Incompatible values in ICNTL(28) and ICNTL(5) and/or ICNTL(19) and/or ICNTL(6)",
                -36: "Problem with Schur complement",
                -37: "Problem with distributed input matrix",
                -38: "Incompatible input matrix format in parallel",
                -39: "Problem with coupling between parallel factorization and solve",
                -40: "Matrix is singular",
                -41: "Problem with extended Schur complement",
                -42: "Problem with permutation arrays",
                -43: "Incompatible SYM and number of rows",
                -44: "Problem with data distribution with centralized input",
                -45: "Problem with MPI during analysis",
                -46: "Problem with MPI during factorization",
                -47: "Problem with MPI during solution",
            }
            msg = error_messages.get(info, f"Unknown error code: {info}")
            raise RuntimeError(f"MUMPS error (INFO[1]={info}): {msg}")

    def finalize(self):
        """Clean up MUMPS instance."""
        if not self._finalized and self.handle.ptr is not None:
            _mumps_lib.mumps_finalize(self.handle)
            self.handle.ptr = None
            self._finalized = True

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, *args):
        """Context manager exit."""
        self.finalize()

    def __del__(self):
        """Destructor."""
        self.finalize()


def solve_system(A, b, sym=0, **kwargs):
    """Convenience function to solve Ax=b using MUMPS.

    Parameters
    ----------
    A : scipy.sparse matrix
        Sparse matrix in any scipy.sparse format
    b : array_like
        Right-hand side vector
    sym : int, optional
        Symmetry mode (0=unsymmetric, 1=SPD, 2=general symmetric)
    **kwargs : dict
        Additional arguments passed to MUMPS constructor

    Returns
    -------
    x : ndarray
        Solution vector

    Examples
    --------
    >>> import numpy as np
    >>> import scipy.sparse as sp
    >>> from pymumps import solve_system
    >>>
    >>> A = sp.eye(5, format='coo')
    >>> b = np.array([1, 2, 3, 4, 5])
    >>> x = solve_system(A, b)
    >>> print(x)  # Should be [1, 2, 3, 4, 5]
    """
    import scipy.sparse as sp

    # Convert to COO format
    A_coo = sp.coo_matrix(A)

    n = A_coo.shape[0]
    if A_coo.shape[0] != A_coo.shape[1]:
        raise ValueError(f"Matrix must be square, got shape {A_coo.shape}")
    if len(b) != n:
        raise ValueError(f"RHS dimension mismatch: A is {n}x{n}, b has length {len(b)}")

    # For symmetric matrices, MUMPS only needs upper triangle
    if sym > 0:
        # Filter to upper triangle only (row <= col)
        mask = A_coo.row <= A_coo.col
        irn = A_coo.row[mask]
        jcn = A_coo.col[mask]
        a = A_coo.data[mask]
    else:
        # Use all entries for unsymmetric
        irn = A_coo.row
        jcn = A_coo.col
        a = A_coo.data

    with MUMPS(n=n, sym=sym, **kwargs) as mumps:
        # Convert to 1-indexed (Fortran convention)
        mumps.set_matrix(irn + 1, jcn + 1, a)
        mumps.set_rhs(b)
        mumps.analyze()
        mumps.factorize()
        mumps.solve()
        return mumps.get_solution()
