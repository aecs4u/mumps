"""
Python interface to MUMPS sparse direct solver.

This module provides a ctypes-based interface to MUMPS using the iso_c_binding layer.
"""

from .mumps import MUMPS, solve_system

__version__ = "5.8.2"
__all__ = ["MUMPS", "solve_system"]
