"""Dynamic BLAS library detection for MUMPS.

Detects installed BLAS libraries using multiple methods in priority order:
1. pkg-config
2. ldconfig cache
3. Environment variables
4. Filesystem search
"""

from __future__ import annotations

import os
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class BLASLibrary:
    """Detected BLAS library information."""

    vendor: str
    path: str
    link_flags: str
    detection_method: str
    version: str = ""
    details: dict[str, str] = field(default_factory=dict)


def _run_cmd(cmd: list[str], timeout: int = 5) -> tuple[int, str, str]:
    """Run a command and return (returncode, stdout, stderr)."""
    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout
        )
        return result.returncode, result.stdout.strip(), result.stderr.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        return -1, "", ""


def _has_pkg_config() -> bool:
    return shutil.which("pkg-config") is not None


def _has_ldconfig() -> bool:
    return shutil.which("ldconfig") is not None


def _pkg_config_check(package: str) -> str | None:
    """Check if a pkg-config package exists and return its --libs output."""
    if not _has_pkg_config():
        return None
    rc, stdout, _ = _run_cmd(["pkg-config", "--libs", package])
    if rc == 0 and stdout:
        return stdout
    return None


def _ldconfig_find(library_name: str) -> str | None:
    """Search ldconfig cache for a library and return its path."""
    if not _has_ldconfig():
        return None
    rc, stdout, _ = _run_cmd(["ldconfig", "-p"])
    if rc != 0:
        return None
    for line in stdout.splitlines():
        if library_name in line and "=>" in line:
            path = line.split("=>")[-1].strip()
            if os.path.isfile(path):
                return path
    return None


def _find_library(name: str, search_paths: list[str]) -> str | None:
    """Search filesystem paths for a library file."""
    import glob

    for pattern in search_paths:
        # Support glob patterns
        matches = glob.glob(pattern)
        for match in matches:
            if os.path.isfile(match):
                return match
        # Also try direct path + name
        if os.path.isdir(pattern):
            candidate = os.path.join(pattern, name)
            if os.path.isfile(candidate):
                return candidate
    return None


# ---------------------------------------------------------------------------
# MKL Detection
# ---------------------------------------------------------------------------

_MKL_PKG_CONFIG_NAMES = [
    "mkl-dynamic-lp64-seq",
    "mkl-dynamic-ilp64-seq",
    "mkl-sdl",
]

_MKL_FILESYSTEM_PATHS = [
    "/opt/intel/oneapi/mkl/latest/lib",
    "/opt/intel/oneapi/mkl/*/lib",
    "/opt/intel/oneapi/mkl/*/lib/intel64",
    "/opt/intel/mkl/lib/intel64",
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib64",
]


def detect_mkl() -> BLASLibrary | None:
    """Detect Intel MKL using pkg-config, ldconfig, env, and filesystem."""
    # 1. pkg-config
    for pkg in _MKL_PKG_CONFIG_NAMES:
        flags = _pkg_config_check(pkg)
        if flags:
            # Get version
            _, version, _ = _run_cmd(["pkg-config", "--modversion", pkg])
            return BLASLibrary(
                vendor="mkl",
                path=pkg,
                link_flags=flags,
                detection_method=f"pkg-config ({pkg})",
                version=version,
            )

    # 2. ldconfig
    ldconfig_path = _ldconfig_find("libmkl_rt.so")
    if ldconfig_path:
        lib_dir = os.path.dirname(ldconfig_path)
        return BLASLibrary(
            vendor="mkl",
            path=ldconfig_path,
            link_flags=f"-L{lib_dir} -lmkl_rt",
            detection_method="ldconfig",
        )

    # 3. Environment variable
    mklroot = os.environ.get("MKLROOT")
    if mklroot:
        for subdir in ["lib/intel64", "lib"]:
            lib_path = os.path.join(mklroot, subdir, "libmkl_rt.so")
            if os.path.isfile(lib_path):
                lib_dir = os.path.dirname(lib_path)
                return BLASLibrary(
                    vendor="mkl",
                    path=lib_path,
                    link_flags=f"-L{lib_dir} -lmkl_rt",
                    detection_method="MKLROOT environment",
                    details={"MKLROOT": mklroot},
                )

    # 4. Filesystem search
    lib_path = _find_library("libmkl_rt.so", _MKL_FILESYSTEM_PATHS)
    if lib_path:
        lib_dir = os.path.dirname(lib_path)
        return BLASLibrary(
            vendor="mkl",
            path=lib_path,
            link_flags=f"-L{lib_dir} -lmkl_rt",
            detection_method="filesystem",
        )

    return None


# ---------------------------------------------------------------------------
# OpenBLAS Detection
# ---------------------------------------------------------------------------

_OPENBLAS_PKG_CONFIG_NAMES = ["openblas", "openblas-pthread", "openblas-openmp"]

_OPENBLAS_FILESYSTEM_PATHS = [
    "/usr/lib/x86_64-linux-gnu/openblas-pthread",
    "/usr/lib/x86_64-linux-gnu/openblas-serial",
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib64",
    "/usr/lib",
    "/usr/local/lib",
]


def detect_openblas() -> BLASLibrary | None:
    """Detect OpenBLAS using pkg-config, ldconfig, and filesystem."""
    # 1. pkg-config
    for pkg in _OPENBLAS_PKG_CONFIG_NAMES:
        flags = _pkg_config_check(pkg)
        if flags:
            _, version, _ = _run_cmd(["pkg-config", "--modversion", pkg])
            return BLASLibrary(
                vendor="openblas",
                path=pkg,
                link_flags=flags,
                detection_method=f"pkg-config ({pkg})",
                version=version,
            )

    # 2. ldconfig
    ldconfig_path = _ldconfig_find("libopenblas.so")
    if ldconfig_path:
        lib_dir = os.path.dirname(ldconfig_path)
        return BLASLibrary(
            vendor="openblas",
            path=ldconfig_path,
            link_flags=f"-L{lib_dir} -lopenblas",
            detection_method="ldconfig",
        )

    # 3. Filesystem search
    lib_path = _find_library("libopenblas.so", _OPENBLAS_FILESYSTEM_PATHS)
    if lib_path:
        lib_dir = os.path.dirname(lib_path)
        return BLASLibrary(
            vendor="openblas",
            path=lib_path,
            link_flags=f"-L{lib_dir} -lopenblas",
            detection_method="filesystem",
        )

    return None


# ---------------------------------------------------------------------------
# BLIS Detection
# ---------------------------------------------------------------------------

_BLIS_PKG_CONFIG_NAMES = ["blis", "blis-mt", "blis-pthread"]

_BLIS_FILESYSTEM_PATHS = [
    "/usr/local/lib",
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib64",
    "/usr/lib",
]


def detect_blis() -> BLASLibrary | None:
    """Detect AMD BLIS using pkg-config, ldconfig, and filesystem."""
    # 1. pkg-config
    for pkg in _BLIS_PKG_CONFIG_NAMES:
        flags = _pkg_config_check(pkg)
        if flags:
            _, version, _ = _run_cmd(["pkg-config", "--modversion", pkg])
            return BLASLibrary(
                vendor="blis",
                path=pkg,
                link_flags=flags,
                detection_method=f"pkg-config ({pkg})",
                version=version,
            )

    # 2. ldconfig - prefer multi-threaded
    for lib_name in ["libblis-mt.so", "libblis.so"]:
        ldconfig_path = _ldconfig_find(lib_name)
        if ldconfig_path:
            lib_dir = os.path.dirname(ldconfig_path)
            lib_flag = "-lblis-mt" if "blis-mt" in ldconfig_path else "-lblis"
            return BLASLibrary(
                vendor="blis",
                path=ldconfig_path,
                link_flags=f"-L{lib_dir} {lib_flag}",
                detection_method="ldconfig",
            )

    # 3. Filesystem search - prefer multi-threaded
    for lib_name, lib_flag in [("libblis-mt.so", "-lblis-mt"), ("libblis.so", "-lblis")]:
        lib_path = _find_library(lib_name, _BLIS_FILESYSTEM_PATHS)
        if lib_path:
            lib_dir = os.path.dirname(lib_path)
            return BLASLibrary(
                vendor="blis",
                path=lib_path,
                link_flags=f"-L{lib_dir} {lib_flag}",
                detection_method="filesystem",
            )

    return None


# ---------------------------------------------------------------------------
# Reference BLAS Detection
# ---------------------------------------------------------------------------

_REFERENCE_PKG_CONFIG_NAMES = ["blas", "blas-netlib"]

_REFERENCE_FILESYSTEM_PATHS = [
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib64",
    "/usr/lib",
]


def detect_reference() -> BLASLibrary | None:
    """Detect reference BLAS/LAPACK using pkg-config, ldconfig, and filesystem."""
    # 1. pkg-config
    for pkg in _REFERENCE_PKG_CONFIG_NAMES:
        flags = _pkg_config_check(pkg)
        if flags:
            # Also need LAPACK
            lapack_flags = _pkg_config_check("lapack") or ""
            combined = f"{flags} {lapack_flags}".strip()
            _, version, _ = _run_cmd(["pkg-config", "--modversion", pkg])
            return BLASLibrary(
                vendor="reference",
                path=pkg,
                link_flags=combined,
                detection_method=f"pkg-config ({pkg})",
                version=version,
            )

    # 2. ldconfig
    blas_path = _ldconfig_find("libblas.so")
    lapack_path = _ldconfig_find("liblapack.so")
    if blas_path and lapack_path:
        blas_dir = os.path.dirname(blas_path)
        lapack_dir = os.path.dirname(lapack_path)
        if blas_dir == lapack_dir:
            flags = f"-L{blas_dir} -lblas -llapack"
        else:
            flags = f"-L{blas_dir} -lblas -L{lapack_dir} -llapack"
        return BLASLibrary(
            vendor="reference",
            path=blas_path,
            link_flags=flags,
            detection_method="ldconfig",
        )

    # 3. Filesystem search
    for path in _REFERENCE_FILESYSTEM_PATHS:
        blas = os.path.join(path, "libblas.so")
        lapack = os.path.join(path, "liblapack.so")
        if os.path.isfile(blas) and os.path.isfile(lapack):
            return BLASLibrary(
                vendor="reference",
                path=blas,
                link_flags=f"-L{path} -lblas -llapack",
                detection_method="filesystem",
            )

    return None


# ---------------------------------------------------------------------------
# CMake Cache Integration
# ---------------------------------------------------------------------------

def detect_from_cmake_cache(build_dir: str | Path = "build") -> BLASLibrary | None:
    """Extract BLAS configuration from CMake cache if available."""
    cache_file = Path(build_dir) / "CMakeCache.txt"
    if not cache_file.exists():
        return None

    blas_libs = ""
    blas_vendor = ""

    try:
        for line in cache_file.read_text().splitlines():
            if line.startswith("BLAS_LIBRARIES:") or line.startswith("BLAS_LIBRARIES="):
                blas_libs = line.split("=", 1)[1].strip()
            elif line.startswith("BLAS_VENDOR:") or line.startswith("BLA_VENDOR:"):
                blas_vendor = line.split("=", 1)[1].strip()
    except OSError:
        return None

    if not blas_libs:
        return None

    vendor = blas_vendor.lower() if blas_vendor else "cmake"

    return BLASLibrary(
        vendor=vendor,
        path=str(cache_file),
        link_flags=blas_libs,
        detection_method="cmake-cache",
        details={"build_dir": str(build_dir), "BLA_VENDOR": blas_vendor},
    )


# ---------------------------------------------------------------------------
# Main Detection
# ---------------------------------------------------------------------------

_DETECTORS = {
    "reference": detect_reference,
    "openblas": detect_openblas,
    "blis": detect_blis,
    "mkl": detect_mkl,
}


def detect_all() -> dict[str, BLASLibrary]:
    """Detect all available BLAS libraries.

    Returns a dict mapping vendor name to BLASLibrary info.
    """
    found: dict[str, BLASLibrary] = {}
    for vendor, detector in _DETECTORS.items():
        result = detector()
        if result is not None:
            found[vendor] = result
    return found


def detect_vendor(vendor: str) -> BLASLibrary | None:
    """Detect a specific BLAS vendor."""
    detector = _DETECTORS.get(vendor)
    if detector is None:
        return None
    return detector()


if __name__ == "__main__":
    # Quick test when run directly
    print("MUMPS BLAS Library Detection")
    print("=" * 50)

    libs = detect_all()
    if not libs:
        print("No BLAS libraries detected.")
    else:
        for vendor, lib in libs.items():
            print(f"\n{vendor}:")
            print(f"  Path:      {lib.path}")
            print(f"  Flags:     {lib.link_flags}")
            print(f"  Method:    {lib.detection_method}")
            if lib.version:
                print(f"  Version:   {lib.version}")
            if lib.details:
                for k, v in lib.details.items():
                    print(f"  {k}: {v}")

    # Check CMake cache
    cmake = detect_from_cmake_cache()
    if cmake:
        print(f"\nCMake cache:")
        print(f"  Vendor:  {cmake.vendor}")
        print(f"  Flags:   {cmake.link_flags}")
