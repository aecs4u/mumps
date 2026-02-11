"""MUMPS CLI - Command-line interface for building, testing, and benchmarking.

Usage:
    python -m cli.mumps_cli [COMMAND] [OPTIONS]
    mumps [COMMAND] [OPTIONS]     (if installed via pyproject.toml)
"""

from __future__ import annotations

import os
import signal
import subprocess
import sys
from pathlib import Path

import typer
from rich.console import Console
from rich.panel import Panel
from rich.table import Table

app = typer.Typer(
    name="mumps",
    help="MUMPS Sparse Direct Solver - Build, Test & Benchmark CLI",
    no_args_is_help=True,
    rich_markup_mode="rich",
)

build_app = typer.Typer(help="Build MUMPS libraries, examples, and benchmarks")
test_app = typer.Typer(help="Run tests (pytest, vendor, Fortran, correctness)")
benchmark_app = typer.Typer(help="Run performance benchmarks")
vendor_app = typer.Typer(help="BLAS vendor detection and management")
webapp_app = typer.Typer(help="Start/stop the webapp dashboard")
info_app = typer.Typer(help="System and build configuration info")

app.add_typer(build_app, name="build")
app.add_typer(test_app, name="test")
app.add_typer(benchmark_app, name="benchmark")
app.add_typer(vendor_app, name="vendor")
app.add_typer(webapp_app, name="webapp")
app.add_typer(info_app, name="info")

console = Console()
PROJECT_ROOT = Path(__file__).resolve().parents[1]


# ─── Build Commands ───────────────────────────────────────────

@build_app.command("library")
def build_library(
    precision: str = typer.Option("d", "-p", "--precision", help="Precision: s, d, c, z"),
    shared: bool = typer.Option(True, "--shared/--static", help="Build shared libraries"),
    blas_vendor: str = typer.Option("", "--blas", help="BLAS vendor (auto-detect if empty)"),
):
    """Build MUMPS library for a specific precision."""
    from cli._runner import run_make

    target = f"{precision}{'shared' if shared else ''}"
    make_args = []
    if blas_vendor:
        from cli.lib_detect import detect_vendor
        lib = detect_vendor(blas_vendor)
        if lib:
            make_args.append(f'LIBBLAS={lib.link_flags}')
        else:
            console.print(f"[red]BLAS vendor '{blas_vendor}' not found[/red]")
            raise typer.Exit(1)

    console.print(Panel(f"Building MUMPS {precision.upper()} ({'shared' if shared else 'static'})", style="blue"))
    rc = run_make([target], make_args=make_args)
    raise typer.Exit(rc)


@build_app.command("all")
def build_all(
    shared: bool = typer.Option(True, "--shared/--static", help="Build shared libraries"),
):
    """Build all 4 MUMPS precisions (S/D/C/Z)."""
    from cli._runner import run_make

    target = "allshared" if shared else "all"
    console.print(Panel(f"Building all MUMPS precisions ({'shared' if shared else 'static'})", style="blue"))
    rc = run_make([target])
    raise typer.Exit(rc)


@build_app.command("examples")
def build_examples(
    precision: str = typer.Option("d", "-p", "--precision", help="Precision: s, d, c, z"),
):
    """Build MUMPS examples for a specific precision."""
    from cli._runner import run_script

    console.print(Panel(f"Building examples for precision {precision.upper()}", style="blue"))
    rc = run_script("smart_build.sh", args=["examples", precision])
    raise typer.Exit(rc)


@build_app.command("benchmarks")
def build_benchmarks(
    precision: str = typer.Option("d", "-p", "--precision", help="Precision: s, d, c, z"),
):
    """Build MUMPS benchmarks for a specific precision."""
    from cli._runner import run_script

    console.print(Panel(f"Building benchmarks for precision {precision.upper()}", style="blue"))
    rc = run_script("smart_build.sh", args=["benchmarks", precision])
    raise typer.Exit(rc)


# ─── Test Commands ────────────────────────────────────────────

@test_app.command("pytest")
def test_pytest(
    verbose: bool = typer.Option(True, "-v", "--verbose/--quiet"),
    markers: str = typer.Option("", "-m", "--markers", help="pytest marker expression"),
):
    """Run Python test suite with pytest."""
    from cli._runner import run_command

    cmd = [sys.executable, "-m", "pytest", "tests/", "--tb=short"]
    if verbose:
        cmd.append("-v")
    if markers:
        cmd.extend(["-m", markers])

    console.print(Panel("Running pytest test suite", style="green"))
    rc = run_command(cmd)
    raise typer.Exit(rc)


@test_app.command("vendors")
def test_vendors():
    """Run tests across all available BLAS vendors."""
    from cli._runner import run_script

    console.print(Panel("Testing all BLAS vendors", style="green"))
    rc = run_script("scripts/test_all_vendors.sh")
    raise typer.Exit(rc)


@test_app.command("fortran")
def test_fortran():
    """Run Fortran example test suite."""
    from cli._runner import run_script

    console.print(Panel("Running Fortran example tests", style="green"))
    rc = run_script("examples/run_all_tests.sh")
    raise typer.Exit(rc)


@test_app.command("correctness")
def test_correctness():
    """Run BLAS correctness validation tests."""
    from cli._runner import run_script

    console.print(Panel("Running BLAS correctness checks", style="green"))
    rc = run_script("scripts/check_blas_correctness.sh")
    raise typer.Exit(rc)


# ─── Benchmark Commands ──────────────────────────────────────

@benchmark_app.command("sparse")
def benchmark_sparse():
    """Run sparse MUMPS solver benchmark (JOB=6)."""
    from cli._runner import run_script

    console.print(Panel("Running sparse MUMPS benchmark", style="magenta"))
    rc = run_script("scripts/benchmark_blas.sh")
    raise typer.Exit(rc)


@benchmark_app.command("dense")
def benchmark_dense():
    """Run dense GEMM benchmark."""
    from cli._runner import run_script

    console.print(Panel("Running dense GEMM benchmark", style="magenta"))
    rc = run_script("scripts/benchmark_blas_dense.sh")
    raise typer.Exit(rc)


@benchmark_app.command("all")
def benchmark_all():
    """Run complete benchmark suite and store results in SQLite."""
    from cli._runner import run_script

    console.print(Panel("Running full benchmark suite → SQLite", style="magenta"))
    rc = run_script("scripts/run_all_benchmarks_to_sqlite.sh")
    raise typer.Exit(rc)


# ─── Vendor Commands ─────────────────────────────────────────

@vendor_app.command("detect")
def vendor_detect():
    """Detect all available BLAS libraries on this system."""
    from cli.lib_detect import detect_all, detect_from_cmake_cache

    console.print(Panel("BLAS Library Detection", style="purple"))

    libs = detect_all()
    if not libs:
        console.print("[yellow]No BLAS libraries detected.[/yellow]")
        raise typer.Exit(1)

    table = Table(title="Detected BLAS Libraries")
    table.add_column("Vendor", style="bold")
    table.add_column("Link Flags")
    table.add_column("Detection Method", style="cyan")
    table.add_column("Version")
    table.add_column("Path", style="dim")

    for vendor, lib in libs.items():
        table.add_row(vendor, lib.link_flags, lib.detection_method, lib.version or "-", lib.path)

    console.print(table)

    # Check CMake cache
    cmake = detect_from_cmake_cache(PROJECT_ROOT / "build")
    if cmake:
        console.print(f"\n[dim]CMake cache: vendor={cmake.vendor}, flags={cmake.link_flags}[/dim]")


@vendor_app.command("status")
def vendor_status():
    """Show vendor library build status."""
    from cli._runner import run_make

    console.print(Panel("Vendor Library Status", style="purple"))
    rc = run_make(["vendor-status"])
    raise typer.Exit(rc)


@vendor_app.command("rebuild")
def vendor_rebuild(
    force: bool = typer.Option(False, "--force", "-f", help="Rebuild even if libraries exist"),
):
    """Rebuild vendor-specific MUMPS libraries (skips existing unless --force)."""
    from cli._runner import run_script

    label = "Force-rebuilding" if force else "Building missing"
    console.print(Panel(f"{label} vendor libraries", style="purple"))
    args = ["--force"] if force else []
    rc = run_script("scripts/rebuild_vendor_libraries.sh", args=args)
    raise typer.Exit(rc)


# ─── Webapp Commands ─────────────────────────────────────────

@webapp_app.command("start")
def webapp_start(
    port: int = typer.Option(9002, "--port", help="Port number"),
    host: str = typer.Option("127.0.0.1", "--host", help="Host to bind to"),
    reload: bool = typer.Option(False, "--reload", help="Enable auto-reload"),
):
    """Start the webapp dashboard."""
    from cli._runner import run_command

    console.print(Panel(f"Starting webapp at http://{host}:{port}", style="blue"))
    cmd = [
        sys.executable, "-m", "uvicorn", "webapp.main:app",
        "--host", host, "--port", str(port),
    ]
    if reload:
        cmd.append("--reload")
    rc = run_command(cmd)
    raise typer.Exit(rc)


@webapp_app.command("stop")
def webapp_stop():
    """Stop running webapp instances."""
    import subprocess as sp
    result = sp.run(
        ["pkill", "-f", "webapp.main:app"],
        capture_output=True, text=True
    )
    if result.returncode == 0:
        console.print("[green]Webapp stopped[/green]")
    else:
        console.print("[yellow]No running webapp found[/yellow]")


# ─── Info Commands ────────────────────────────────────────────

@info_app.command("config")
def info_config():
    """Show current build configuration."""
    from cli._runner import run_make

    console.print(Panel("Build Configuration", style="blue"))

    # Read Makefile.inc for key settings
    makefile_inc = PROJECT_ROOT / "Makefile.inc"
    if makefile_inc.exists():
        table = Table(title="Makefile.inc Settings")
        table.add_column("Variable", style="bold")
        table.add_column("Value")

        for line in makefile_inc.read_text().splitlines():
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, _, value = line.partition("=")
                key = key.strip()
                value = value.strip()
                if key in {"FC", "CC", "FL", "LIBBLAS", "OPTF", "OPTC", "OPTL",
                           "CDEFS", "LMETISDIR", "IMETIS", "LMETIS",
                           "ORDERINGSF", "ORDERINGSC"}:
                    table.add_row(key, value)
        console.print(table)
    else:
        console.print("[yellow]Makefile.inc not found[/yellow]")


@info_app.command("system")
def info_system():
    """Show system information relevant to MUMPS."""
    import platform

    console.print(Panel("System Information", style="blue"))

    table = Table()
    table.add_column("Property", style="bold")
    table.add_column("Value")

    table.add_row("Platform", platform.platform())
    table.add_row("Architecture", platform.machine())
    table.add_row("Python", platform.python_version())
    table.add_row("Processor", platform.processor() or "N/A")

    # CPU info
    try:
        nproc = os.cpu_count()
        table.add_row("CPU Cores", str(nproc))
    except Exception:
        pass

    # Memory
    try:
        with open("/proc/meminfo") as f:
            for line in f:
                if line.startswith("MemTotal"):
                    mem_kb = int(line.split()[1])
                    table.add_row("Memory", f"{mem_kb / 1024 / 1024:.1f} GB")
                    break
    except (OSError, ValueError):
        pass

    # Fortran compiler
    for fc in ["gfortran", "ifort", "ifx"]:
        try:
            result = subprocess.run([fc, "--version"], capture_output=True, text=True, timeout=3)
            if result.returncode == 0:
                version = result.stdout.splitlines()[0]
                table.add_row(f"Fortran ({fc})", version)
        except (FileNotFoundError, subprocess.TimeoutExpired):
            pass

    # C compiler
    for cc in ["gcc", "icc", "icx", "clang"]:
        try:
            result = subprocess.run([cc, "--version"], capture_output=True, text=True, timeout=3)
            if result.returncode == 0:
                version = result.stdout.splitlines()[0]
                table.add_row(f"C ({cc})", version)
        except (FileNotFoundError, subprocess.TimeoutExpired):
            pass

    console.print(table)

    # BLAS detection summary
    from cli.lib_detect import detect_all
    libs = detect_all()
    if libs:
        blas_table = Table(title="BLAS Libraries")
        blas_table.add_column("Vendor", style="bold")
        blas_table.add_column("Method", style="cyan")
        blas_table.add_column("Flags")
        for name, lib in libs.items():
            blas_table.add_row(name, lib.detection_method, lib.link_flags)
        console.print(blas_table)


@info_app.command("blas")
def info_blas():
    """Detect and display BLAS library information."""
    vendor_detect()


# ─── Clean Command ────────────────────────────────────────────

@app.command("clean")
def clean():
    """Clean all build artifacts."""
    from cli._runner import run_make

    console.print(Panel("Cleaning build artifacts", style="red"))
    rc = run_make(["clean"])
    raise typer.Exit(rc)


# ─── Main ─────────────────────────────────────────────────────

def main():
    app()


if __name__ == "__main__":
    main()
