"""Subprocess runner with Rich live output for MUMPS CLI."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path
from typing import Any

from rich.console import Console
from rich.live import Live
from rich.panel import Panel
from rich.text import Text

PROJECT_ROOT = Path(__file__).resolve().parents[1]
console = Console()


def _build_env(extra: dict[str, str] | None = None) -> dict[str, str]:
    """Build environment with LD_LIBRARY_PATH set for MUMPS."""
    env = os.environ.copy()
    lib_path = f"{PROJECT_ROOT}/lib:{PROJECT_ROOT}/src/PORD/lib"
    existing = env.get("LD_LIBRARY_PATH", "")
    env["LD_LIBRARY_PATH"] = f"{lib_path}:{existing}" if existing else lib_path
    if extra:
        env.update(extra)
    return env


def run_make(
    targets: list[str],
    extra_env: dict[str, str] | None = None,
    make_args: list[str] | None = None,
) -> int:
    """Run make with Rich live output.

    Returns the process exit code.
    """
    cmd = ["make"] + (make_args or []) + targets
    return run_command(cmd, cwd=str(PROJECT_ROOT), extra_env=extra_env)


def run_script(
    script_path: str | Path,
    args: list[str] | None = None,
    extra_env: dict[str, str] | None = None,
) -> int:
    """Run a shell script with Rich live output.

    Returns the process exit code.
    """
    path = Path(script_path)
    if not path.is_absolute():
        path = PROJECT_ROOT / path
    cmd = ["bash", str(path)] + (args or [])
    return run_command(cmd, cwd=str(PROJECT_ROOT), extra_env=extra_env)


def run_command(
    cmd: list[str],
    cwd: str | None = None,
    extra_env: dict[str, str] | None = None,
) -> int:
    """Run a command with streaming Rich output.

    Returns the process exit code.
    """
    env = _build_env(extra_env)
    cwd = cwd or str(PROJECT_ROOT)

    console.print(f"[dim]$ {' '.join(cmd)}[/dim]")

    try:
        proc = subprocess.Popen(
            cmd,
            cwd=cwd,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
        )

        output_lines: list[str] = []
        for line in proc.stdout:  # type: ignore[union-attr]
            line = line.rstrip("\n")
            output_lines.append(line)
            # Color-code output
            if "error" in line.lower() or "failed" in line.lower():
                console.print(f"[red]{line}[/red]")
            elif "warning" in line.lower():
                console.print(f"[yellow]{line}[/yellow]")
            elif "success" in line.lower() or line.startswith("✓") or line.startswith("✅"):
                console.print(f"[green]{line}[/green]")
            else:
                console.print(line)

        proc.wait()
        return proc.returncode

    except FileNotFoundError:
        console.print(f"[red]Command not found: {cmd[0]}[/red]")
        return 127
    except KeyboardInterrupt:
        console.print("\n[yellow]Interrupted[/yellow]")
        return 130
