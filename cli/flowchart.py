"""Generate SVG flowchart of MUMPS CLI use cases using Graphviz."""

from __future__ import annotations

import graphviz


def generate_flowchart_svg() -> str:
    """Generate an SVG flowchart showing all MUMPS CLI use cases.

    Returns the SVG content as a string.
    """
    dot = graphviz.Digraph(
        "mumps_cli",
        format="svg",
        graph_attr={
            "rankdir": "TB",
            "fontname": "Helvetica",
            "fontsize": "14",
            "bgcolor": "transparent",
            "pad": "0.5",
            "nodesep": "0.6",
            "ranksep": "0.8",
        },
        node_attr={
            "fontname": "Helvetica",
            "fontsize": "11",
            "style": "filled,rounded",
            "shape": "box",
            "penwidth": "1.5",
        },
        edge_attr={
            "fontname": "Helvetica",
            "fontsize": "9",
            "color": "#6b7280",
        },
    )

    # ── Start node ──
    dot.node("start", "Start", shape="ellipse", fillcolor="#667eea", fontcolor="white")

    # ── Decision node ──
    dot.node(
        "decide",
        "What do you need?",
        shape="diamond",
        fillcolor="#fef3c7",
        fontcolor="#92400e",
        width="2",
    )
    dot.edge("start", "decide")

    # ── BUILD branch ──
    dot.node("build", "Build", fillcolor="#dbeafe", fontcolor="#1e40af")
    dot.edge("decide", "build", label="build")

    dot.node("build_lib", "mumps build library\n-p d --shared", fillcolor="#eff6ff", fontcolor="#1e40af")
    dot.node("build_all", "mumps build all\n--shared", fillcolor="#eff6ff", fontcolor="#1e40af")
    dot.node("build_ex", "mumps build examples\n-p d", fillcolor="#eff6ff", fontcolor="#1e40af")
    dot.node("build_bench", "mumps build benchmarks\n-p d", fillcolor="#eff6ff", fontcolor="#1e40af")

    dot.edge("build", "build_lib")
    dot.edge("build", "build_all")
    dot.edge("build", "build_ex")
    dot.edge("build", "build_bench")

    # ── TEST branch ──
    dot.node("test", "Test", fillcolor="#dcfce7", fontcolor="#166534")
    dot.edge("decide", "test", label="test")

    dot.node("test_pytest", "mumps test pytest", fillcolor="#f0fdf4", fontcolor="#166534")
    dot.node("test_vendors", "mumps test vendors", fillcolor="#f0fdf4", fontcolor="#166534")
    dot.node("test_fortran", "mumps test fortran", fillcolor="#f0fdf4", fontcolor="#166534")
    dot.node("test_correct", "mumps test correctness", fillcolor="#f0fdf4", fontcolor="#166534")

    dot.edge("test", "test_pytest")
    dot.edge("test", "test_vendors")
    dot.edge("test", "test_fortran")
    dot.edge("test", "test_correct")

    # ── BENCHMARK branch ──
    dot.node("benchmark", "Benchmark", fillcolor="#fce7f3", fontcolor="#831843")
    dot.edge("decide", "benchmark", label="benchmark")

    dot.node("bench_sparse", "mumps benchmark sparse", fillcolor="#fdf2f8", fontcolor="#831843")
    dot.node("bench_dense", "mumps benchmark dense", fillcolor="#fdf2f8", fontcolor="#831843")
    dot.node("bench_all", "mumps benchmark all", fillcolor="#fdf2f8", fontcolor="#831843")

    dot.edge("benchmark", "bench_sparse")
    dot.edge("benchmark", "bench_dense")
    dot.edge("benchmark", "bench_all")

    # ── SQLite + Webapp flow ──
    dot.node("sqlite", "SQLite DB", shape="cylinder", fillcolor="#e0e7ff", fontcolor="#3730a3")
    dot.node("webapp", "Webapp Dashboard", fillcolor="#667eea", fontcolor="white")

    dot.edge("bench_all", "sqlite", label="stores results")
    dot.edge("test_pytest", "sqlite", label="stores results", style="dashed")
    dot.edge("sqlite", "webapp", label="feeds data", style="dashed")

    # ── VENDOR branch ──
    dot.node("vendor", "Vendor", fillcolor="#ddd6fe", fontcolor="#5b21b6")
    dot.edge("decide", "vendor", label="vendor")

    dot.node("vendor_detect", "mumps vendor detect", fillcolor="#ede9fe", fontcolor="#5b21b6")
    dot.node("vendor_status", "mumps vendor status", fillcolor="#ede9fe", fontcolor="#5b21b6")
    dot.node("vendor_rebuild", "mumps vendor rebuild", fillcolor="#ede9fe", fontcolor="#5b21b6")

    dot.edge("vendor", "vendor_detect")
    dot.edge("vendor", "vendor_status")
    dot.edge("vendor", "vendor_rebuild")

    # ── INFO branch ──
    dot.node("info", "Info", fillcolor="#f3f4f6", fontcolor="#374151")
    dot.edge("decide", "info", label="info")

    dot.node("info_config", "mumps info config", fillcolor="#f9fafb", fontcolor="#374151")
    dot.node("info_system", "mumps info system", fillcolor="#f9fafb", fontcolor="#374151")

    dot.edge("info", "info_config")
    dot.edge("info", "info_system")

    # ── WEBAPP branch ──
    dot.node("webapp_cmd", "Webapp", fillcolor="#667eea", fontcolor="white")
    dot.edge("decide", "webapp_cmd", label="webapp")

    dot.node("webapp_start", "mumps webapp start\n--port 9001", fillcolor="#818cf8", fontcolor="white")
    dot.node("webapp_stop", "mumps webapp stop", fillcolor="#818cf8", fontcolor="white")

    dot.edge("webapp_cmd", "webapp_start")
    dot.edge("webapp_cmd", "webapp_stop")

    # ── Cross-links (dashed) ──
    dot.edge("build_lib", "test", style="dashed", label="after build", color="#9ca3af")
    dot.edge("build_bench", "benchmark", style="dashed", label="after build", color="#9ca3af")
    dot.edge("vendor_rebuild", "build", style="dashed", label="triggers", color="#9ca3af")

    # ── CLEAN ──
    dot.node("clean", "mumps clean", fillcolor="#fef2f2", fontcolor="#991b1b")
    dot.edge("decide", "clean", label="clean")

    return dot.pipe(encoding="utf-8")


if __name__ == "__main__":
    svg = generate_flowchart_svg()
    print(svg)
