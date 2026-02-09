#!/usr/bin/env python3
"""
Generate test matrix fixtures for MUMPS benchmarking.
Creates matrices in MUMPS coordinate format (.coo).
"""
import argparse
from pathlib import Path
import sys


def generate_diagonal(n: int, output: Path):
    """Generate n×n diagonal matrix with values 1..n on diagonal."""
    with open(output, 'w') as f:
        f.write(f"# Diagonal matrix {n}×{n}\n")
        f.write(f"# n nnz\n")
        f.write(f"{n} {n}\n")
        f.write(f"# i j value\n")
        for i in range(1, n + 1):
            f.write(f"{i} {i} {float(i)}\n")
    print(f"Generated diagonal matrix: {n}×{n}, nnz={n} -> {output}")


def generate_laplacian_2d(ngrid: int, output: Path):
    """Generate 2D 5-point Laplacian matrix on ngrid×ngrid grid."""
    n = ngrid * ngrid
    entries = []

    for i in range(ngrid):
        for j in range(ngrid):
            row = i * ngrid + j + 1  # 1-indexed
            # Diagonal element
            entries.append((row, row, 4.0))
            # Off-diagonal elements (5-point stencil)
            if i > 0:
                entries.append((row, row - ngrid, -1.0))
            if i < ngrid - 1:
                entries.append((row, row + ngrid, -1.0))
            if j > 0:
                entries.append((row, row - 1, -1.0))
            if j < ngrid - 1:
                entries.append((row, row + 1, -1.0))

    with open(output, 'w') as f:
        f.write(f"# 2D Laplacian {ngrid}×{ngrid} grid (5-point stencil)\n")
        f.write(f"# n nnz\n")
        f.write(f"{n} {len(entries)}\n")
        f.write(f"# i j value\n")
        for i, j, val in entries:
            f.write(f"{i} {j} {val}\n")

    print(f"Generated 2D Laplacian: grid={ngrid}×{ngrid}, n={n}, nnz={len(entries)} -> {output}")


def generate_tridiagonal(n: int, output: Path):
    """Generate n×n tridiagonal matrix."""
    entries = []

    for i in range(1, n + 1):
        # Diagonal
        entries.append((i, i, 2.0))
        # Sub-diagonal
        if i > 1:
            entries.append((i, i - 1, -1.0))
        # Super-diagonal
        if i < n:
            entries.append((i, i + 1, -1.0))

    with open(output, 'w') as f:
        f.write(f"# Tridiagonal matrix {n}×{n}\n")
        f.write(f"# n nnz\n")
        f.write(f"{n} {len(entries)}\n")
        f.write(f"# i j value\n")
        for i, j, val in entries:
            f.write(f"{i} {j} {val}\n")

    print(f"Generated tridiagonal matrix: {n}×{n}, nnz={len(entries)} -> {output}")


def main():
    parser = argparse.ArgumentParser(
        description="Generate test matrix fixtures for MUMPS",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Generate small test matrices
  %(prog)s --size small --output-dir benchmarks/matrices

  # Generate all sizes
  %(prog)s --size all --output-dir benchmarks/matrices

  # Generate specific matrix type
  %(prog)s --type laplacian --ngrid 100 --output test.coo
        """
    )

    parser.add_argument('--size', choices=['small', 'medium', 'large', 'huge', 'all'],
                        default='all', help='Matrix size category')
    parser.add_argument('--type', choices=['diagonal', 'laplacian', 'tridiagonal'],
                        help='Specific matrix type to generate')
    parser.add_argument('--ngrid', type=int, help='Grid size for Laplacian')
    parser.add_argument('--n', type=int, help='Matrix dimension')
    parser.add_argument('--output', type=Path, help='Output file path')
    parser.add_argument('--output-dir', type=Path, default=Path('benchmarks/matrices'),
                        help='Output directory for size presets (default: benchmarks/matrices)')

    args = parser.parse_args()

    # Ensure output directory exists
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Predefined size categories
    size_configs = {
        'small': [
            ('diagonal', 10, 'test_tiny_10x10_diagonal.coo'),
            ('laplacian', 10, 'test_small_10x10_laplacian.coo'),  # 100 unknowns
            ('tridiagonal', 100, 'test_small_100_tridiagonal.coo'),
        ],
        'medium': [
            ('laplacian', 50, 'test_medium_50x50_laplacian.coo'),  # 2,500 unknowns
            ('tridiagonal', 1000, 'test_medium_1000_tridiagonal.coo'),
            ('diagonal', 1000, 'test_medium_1000x1000_diagonal.coo'),
        ],
        'large': [
            ('laplacian', 100, 'test_large_100x100_laplacian.coo'),  # 10,000 unknowns
            ('tridiagonal', 10000, 'test_large_10000_tridiagonal.coo'),
        ],
        'huge': [
            ('laplacian', 200, 'test_huge_200x200_laplacian.coo'),  # 40,000 unknowns
            ('tridiagonal', 50000, 'test_huge_50000_tridiagonal.coo'),
        ],
    }

    # Handle specific type generation
    if args.type:
        if not args.output:
            print("Error: --output required when using --type", file=sys.stderr)
            return 1

        if args.type == 'laplacian':
            if not args.ngrid:
                print("Error: --ngrid required for Laplacian", file=sys.stderr)
                return 1
            generate_laplacian_2d(args.ngrid, args.output)
        elif args.type == 'diagonal':
            if not args.n:
                print("Error: --n required for diagonal", file=sys.stderr)
                return 1
            generate_diagonal(args.n, args.output)
        elif args.type == 'tridiagonal':
            if not args.n:
                print("Error: --n required for tridiagonal", file=sys.stderr)
                return 1
            generate_tridiagonal(args.n, args.output)
        return 0

    # Generate size presets
    sizes_to_generate = [args.size] if args.size != 'all' else ['small', 'medium', 'large', 'huge']

    for size in sizes_to_generate:
        print(f"\n=== Generating {size} matrices ===")
        for matrix_type, param, filename in size_configs[size]:
            output_path = args.output_dir / filename
            if matrix_type == 'laplacian':
                generate_laplacian_2d(param, output_path)
            elif matrix_type == 'diagonal':
                generate_diagonal(param, output_path)
            elif matrix_type == 'tridiagonal':
                generate_tridiagonal(param, output_path)

    print(f"\n✓ Matrix generation complete. Files written to {args.output_dir}")
    return 0


if __name__ == '__main__':
    sys.exit(main())
