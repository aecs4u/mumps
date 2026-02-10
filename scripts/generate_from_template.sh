#!/usr/bin/env bash
#
# Generate precision-specific source files from templates
#
# Usage: generate_from_template.sh <template.in> <output_dir>
#
# Template variables:
#   @MUMPS_PREFIX@        → S, D, C, Z (precision prefix)
#   @MUMPS_PREFIX_LOWER@  → s, d, c, z (lowercase precision prefix)
#   @MUMPS_TYPE@          → REAL, DOUBLE PRECISION, COMPLEX, COMPLEX*16
#   @MUMPS_REAL_TYPE@     → REAL, DOUBLE PRECISION, REAL, DOUBLE PRECISION
#                            (real-valued type, even for complex builds)
#   @MUMPS_REAL_CONV@     → real, dble, real, dble
#                            (Fortran intrinsic for real-valued conversion)
#   @MUMPS_REAL_LIT@      → E0, D0, E0, D0
#                            (Fortran real literal exponent suffix)
#   @MUMPS_ARITH@         → real, real, complex, complex
#                            (arithmetic type string for MatrixMarket format)
#   @MUMPS_RHS_WRITE@     → id%RHS(K8), id%RHS(K8),
#                            real(id%RHS(K8)), aimag(id%RHS(K8)),
#                            dble(id%RHS(K8)), aimag(id%RHS(K8))
#                            (RHS array write format: real→single value, complex→real,imag)
#   @MUMPS_MPI_TYPE@      → MPI_REAL, MPI_DOUBLE_PRECISION, MPI_COMPLEX, MPI_DOUBLE_COMPLEX
#                            (MPI data type for precision)
#
# Conditional blocks:
#   @IF_COMPLEX@...@ENDIF@  → Include only for c/z precisions
#   @IF_REAL@...@ENDIF@     → Include only for s/d precisions
#

set -euo pipefail

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <template.in> <output_dir>" >&2
    exit 1
fi

TEMPLATE="$1"
OUTPUT_DIR="$2"

if [[ ! -f "$TEMPLATE" ]]; then
    echo "Error: Template file not found: $TEMPLATE" >&2
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

# Get base name without .in extension
BASENAME="$(basename "$TEMPLATE" .in)"

# Precision definitions
declare -A PREFIXES=(
    [s]="S"
    [d]="D"
    [c]="C"
    [z]="Z"
)

declare -A TYPES=(
    [s]="REAL"
    [d]="DOUBLE PRECISION"
    [c]="COMPLEX"
    [z]="COMPLEX*16"
)

declare -A REAL_TYPES=(
    [s]="REAL"
    [d]="DOUBLE PRECISION"
    [c]="REAL"
    [z]="DOUBLE PRECISION"
)

declare -A REAL_CONVS=(
    [s]="real"
    [d]="dble"
    [c]="real"
    [z]="dble"
)

declare -A REAL_LITS=(
    [s]="E0"
    [d]="D0"
    [c]="E0"
    [z]="D0"
)

declare -A ARITHS=(
    [s]="real"
    [d]="real"
    [c]="complex"
    [z]="complex"
)

declare -A RHS_WRITES=(
    [s]="id%RHS(K8)"
    [d]="id%RHS(K8)"
    [c]="real(id%RHS(K8)), aimag(id%RHS(K8))"
    [z]="dble(id%RHS(K8)), aimag(id%RHS(K8))"
)

declare -A MPI_TYPES=(
    [s]="MPI_REAL"
    [d]="MPI_DOUBLE_PRECISION"
    [c]="MPI_COMPLEX"
    [z]="MPI_DOUBLE_COMPLEX"
)

# Generate for each precision
for prec in s d c z; do
    PREFIX="${PREFIXES[$prec]}"
    PREFIX_LOWER="$prec"
    TYPE="${TYPES[$prec]}"
    REAL_TYPE="${REAL_TYPES[$prec]}"
    REAL_CONV="${REAL_CONVS[$prec]}"
    REAL_LIT="${REAL_LITS[$prec]}"
    ARITH="${ARITHS[$prec]}"
    RHS_WRITE="${RHS_WRITES[$prec]}"
    MPI_TYPE="${MPI_TYPES[$prec]}"
    OUTPUT_FILE="$OUTPUT_DIR/${prec}${BASENAME}"

    # Process conditional blocks based on precision
    # Complex precisions: c, z
    # Real precisions: s, d
    IS_COMPLEX=$([[ "$prec" == "c" || "$prec" == "z" ]] && echo "1" || echo "0")

    # Use awk to handle conditional blocks, then sed for variable substitution
    RHS_WRITE_ESCAPED=$(echo "$RHS_WRITE" | sed 's/[&/\]/\\&/g')

    awk -v is_complex="$IS_COMPLEX" '
        /@IF_COMPLEX@/ { in_complex=1; next }
        /@IF_REAL@/ { in_real=1; next }
        /@ENDIF@/ {
            in_complex=0
            in_real=0
            next
        }
        {
            if (in_complex && !is_complex) next
            if (in_real && is_complex) next
            print
        }
    ' "$TEMPLATE" | sed \
        -e "s/@MUMPS_PREFIX@/${PREFIX}/g" \
        -e "s/@MUMPS_PREFIX_LOWER@/${PREFIX_LOWER}/g" \
        -e "s/@MUMPS_REAL_TYPE@/${REAL_TYPE}/g" \
        -e "s/@MUMPS_REAL_CONV@/${REAL_CONV}/g" \
        -e "s/@MUMPS_REAL_LIT@/${REAL_LIT}/g" \
        -e "s/@MUMPS_ARITH@/${ARITH}/g" \
        -e "s/@MUMPS_RHS_WRITE@/${RHS_WRITE_ESCAPED}/g" \
        -e "s/@MUMPS_MPI_TYPE@/${MPI_TYPE}/g" \
        -e "s/@MUMPS_TYPE@/${TYPE}/g" \
        > "$OUTPUT_FILE"

    echo "Generated: $OUTPUT_FILE"
done

echo "✓ Generated 4 precision-specific files from template: $TEMPLATE"
