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

# Generate for each precision
for prec in s d c z; do
    PREFIX="${PREFIXES[$prec]}"
    PREFIX_LOWER="$prec"
    TYPE="${TYPES[$prec]}"
    REAL_TYPE="${REAL_TYPES[$prec]}"
    REAL_CONV="${REAL_CONVS[$prec]}"
    REAL_LIT="${REAL_LITS[$prec]}"
    OUTPUT_FILE="$OUTPUT_DIR/${prec}${BASENAME}"

    # Use sed to replace template variables
    # NOTE: @MUMPS_REAL_TYPE@ must be replaced BEFORE @MUMPS_TYPE@
    # to avoid partial matches
    sed \
        -e "s/@MUMPS_PREFIX@/${PREFIX}/g" \
        -e "s/@MUMPS_PREFIX_LOWER@/${PREFIX_LOWER}/g" \
        -e "s/@MUMPS_REAL_TYPE@/${REAL_TYPE}/g" \
        -e "s/@MUMPS_REAL_CONV@/${REAL_CONV}/g" \
        -e "s/@MUMPS_REAL_LIT@/${REAL_LIT}/g" \
        -e "s/@MUMPS_TYPE@/${TYPE}/g" \
        "$TEMPLATE" > "$OUTPUT_FILE"

    echo "Generated: $OUTPUT_FILE"
done

echo "✓ Generated 4 precision-specific files from template: $TEMPLATE"
