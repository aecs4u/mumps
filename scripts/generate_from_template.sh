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

# Generate for each precision
for prec in s d c z; do
    PREFIX="${PREFIXES[$prec]}"
    PREFIX_LOWER="$prec"
    TYPE="${TYPES[$prec]}"
    OUTPUT_FILE="$OUTPUT_DIR/${prec}${BASENAME}"

    # Use sed to replace template variables
    sed \
        -e "s/@MUMPS_PREFIX@/${PREFIX}/g" \
        -e "s/@MUMPS_PREFIX_LOWER@/${PREFIX_LOWER}/g" \
        -e "s/@MUMPS_TYPE@/${TYPE}/g" \
        "$TEMPLATE" > "$OUTPUT_FILE"

    echo "Generated: $OUTPUT_FILE"
done

echo "✓ Generated 4 precision-specific files from template: $TEMPLATE"
