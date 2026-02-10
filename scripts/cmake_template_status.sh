#!/usr/bin/env bash
#
# Show smart build status for CMake template system
#

set -euo pipefail

BUILD_DIR="${1:-.build}"
STAMP_DIR="$BUILD_DIR/template_stamps"
TEMPLATE_DIR="src/templates"

if [[ ! -d "$STAMP_DIR" ]]; then
    echo "No build directory found. Run cmake first:"
    echo "  cmake -B $BUILD_DIR"
    exit 1
fi

echo "CMake Template Smart Build Status"
echo "=================================="
echo ""

# Count templates
total_templates=$(ls -1 "$TEMPLATE_DIR"/*.in 2>/dev/null | wc -l)
echo "Total templates: $total_templates"
echo ""

# Check which need rebuilding
needs_rebuild=0
up_to_date=0

echo "Status by template:"
echo ""

for template in "$TEMPLATE_DIR"/*.in; do
    basename=$(basename "$template" .in)
    basename=${basename%.F}
    basename=${basename%.f90}
    
    stamp="$STAMP_DIR/${basename}.stamp"
    
    # Check if needs rebuild
    rebuild=0
    
    # Check if stamp exists
    if [[ ! -f "$stamp" ]]; then
        rebuild=1
        reason="no stamp"
    else
        # Check if template is newer
        if [[ "$template" -nt "$stamp" ]]; then
            rebuild=1
            reason="template modified"
        fi
    fi
    
    # Check if any output is missing
    if [[ $rebuild -eq 0 ]]; then
        for prec in s d c z; do
            # Determine extension
            if [[ "$template" == *.F.in ]]; then
                output="src/${prec}${basename}.F"
            elif [[ "$template" == *.f90.in ]]; then
                output="src/${prec}${basename}.f90"
            else
                output="src/${prec}${basename}"
            fi
            
            if [[ ! -f "$output" ]]; then
                rebuild=1
                reason="missing $prec output"
                break
            fi
        done
    fi
    
    if [[ $rebuild -eq 1 ]]; then
        echo "  ⟳ $basename - needs rebuild ($reason)"
        ((needs_rebuild++))
    else
        echo "  ✓ $basename - up-to-date"
        ((up_to_date++))
    fi
done

echo ""
echo "Summary:"
echo "  Up-to-date: $up_to_date"
echo "  Need rebuild: $needs_rebuild"

if [[ $needs_rebuild -gt 0 ]]; then
    echo ""
    echo "Run 'cmake --build $BUILD_DIR' to generate updated templates"
else
    echo ""
    echo "All templates are current. No rebuild needed!"
fi
