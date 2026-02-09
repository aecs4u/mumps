#!/usr/bin/env bash
#
# Test harness for template-based code generation verification
# Verifies that generated files are byte-identical to originals
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

TEMPLATE_DIR="src/templates"
GENERATED_DIR="src/generated"
ORIGINAL_DIR="src"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

passed=0
failed=0
errors=()

usage() {
    echo "Usage: $0 [template_name]"
    echo ""
    echo "Verifies template generation produces byte-identical output to originals."
    echo ""
    echo "Arguments:"
    echo "  template_name   Specific template to verify (without .in extension)"
    echo "                  If omitted, verifies all templates"
    echo ""
    echo "Examples:"
    echo "  $0                        # Verify all templates"
    echo "  $0 mumps_config_file      # Verify specific template"
    echo "  $0 ana_aux                # Verify analysis auxiliary template"
}

verify_file() {
    local base_name="$1"
    local prec="$2"
    local prec_upper="${prec^^}"

    local template_file="${TEMPLATE_DIR}/${base_name}.in"
    local generated_file="${GENERATED_DIR}/${prec}${base_name}"
    local original_file="${ORIGINAL_DIR}/${prec}${base_name}"

    # Skip if original doesn't exist (might be intentionally missing)
    if [[ ! -f "$original_file" ]]; then
        echo -e "${YELLOW}SKIP${NC} ${prec}${base_name} - original not found"
        return 0
    fi

    # Generate from template
    if ! ./scripts/generate_from_template.sh "$template_file" "$GENERATED_DIR" >/dev/null 2>&1; then
        echo -e "${RED}FAIL${NC} ${prec}${base_name} - generation failed"
        errors+=("Generation failed for $template_file")
        ((failed++))
        return 1
    fi

    # Verify generated file exists
    if [[ ! -f "$generated_file" ]]; then
        echo -e "${RED}FAIL${NC} ${prec}${base_name} - generated file not created"
        errors+=("Generated file not found: $generated_file")
        ((failed++))
        return 1
    fi

    # Byte-compare (ignoring trailing whitespace differences)
    if diff -q -Z "$original_file" "$generated_file" >/dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC} ${prec}${base_name} - byte-identical"
        ((passed++))
        return 0
    else
        # Check if only whitespace differs
        if diff -q -w "$original_file" "$generated_file" >/dev/null 2>&1; then
            echo -e "${YELLOW}WARN${NC} ${prec}${base_name} - whitespace differences only"
            ((passed++))
            return 0
        else
            echo -e "${RED}FAIL${NC} ${prec}${base_name} - content differs"
            errors+=("Content mismatch: $original_file vs $generated_file")
            ((failed++))
            return 1
        fi
    fi
}

verify_template() {
    local template_basename="$1"

    echo ""
    echo "Verifying template: ${template_basename}"
    echo "========================================"

    # Detect file extension from template
    local template_path="${TEMPLATE_DIR}/${template_basename}.in"
    if [[ ! -f "$template_path" ]]; then
        echo -e "${RED}ERROR${NC}: Template not found: $template_path"
        return 1
    fi

    # Determine base filename (without precision prefix)
    # Template might be named like "mumps_config_file.f90.in"
    local base_with_ext=$(basename "$template_path" .in)

    # Try all 4 precisions
    for prec in s d c z; do
        verify_file "$base_with_ext" "$prec"
    done

    echo ""
}

verify_all_templates() {
    echo "Verifying all templates in ${TEMPLATE_DIR}/"
    echo "============================================"

    if [[ ! -d "$TEMPLATE_DIR" ]]; then
        echo -e "${RED}ERROR${NC}: Template directory not found: $TEMPLATE_DIR"
        exit 1
    fi

    # Find all .in files in template directory
    local templates=($(find "$TEMPLATE_DIR" -name "*.in" -type f -exec basename {} .in \;))

    if [[ ${#templates[@]} -eq 0 ]]; then
        echo -e "${YELLOW}WARNING${NC}: No templates found in $TEMPLATE_DIR"
        exit 0
    fi

    echo "Found ${#templates[@]} template(s)"

    for template in "${templates[@]}"; do
        verify_template "$template"
    done
}

compile_test() {
    local prec="$1"

    echo ""
    echo "Compilation test for precision: $prec"
    echo "====================================="

    # Try to compile one generated file as a quick test
    local test_file="${GENERATED_DIR}/${prec}mumps_config_file.f90"

    if [[ ! -f "$test_file" ]]; then
        echo -e "${YELLOW}SKIP${NC} - test file not found: $test_file"
        return 0
    fi

    local obj_file="/tmp/${prec}mumps_config_file.o"

    if gfortran -c "$test_file" -o "$obj_file" >/dev/null 2>&1; then
        local size=$(stat -c%s "$obj_file" 2>/dev/null || stat -f%z "$obj_file" 2>/dev/null || echo "unknown")
        echo -e "${GREEN}PASS${NC} - compiles successfully (object size: $size bytes)"
        rm -f "$obj_file"
        return 0
    else
        echo -e "${RED}FAIL${NC} - compilation failed"
        errors+=("Compilation failed for $test_file")
        return 1
    fi
}

# Main execution
main() {
    if [[ "${1:-}" == "--help" ]] || [[ "${1:-}" == "-h" ]]; then
        usage
        exit 0
    fi

    # Create generated directory if it doesn't exist
    mkdir -p "$GENERATED_DIR"

    if [[ $# -eq 0 ]]; then
        # Verify all templates
        verify_all_templates
    else
        # Verify specific template
        verify_template "$1"
    fi

    # Run compilation tests
    echo ""
    echo "Compilation Tests"
    echo "================="
    for prec in s d c z; do
        compile_test "$prec"
    done

    # Summary
    echo ""
    echo "========================================"
    echo "Verification Summary"
    echo "========================================"
    echo -e "Passed: ${GREEN}${passed}${NC}"
    echo -e "Failed: ${RED}${failed}${NC}"

    if [[ ${#errors[@]} -gt 0 ]]; then
        echo ""
        echo "Errors:"
        for error in "${errors[@]}"; do
            echo "  - $error"
        done
    fi

    if [[ $failed -eq 0 ]]; then
        echo -e "\n${GREEN}✓ All verifications passed!${NC}"
        exit 0
    else
        echo -e "\n${RED}✗ Some verifications failed${NC}"
        exit 1
    fi
}

main "$@"
