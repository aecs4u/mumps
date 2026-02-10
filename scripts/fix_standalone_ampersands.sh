#!/bin/bash
#
# Fix standalone continuation ampersands in Fortran files
#
# This script removes trailing & continuation characters that are followed
# by lines with only leading & continuation (no actual code).
#
# Example fix:
#   Before:                         After:
#   IF (A .AND. &                   IF (A .AND.
#     &  B .AND. &         →          &  B .AND.
#     &  C) THEN                      &  C) THEN
#
# Usage:
#   ./fix_standalone_ampersands.sh [--dry-run] [--verbose] [--auto]
#
# Options:
#   --dry-run   Show what would be fixed without making changes
#   --verbose   Show detailed progress
#   --auto      Non-interactive mode (for CI/CD)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DRY_RUN=0
VERBOSE=0
AUTO=0
BACKUP_SUFFIX=".backup_$(date +%Y%m%d_%H%M%S)"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    --verbose)
      VERBOSE=1
      shift
      ;;
    --auto)
      AUTO=1
      shift
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--dry-run] [--verbose] [--auto]"
      exit 1
      ;;
  esac
done

if [ $DRY_RUN -eq 1 ]; then
  echo "=== DRY RUN MODE - No files will be modified ==="
fi

# Create Python script for precise fixing
PYTHON_FIXER=$(cat <<'PYTHON_EOF'
import sys
import re
import argparse

def fix_standalone_ampersands(filepath, dry_run=False, verbose=False):
    """
    Fix standalone continuation ampersands in Fortran files.

    A standalone ampersand is a line that:
    1. Ends with & (continuation to next line)
    2. The next line ONLY contains whitespace and leading & (no actual code)

    These are syntax errors in strict Fortran 2023 mode.
    """
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        lines = f.readlines()

    fixed_lines = []
    fixes_made = 0
    i = 0

    while i < len(lines):
        line = lines[i]
        original_line = line

        # Check if this is a non-comment line ending with &
        # Pattern: any text (not starting with !) ending with & and optional whitespace
        if re.search(r'^[^!]*&\s*$', line):
            # Look ahead to see if next line is only a continuation line
            if i + 1 < len(lines):
                next_line = lines[i + 1]

                # Check if next line is ONLY whitespace and leading &
                # This indicates a standalone continuation
                if re.match(r'^\s*&\s*$', next_line):
                    # Remove trailing & from current line
                    line = re.sub(r'&(\s*)$', r'\1', line)
                    if line != original_line:
                        fixes_made += 1
                        if verbose:
                            print(f"  Line {i+1}: Removed standalone & before empty continuation", file=sys.stderr)

        fixed_lines.append(line)
        i += 1

    # Write back if not dry run
    if not dry_run and fixes_made > 0:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.writelines(fixed_lines)

    return fixes_made

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('filepath')
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--verbose', action='store_true')
    args = parser.parse_args()

    fixes = fix_standalone_ampersands(args.filepath, args.dry_run, args.verbose)
    print(fixes)  # Return number of fixes
PYTHON_EOF
)

# Find all Fortran files
cd "$REPO_ROOT"

FORTRAN_FILES=$(find src -type f \( -name "*.F" -o -name "*.f90" \) | sort)
TOTAL_FILES=$(echo "$FORTRAN_FILES" | wc -l)
TOTAL_FIXES=0
FILES_MODIFIED=0

echo "Found $TOTAL_FILES Fortran files to process"
echo ""

# Process each file
FILE_NUM=0
for file in $FORTRAN_FILES; do
  FILE_NUM=$((FILE_NUM + 1))

  if [ $VERBOSE -eq 1 ]; then
    echo "[$FILE_NUM/$TOTAL_FILES] Processing $file..."
  fi

  # Create backup if not dry run
  if [ $DRY_RUN -eq 0 ] && [ ! -f "$file$BACKUP_SUFFIX" ]; then
    cp "$file" "$file$BACKUP_SUFFIX"
  fi

  # Run Python fixer
  FIXES=$(echo "$PYTHON_FIXER" | python3 - "$file" \
    $([ $DRY_RUN -eq 1 ] && echo "--dry-run") \
    $([ $VERBOSE -eq 1 ] && echo "--verbose"))

  if [ "$FIXES" -gt 0 ]; then
    FILES_MODIFIED=$((FILES_MODIFIED + 1))
    TOTAL_FIXES=$((TOTAL_FIXES + FIXES))

    if [ $VERBOSE -eq 0 ]; then
      echo "✓ Fixed $FIXES issue(s) in $file"
    fi
  fi
done

echo ""
echo "=== Summary ==="
echo "Files processed: $TOTAL_FILES"
echo "Files modified: $FILES_MODIFIED"
echo "Total fixes: $TOTAL_FIXES"

if [ $DRY_RUN -eq 0 ] && [ $FILES_MODIFIED -gt 0 ]; then
  echo ""
  echo "Backups saved with suffix: $BACKUP_SUFFIX"
  echo "To restore: find src -name '*$BACKUP_SUFFIX' -exec sh -c 'mv \"\$1\" \"\${1%$BACKUP_SUFFIX}\"' _ {} \\;"
fi

if [ $TOTAL_FIXES -eq 0 ]; then
  echo ""
  echo "✓ No standalone ampersands found - codebase is clean!"
  exit 0
elif [ $DRY_RUN -eq 1 ]; then
  echo ""
  echo "Run without --dry-run to apply fixes"
  exit 0
else
  echo ""
  echo "✓ All fixes applied successfully"
  exit 0
fi
