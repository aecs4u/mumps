#!/usr/bin/env python3
"""
Convert Fortran fixed-form continuation syntax to free-form syntax.

Fixed-form (old):
    DO WHILE ( condition .AND.
     &         another_condition )

Free-form (new):
    DO WHILE ( condition .AND. &
               another_condition )

This script:
1. Detects lines followed by fixed-form continuations (& in column 6)
2. Moves the & to the end of the previous line
3. Removes the leading & from the continuation line
4. Preserves proper indentation
"""

import sys
import re
import os
import argparse
from pathlib import Path


def is_comment_line(line):
    """Check if line is a comment."""
    stripped = line.lstrip()
    return stripped.startswith('!') or stripped.startswith('C') or stripped.startswith('c')


def convert_c_comments_to_exclamation(lines):
    """
    Convert fixed-form C/c comments to free-form ! comments.
    Only converts lines that start with C or c (case-insensitive).
    """
    new_lines = []
    for line in lines:
        stripped = line.lstrip()
        if stripped.startswith('C') and (len(stripped) == 1 or stripped[1] in (' ', '\n', '\t')):
            # Replace leading C with !
            new_lines.append(line.replace('C', '!', 1))
        elif stripped.startswith('c') and (len(stripped) == 1 or stripped[1] in (' ', '\n', '\t')):
            # Replace leading c with !
            new_lines.append(line.replace('c', '!', 1))
        else:
            new_lines.append(line)
    return new_lines


def is_fixed_form_continuation(line):
    """
    Check if line is a fixed-form continuation.
    In fixed-form: column 6 (index 5 in 0-based) has & or other continuation char.
    Pattern: "     &" at start of line (5 spaces + &)
    """
    # Match lines that start with exactly 5 spaces followed by &
    return re.match(r'^     &', line) is not None


def get_base_indent(line):
    """Get the indentation level of a line (number of leading spaces)."""
    return len(line) - len(line.lstrip())


def convert_continuation_block(lines, start_idx):
    """
    Convert a block of fixed-form continuations to free-form.

    Args:
        lines: List of all lines in the file
        start_idx: Index of the first line that needs a continuation

    Returns:
        (new_lines, next_idx) - converted lines and index of next line to process
    """
    new_lines = []
    i = start_idx
    base_line = lines[i].rstrip()

    # Check if base line already ends with & (already free-form)
    if base_line.endswith('&'):
        return [lines[i]], i + 1

    # Collect all continuation lines
    continuation_lines = []
    i += 1
    while i < len(lines) and is_fixed_form_continuation(lines[i]):
        continuation_lines.append(lines[i])
        i += 1

    if not continuation_lines:
        return [lines[start_idx]], start_idx + 1

    # Add & to base line
    new_lines.append(base_line + ' &\n')

    # Process continuation lines
    for j, cont_line in enumerate(continuation_lines):
        # Remove the "     &" prefix
        content = cont_line[6:]  # Skip "     &"

        # Determine indentation for continuation
        # Use the indentation of the content after the &
        if content.strip():
            # Keep existing indentation relative to column 7
            new_lines.append(content)
        else:
            # Empty continuation line - preserve it
            new_lines.append('\n')

    return new_lines, i


def convert_file(filepath, dry_run=False, verbose=False):
    """
    Convert a single Fortran file from fixed-form to free-form.

    Performs:
    1. C/c comment conversion to !
    2. Fixed-form continuation conversion to free-form

    Returns:
        (changes_made, total_continuations_fixed, comments_converted)
    """
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        lines = f.readlines()

    # First, convert C comments to ! comments
    lines_with_comments_converted = convert_c_comments_to_exclamation(lines)
    comments_converted = sum(1 for old, new in zip(lines, lines_with_comments_converted) if old != new)

    # Then, convert continuations
    new_lines = []
    i = 0
    continuations_fixed = 0

    while i < len(lines_with_comments_converted):
        line = lines_with_comments_converted[i]

        # Skip comment lines
        if is_comment_line(line):
            new_lines.append(line)
            i += 1
            continue

        # Check if next line is a fixed-form continuation
        if i + 1 < len(lines_with_comments_converted) and is_fixed_form_continuation(lines_with_comments_converted[i + 1]):
            converted, next_i = convert_continuation_block(lines_with_comments_converted, i)
            new_lines.extend(converted)
            continuations_fixed += (next_i - i - 1)  # Count continuation lines fixed
            i = next_i
        else:
            new_lines.append(line)
            i += 1

    # Write back if changes were made and not dry run
    changes_made = (lines != new_lines)

    if changes_made and not dry_run:
        # Backup original
        backup_path = str(filepath) + '.backup_fixedform'
        if not os.path.exists(backup_path):
            with open(backup_path, 'w', encoding='utf-8') as f:
                f.writelines(lines)

        # Write converted file
        with open(filepath, 'w', encoding='utf-8') as f:
            f.writelines(new_lines)

        if verbose:
            details = []
            if comments_converted > 0:
                details.append(f"{comments_converted} comments")
            if continuations_fixed > 0:
                details.append(f"{continuations_fixed} continuations")
            if details:
                print(f"✓ Converted {filepath}: {', '.join(details)}")
    elif changes_made and dry_run:
        if verbose:
            details = []
            if comments_converted > 0:
                details.append(f"{comments_converted} comments")
            if continuations_fixed > 0:
                details.append(f"{continuations_fixed} continuations")
            if details:
                print(f"[DRY RUN] Would convert {filepath}: {', '.join(details)}")

    return changes_made, continuations_fixed, comments_converted


def main():
    parser = argparse.ArgumentParser(
        description='Convert Fortran files from fixed-form to free-form continuations'
    )
    parser.add_argument('path', help='File or directory to process')
    parser.add_argument('--dry-run', action='store_true',
                       help='Show what would be changed without modifying files')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose output')
    parser.add_argument('--pattern', default='*.F',
                       help='File pattern to match (default: *.F)')

    args = parser.parse_args()

    path = Path(args.path)

    # Collect files to process
    if path.is_file():
        files = [path]
    elif path.is_dir():
        files = sorted(path.rglob(args.pattern))
    else:
        print(f"Error: {path} is not a file or directory", file=sys.stderr)
        return 1

    print(f"{'[DRY RUN] ' if args.dry_run else ''}Processing {len(files)} files...")
    print()

    total_files_changed = 0
    total_continuations = 0
    total_comments = 0

    for filepath in files:
        try:
            changed, cont_count, comm_count = convert_file(filepath, args.dry_run, args.verbose)
            if changed:
                total_files_changed += 1
                total_continuations += cont_count
                total_comments += comm_count
        except Exception as e:
            print(f"Error processing {filepath}: {e}", file=sys.stderr)

    print()
    print("=" * 60)
    print(f"Summary:")
    print(f"  Files processed: {len(files)}")
    print(f"  Files changed: {total_files_changed}")
    print(f"  C/c comments converted: {total_comments}")
    print(f"  Continuation lines converted: {total_continuations}")

    if args.dry_run:
        print()
        print("Run without --dry-run to apply changes")
    else:
        print()
        print(f"Backups saved with .backup_fixedform extension")

    return 0


if __name__ == '__main__':
    sys.exit(main())
