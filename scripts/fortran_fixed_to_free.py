#!/usr/bin/env python3
"""
Fortran Fixed-Form to Free-Form Converter

Mechanically converts legacy Fortran 77 fixed-form source code to modern
free-form syntax while preserving template variables and preprocessor directives.

Usage:
    python fortran_fixed_to_free.py input.F > output.f90
    python fortran_fixed_to_free.py input.F -o output.f90
"""

import sys
import argparse
import re
from typing import List, Tuple

class FixedToFreeConverter:
    """Convert Fortran fixed-form to free-form."""

    def __init__(self):
        self.continuation_pending = False
        self.in_preprocessor_block = False

    def is_comment_line(self, line: str) -> bool:
        """Check if line is a comment (C, c, *, or ! in column 1)."""
        if not line:
            return False
        first_char = line[0]
        return first_char in ['C', 'c', '*', '!']

    def is_preprocessor_line(self, line: str) -> bool:
        """Check if line is a preprocessor directive (#if, #ifdef, etc.)."""
        stripped = line.lstrip()
        return stripped.startswith('#')

    def is_template_conditional(self, line: str) -> bool:
        """Check if line is a template conditional (@IF_REAL@, @IF_COMPLEX@, @ENDIF@)."""
        stripped = line.strip()
        return stripped.startswith('@IF_') or stripped.startswith('@ENDIF@')

    def is_blank_line(self, line: str) -> bool:
        """Check if line is blank or whitespace only."""
        return not line.strip()

    def convert_comment(self, line: str) -> str:
        """Convert fixed-form comment to free-form."""
        # Replace C, c, * with !
        return '!' + line[1:]

    def extract_fixed_form_parts(self, line: str) -> Tuple[str, bool, str]:
        """
        Extract parts of a fixed-form line.

        Returns:
            (label, is_continuation, code)

        Fixed-form format:
        - Columns 1-5: Statement label
        - Column 6: Continuation character (non-space, non-zero)
        - Columns 7-72: Fortran code
        - Columns 73-80: Sequence numbers (ignored)
        """
        # Ensure line is at least 6 characters (pad if needed)
        padded = line.ljust(6)

        # Column 1-5: Label
        label = padded[0:5].strip()

        # Column 6: Continuation
        cont_char = padded[5] if len(padded) > 5 else ' '
        is_continuation = cont_char not in [' ', '0']

        # Column 7-72: Code (column 73+ is sequence numbers, ignored)
        code = padded[6:72].rstrip() if len(padded) > 6 else ''

        return label, is_continuation, code

    def convert_line(self, line: str) -> str:
        """Convert a single fixed-form line to free-form."""
        # Remove trailing whitespace and newline
        line = line.rstrip('\n\r')

        # Preserve blank lines
        if self.is_blank_line(line):
            return ''

        # Preserve preprocessor directives unchanged
        if self.is_preprocessor_line(line):
            return line

        # Preserve template conditionals unchanged (must be at start of line)
        if self.is_template_conditional(line):
            return line

        # Convert comment lines
        if self.is_comment_line(line):
            return self.convert_comment(line)

        # Extract fixed-form parts
        label, is_continuation, code = self.extract_fixed_form_parts(line)

        # Build free-form line
        if is_continuation:
            # Continuation line: indent and add leading &
            result = '  &' + code
        elif label:
            # Labeled line: keep label, add code
            result = label + ' ' + code
        else:
            # Normal line: just code (with some indentation if it's not a label target)
            result = code

        return result.rstrip()

    def convert_file(self, input_file) -> List[str]:
        """Convert entire file from fixed-form to free-form."""
        output_lines = []

        for line in input_file:
            converted = self.convert_line(line)
            output_lines.append(converted)

        return output_lines

    def post_process(self, lines: List[str]) -> List[str]:
        """
        Post-process converted lines to handle special cases.

        This includes:
        - Adding trailing & to lines that continue
        - Cleaning up excessive blank lines
        - Preserving intentional blank lines
        """
        processed = []
        prev_line_blank = True  # Start with this to avoid leading blank

        for i, line in enumerate(lines):
            is_blank = not line.strip()

            # Skip excessive consecutive blank lines (max 1)
            if is_blank and prev_line_blank:
                continue

            # Check if next line is a continuation
            next_is_continuation = (i + 1 < len(lines) and
                                   lines[i + 1].lstrip().startswith('&'))

            # Add trailing & if next line continues
            if next_is_continuation and not is_blank:
                # Don't add & if line already ends with &, is a comment, or is a preprocessor directive
                stripped = line.rstrip()
                is_comment = stripped.lstrip().startswith('!')
                is_preprocessor = stripped.lstrip().startswith('#')
                is_template_cond = stripped.startswith('@IF_') or stripped.startswith('@ENDIF@')

                if not stripped.endswith('&') and not is_comment and not is_preprocessor and not is_template_cond:
                    line = line + ' &'

            processed.append(line)
            prev_line_blank = is_blank

        return processed


def main():
    parser = argparse.ArgumentParser(
        description='Convert Fortran fixed-form to free-form',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python fortran_fixed_to_free.py input.F > output.f90
  python fortran_fixed_to_free.py input.F -o output.f90
  cat input.F | python fortran_fixed_to_free.py - > output.f90

Notes:
  - Preserves template variables (@MUMPS_PREFIX@, @MUMPS_TYPE@, etc.)
  - Preserves preprocessor directives (#if, #ifdef, #include, etc.)
  - Preserves template conditionals (@IF_REAL@, @IF_COMPLEX@, @ENDIF@)
  - Converts comment markers (C, c, * → !)
  - Handles continuation lines (column 6 → & at start/end)
  - Removes sequence numbers (columns 73-80)
        """
    )

    parser.add_argument('input', help='Input file (use - for stdin)')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')

    args = parser.parse_args()

    # Open input
    if args.input == '-':
        input_file = sys.stdin
    else:
        try:
            input_file = open(args.input, 'r')
        except IOError as e:
            print(f"Error opening input file: {e}", file=sys.stderr)
            return 1

    # Convert
    converter = FixedToFreeConverter()
    converted_lines = converter.convert_file(input_file)
    post_processed = converter.post_process(converted_lines)

    # Close input if not stdin
    if args.input != '-':
        input_file.close()

    # Open output
    if args.output:
        try:
            output_file = open(args.output, 'w')
        except IOError as e:
            print(f"Error opening output file: {e}", file=sys.stderr)
            return 1
    else:
        output_file = sys.stdout

    # Write output
    for line in post_processed:
        output_file.write(line + '\n')

    # Close output if not stdout
    if args.output:
        output_file.close()

    return 0


if __name__ == '__main__':
    sys.exit(main())
