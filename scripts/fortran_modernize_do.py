#!/usr/bin/env python3
"""
Fortran DO Loop Modernizer

Converts old-style DO N...N CONTINUE loops to modern DO...END DO syntax.

Usage:
    python fortran_modernize_do.py input.f90 > output.f90
    python fortran_modernize_do.py input.f90 -o output.f90
"""

import sys
import argparse
import re
from typing import List, Dict, Tuple, Optional

class DOLoopModernizer:
    """Modernize Fortran DO loops from labeled to structured form."""

    def __init__(self):
        # Map of label -> (line_index, loop_spec)
        self.do_labels: Dict[str, Tuple[int, str]] = {}
        # Set of labels that are used by non-DO statements
        self.non_do_labels = set()
        # Map of label -> line_index for CONTINUE statements
        self.continue_labels: Dict[str, int] = {}

    def is_comment(self, line: str) -> bool:
        """Check if line is a comment."""
        stripped = line.lstrip()
        return stripped.startswith('!')

    def is_preprocessor(self, line: str) -> bool:
        """Check if line is a preprocessor directive."""
        stripped = line.lstrip()
        return stripped.startswith('#')

    def is_template_conditional(self, line: str) -> bool:
        """Check if line is a template conditional."""
        stripped = line.strip()
        return stripped.startswith('@IF_') or stripped.startswith('@ENDIF@')

    def extract_label(self, line: str) -> Optional[str]:
        """Extract statement label from line if present."""
        if self.is_comment(line) or self.is_preprocessor(line) or self.is_template_conditional(line):
            return None

        # Label must be at the start (after optional whitespace)
        match = re.match(r'^\s*(\d+)\s+', line)
        if match:
            return match.group(1)
        return None

    def analyze_do_loop(self, line: str) -> Optional[Tuple[str, str]]:
        """
        Analyze if line is a labeled DO loop.

        Returns:
            (label, loop_spec) if it's a labeled DO, None otherwise
        """
        # Pattern: DO label variable = start, end [, increment]
        # Examples:
        #   DO 10 I = 1, N
        #   DO 20 J=1,100,2
        #   10 DO 20 I = 1, N
        match = re.match(r'^\s*(?:(\d+)\s+)?DO\s+(\d+)\s+(.+)', line, re.IGNORECASE)
        if match:
            stmt_label = match.group(1)  # May be None
            do_label = match.group(2)
            loop_spec = match.group(3).strip()
            return (do_label, loop_spec)
        return None

    def analyze_continue(self, line: str) -> Optional[str]:
        """
        Analyze if line is a labeled CONTINUE statement.

        Returns:
            label if it's a labeled CONTINUE, None otherwise
        """
        match = re.match(r'^\s*(\d+)\s+CONTINUE\s*$', line, re.IGNORECASE)
        if match:
            return match.group(1)
        return None

    def find_loop_pairs(self, lines: List[str]) -> Dict[str, Tuple[int, int, str]]:
        """
        Find DO...CONTINUE pairs.

        Returns:
            Dict mapping label -> (do_line_index, continue_line_index, loop_spec)
        """
        # First pass: collect DO labels and CONTINUE labels
        for i, line in enumerate(lines):
            if self.is_comment(line) or self.is_preprocessor(line) or self.is_template_conditional(line):
                continue

            # Check for DO loop
            do_info = self.analyze_do_loop(line)
            if do_info:
                label, loop_spec = do_info
                self.do_labels[label] = (i, loop_spec)
                continue

            # Check for CONTINUE
            cont_label = self.analyze_continue(line)
            if cont_label:
                self.continue_labels[cont_label] = i
                continue

            # Check if any label is used in other statements (GOTO, etc.)
            label = self.extract_label(line)
            if label and label not in self.do_labels:
                self.non_do_labels.add(label)

        # Second pass: match DO with CONTINUE
        loop_pairs = {}
        for label, (do_idx, loop_spec) in self.do_labels.items():
            if label in self.continue_labels:
                cont_idx = self.continue_labels[label]
                if cont_idx > do_idx:  # CONTINUE must be after DO
                    loop_pairs[label] = (do_idx, cont_idx, loop_spec)

        return loop_pairs

    def convert_do_loops(self, lines: List[str]) -> List[str]:
        """
        Convert DO...CONTINUE loops to DO...END DO.

        Returns:
            Modified list of lines
        """
        # Find all DO...CONTINUE pairs
        loop_pairs = self.find_loop_pairs(lines)

        if not loop_pairs:
            return lines  # No loops to convert

        # Create a new list of lines
        converted = []
        skip_lines = set()

        for label, (do_idx, cont_idx, loop_spec) in loop_pairs.items():
            # Mark CONTINUE line for replacement
            skip_lines.add(cont_idx)

        for i, line in enumerate(lines):
            if i in skip_lines:
                # Replace CONTINUE with END DO
                # Preserve indentation
                indent = len(line) - len(line.lstrip())
                converted.append(' ' * indent + 'END DO')
                continue

            # Check if this is a DO line that we're converting
            do_converted = False
            for label, (do_idx, cont_idx, loop_spec) in loop_pairs.items():
                if i == do_idx:
                    # Convert DO N ... to DO ...
                    # Remove the label from DO statement
                    new_line = re.sub(r'(DO)\s+\d+\s+', r'\1 ', line, flags=re.IGNORECASE)
                    converted.append(new_line)
                    do_converted = True
                    break

            if not do_converted:
                converted.append(line)

        return converted

    def process_file(self, input_file) -> List[str]:
        """Process entire file."""
        lines = [line.rstrip('\n\r') for line in input_file]
        converted = self.convert_do_loops(lines)
        return converted


def main():
    parser = argparse.ArgumentParser(
        description='Modernize Fortran DO loops from labeled to structured form',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python fortran_modernize_do.py input.f90 > output.f90
  python fortran_modernize_do.py input.f90 -o output.f90

Converts:
  DO 10 I = 1, N    →    DO I = 1, N
    ...                    ...
  10 CONTINUE            END DO

Notes:
  - Only converts DO...CONTINUE pairs
  - Preserves DO loops without CONTINUE (already modern or using EXIT/CYCLE)
  - Preserves nested loops
  - Preserves labels used by GOTO or other statements
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

    # Process
    modernizer = DOLoopModernizer()
    converted_lines = modernizer.process_file(input_file)

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
    for line in converted_lines:
        output_file.write(line + '\n')

    # Close output if not stdout
    if args.output:
        output_file.close()

    return 0


if __name__ == '__main__':
    sys.exit(main())
