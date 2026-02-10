#!/usr/bin/env python3
"""
Fortran Simple GOTO Converter

Automatically converts simple GOTO patterns to modern structured control flow.
Leaves complex patterns for manual conversion.

Usage:
    python fortran_simple_goto_convert.py input.f90 > output.f90
    python fortran_simple_goto_convert.py input.f90 -o output.f90
"""

import sys
import argparse
import re
from typing import List, Dict, Optional, Set, Tuple

class SimpleGOTOConverter:
    """Convert simple GOTO patterns to structured control flow."""

    def __init__(self):
        self.label_locations: Dict[str, int] = {}  # label -> line index
        self.goto_info: List[Tuple[int, str]] = []  # (line_index, target_label)
        self.labels_to_remove: Set[str] = set()  # Labels that can be removed
        self.lines_to_remove: Set[int] = set()  # Line indices to remove

    def is_comment(self, line: str) -> bool:
        """Check if line is a comment."""
        stripped = line.lstrip()
        return stripped.startswith('!')

    def is_preprocessor(self, line: str) -> bool:
        """Check if line is a preprocessor directive."""
        stripped = line.lstrip()
        return stripped.startswith('#')

    def extract_label(self, line: str) -> Optional[str]:
        """Extract statement label from line if present."""
        if self.is_comment(line) or self.is_preprocessor(line):
            return None
        match = re.match(r'^\s*(\d+)\s+', line)
        if match:
            return match.group(1)
        return None

    def find_goto_target(self, line: str) -> Optional[str]:
        """Find GOTO target label (only simple GOTO, not computed)."""
        # Pattern: GOTO label or GO TO label (but not GOTO (...))
        match = re.search(r'\bGO\s*TO\s+(\d+)(?!\s*\()', line, re.IGNORECASE)
        if match:
            return match.group(1)
        return None

    def is_error_handling_goto(self, goto_idx: int, target_idx: int, lines: List[str]) -> bool:
        """
        Check if GOTO is error handling pattern.

        Pattern:
          IF (ERROR_CONDITION) GOTO label
          ...
          label RETURN  or  label CONTINUE / RETURN
        """
        if target_idx <= goto_idx:
            return False  # Not forward jump

        # Check if target is near end and has RETURN nearby
        lines_remaining = len(lines) - target_idx
        if lines_remaining > 10:
            return False

        # Check for RETURN within a few lines of target
        for i in range(target_idx, min(target_idx + 5, len(lines))):
            if re.search(r'\bRETURN\b', lines[i], re.IGNORECASE):
                return True

        return False

    def convert_error_handling_goto(self, line: str, target_label: str) -> str:
        """
        Convert error handling GOTO to RETURN.

        Pattern:
          IF (CONDITION) GOTO label  →  IF (CONDITION) RETURN
        """
        # Replace GOTO with RETURN
        converted = re.sub(
            r'\bGO\s*TO\s+' + target_label + r'\b',
            'RETURN',
            line,
            flags=re.IGNORECASE
        )
        return converted

    def is_simple_forward_goto(self, goto_idx: int, target_idx: int, lines: List[str]) -> bool:
        """
        Check if GOTO is simple forward jump (skip code block).

        Pattern:
          IF (CONDITION) GOTO label
          ... code block ...
          label CONTINUE
        """
        if target_idx <= goto_idx:
            return False  # Not forward jump

        # Check if target is CONTINUE (or just a label)
        target_line = lines[target_idx].strip()
        if re.match(r'^\d+\s+CONTINUE\s*$', target_line, re.IGNORECASE):
            return True

        # Check if target is just a label (followed by another statement)
        if re.match(r'^\d+\s+\w', target_line):
            return True

        return False

    def convert_simple_forward_goto(self, lines: List[str], goto_idx: int, target_idx: int, target_label: str) -> List[str]:
        """
        Convert simple forward GOTO to IF...THEN...END IF.

        Pattern:
          IF (CONDITION) GOTO label  →  IF (.NOT. CONDITION) THEN
          ... code block ...              ... code block ...
          label CONTINUE                  END IF
        """
        converted = list(lines)  # Copy

        # Get GOTO line
        goto_line = converted[goto_idx]

        # Extract condition from IF statement
        match = re.search(r'\bIF\s*\(([^)]+)\)\s*GO\s*TO\s+\d+', goto_line, re.IGNORECASE)
        if not match:
            return lines  # Cannot convert (not simple IF GOTO)

        condition = match.group(1).strip()
        indent = len(goto_line) - len(goto_line.lstrip())

        # Negate condition (wrap in .NOT. if not already negated)
        if condition.upper().startswith('.NOT.'):
            # Remove .NOT.
            negated_condition = condition[5:].strip().strip('()')
        else:
            # Add .NOT.
            negated_condition = f'.NOT. ({condition})'

        # Replace GOTO line with IF THEN
        new_if_line = ' ' * indent + f'IF ({negated_condition}) THEN'
        converted[goto_idx] = new_if_line

        # Replace target CONTINUE with END IF
        target_line = converted[target_idx].strip()
        if re.match(r'^\d+\s+CONTINUE\s*$', target_line, re.IGNORECASE):
            target_indent = len(converted[target_idx]) - len(converted[target_idx].lstrip())
            converted[target_idx] = ' ' * target_indent + 'END IF'

        return converted

    def analyze_file(self, lines: List[str]):
        """Analyze file to identify conversion opportunities."""
        # Collect all labels
        for i, line in enumerate(lines):
            label = self.extract_label(line)
            if label:
                self.label_locations[label] = i

        # Collect all GOTOs
        for i, line in enumerate(lines):
            if self.is_comment(line) or self.is_preprocessor(line):
                continue
            target = self.find_goto_target(line)
            if target:
                self.goto_info.append((i, target))

    def convert_file(self, lines: List[str]) -> List[str]:
        """Convert simple GOTO patterns in file."""
        self.analyze_file(lines)
        converted = list(lines)

        # Convert error handling GOTOs
        for goto_idx, target_label in self.goto_info:
            if target_label not in self.label_locations:
                continue

            target_idx = self.label_locations[target_label]

            if self.is_error_handling_goto(goto_idx, target_idx, lines):
                # Convert to RETURN
                converted[goto_idx] = self.convert_error_handling_goto(converted[goto_idx], target_label)
                # Mark target label for removal (if safe)
                self.labels_to_remove.add(target_label)

        # Convert simple forward GOTOs
        # (Do this in a separate pass to avoid conflicts)
        conversions_made = []
        for goto_idx, target_label in self.goto_info:
            if target_label not in self.label_locations:
                continue

            target_idx = self.label_locations[target_label]

            if (self.is_simple_forward_goto(goto_idx, target_idx, converted) and
                not self.is_error_handling_goto(goto_idx, target_idx, lines)):
                # Try to convert to IF...THEN...END IF
                new_lines = self.convert_simple_forward_goto(converted, goto_idx, target_idx, target_label)
                if new_lines != converted:
                    converted = new_lines
                    conversions_made.append((goto_idx, target_idx, target_label))

        return converted


def main():
    parser = argparse.ArgumentParser(
        description='Convert simple GOTO patterns to structured control flow',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python fortran_simple_goto_convert.py input.f90 > output.f90
  python fortran_simple_goto_convert.py input.f90 -o output.f90

Converts:
  Pattern 1 (Error handling):
    IF (ERROR) GOTO 999    →    IF (ERROR) RETURN
    ...
    999 RETURN

  Pattern 2 (Simple forward jump):
    IF (CONDITION) GOTO 10    →    IF (.NOT. CONDITION) THEN
    ... code block ...                 ... code block ...
    10 CONTINUE                        END IF

Notes:
  - Only converts simple, safe patterns
  - Leaves complex patterns unchanged for manual review
  - Use fortran_analyze_goto.py first to assess conversion opportunities
  - Run compilation tests after conversion to verify correctness
        """
    )

    parser.add_argument('input', help='Input file (use - for stdin)')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print conversion statistics')

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

    # Read lines
    lines = [line.rstrip('\n\r') for line in input_file]

    # Close input if not stdin
    if args.input != '-':
        input_file.close()

    # Convert
    converter = SimpleGOTOConverter()
    original_goto_count = len([l for l in lines if converter.find_goto_target(l)])
    converted_lines = converter.convert_file(lines)
    new_goto_count = len([l for l in converted_lines if converter.find_goto_target(l)])

    # Verbose output
    if args.verbose:
        conversions = original_goto_count - new_goto_count
        print(f"Converted {conversions} simple GOTO patterns", file=sys.stderr)
        print(f"Remaining GOTOs: {new_goto_count} (require manual conversion)", file=sys.stderr)

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
