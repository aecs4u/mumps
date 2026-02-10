#!/usr/bin/env python3
"""
Fortran GOTO Pattern Analyzer

Analyzes GOTO statements in Fortran code and classifies them by conversion complexity.
Generates a JSON report to guide automated vs manual conversion.

Usage:
    python fortran_analyze_goto.py input.f90
    python fortran_analyze_goto.py input.f90 -o report.json
"""

import sys
import argparse
import re
import json
from typing import List, Dict, Optional, Tuple
from collections import defaultdict

class GOTOAnalyzer:
    """Analyze GOTO patterns in Fortran code."""

    def __init__(self):
        # Classification counters
        self.simple_forward = 0  # Simple forward jump (single target)
        self.error_handling = 0  # Jump to end of routine
        self.backward_jump = 0   # Jump to earlier line (loop-like)
        self.multi_target = 0    # Same label targeted by multiple GOTOs
        self.computed_goto = 0   # Computed GOTO (GOTO (10,20,30), I)
        self.complex_pattern = 0 # Other complex patterns

        # Detailed tracking
        self.goto_locations: List[Tuple[int, str, str]] = []  # (line, label, context)
        self.label_locations: Dict[str, int] = {}  # label -> line number
        self.label_usage_count: Dict[str, int] = defaultdict(int)  # label -> usage count

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

    def find_goto_statements(self, line: str) -> List[str]:
        """
        Find all GOTO statements and their target labels in a line.

        Returns:
            List of target labels
        """
        labels = []

        # Pattern 1: Simple GOTO (GOTO 100, GO TO 100)
        match = re.findall(r'\bGO\s*TO\s+(\d+)', line, re.IGNORECASE)
        labels.extend(match)

        # Pattern 2: Computed GOTO (GOTO (10,20,30), I)
        match = re.search(r'\bGO\s*TO\s*\(([^)]+)\)', line, re.IGNORECASE)
        if match:
            # Extract all numbers from the list
            target_list = match.group(1)
            targets = re.findall(r'\d+', target_list)
            labels.extend(targets)

        return labels

    def is_error_handling_pattern(self, goto_line_num: int, target_line_num: int, lines: List[str]) -> bool:
        """
        Check if GOTO is error handling (jump to end of routine).

        Error handling pattern:
        - Forward jump
        - Target near end of file or before RETURN/END
        """
        if target_line_num <= goto_line_num:
            return False  # Not forward jump

        # Check if target is near end
        lines_remaining = len(lines) - target_line_num
        if lines_remaining < 10:  # Within 10 lines of end
            return True

        # Check if there's a RETURN or END statement shortly after target
        for i in range(target_line_num, min(target_line_num + 10, len(lines))):
            line_upper = lines[i].upper()
            if re.search(r'\b(RETURN|END\s+SUBROUTINE|END\s+FUNCTION|END\s+PROGRAM)\b', line_upper):
                return True

        return False

    def classify_goto(self, goto_line_num: int, target_label: str, lines: List[str]) -> str:
        """
        Classify a GOTO statement.

        Returns:
            Classification string: 'simple_forward', 'error_handling', 'backward_jump',
            'multi_target', 'computed', or 'complex'
        """
        if target_label not in self.label_locations:
            return 'complex'  # Label not found

        target_line_num = self.label_locations[target_label]

        # Check if computed GOTO (already detected)
        goto_line = lines[goto_line_num].upper()
        if re.search(r'GO\s*TO\s*\(', goto_line):
            return 'computed'

        # Check if multi-target (same label used by multiple GOTOs)
        if self.label_usage_count[target_label] > 1:
            # Could be multi-target or simple, check further
            is_multi = True
        else:
            is_multi = False

        # Check direction
        if target_line_num < goto_line_num:
            # Backward jump (loop-like pattern)
            return 'backward_jump'

        # Check if error handling
        if self.is_error_handling_pattern(goto_line_num, target_line_num, lines):
            return 'error_handling'

        # Check if simple forward jump (single use)
        if not is_multi:
            return 'simple_forward'

        # Multi-target (same label, multiple GOTOs)
        return 'multi_target'

    def analyze_file(self, lines: List[str]) -> Dict:
        """
        Analyze entire file and generate report.

        Returns:
            Dictionary with analysis results
        """
        # First pass: collect all labels and their locations
        for i, line in enumerate(lines):
            label = self.extract_label(line)
            if label:
                self.label_locations[label] = i

        # Second pass: find all GOTO statements
        for i, line in enumerate(lines):
            if self.is_comment(line) or self.is_preprocessor(line):
                continue

            goto_labels = self.find_goto_statements(line)
            for label in goto_labels:
                self.goto_locations.append((i, label, line.strip()[:80]))  # Truncate context
                self.label_usage_count[label] += 1

        # Third pass: classify each GOTO
        classifications = defaultdict(list)
        for goto_line, target_label, context in self.goto_locations:
            classification = self.classify_goto(goto_line, target_label, lines)

            # Update counters
            if classification == 'simple_forward':
                self.simple_forward += 1
            elif classification == 'error_handling':
                self.error_handling += 1
            elif classification == 'backward_jump':
                self.backward_jump += 1
            elif classification == 'multi_target':
                self.multi_target += 1
            elif classification == 'computed':
                self.computed_goto += 1
            else:
                self.complex_pattern += 1

            # Store details
            classifications[classification].append({
                'line': goto_line + 1,  # 1-indexed for readability
                'label': target_label,
                'context': context
            })

        # Calculate recommendations
        automated = self.simple_forward + self.error_handling
        manual = (self.backward_jump + self.multi_target +
                 self.computed_goto + self.complex_pattern)

        # Generate report
        report = {
            'total_goto_count': len(self.goto_locations),
            'patterns': {
                'simple_forward': self.simple_forward,
                'error_handling': self.error_handling,
                'backward_jump': self.backward_jump,
                'multi_target': self.multi_target,
                'computed_goto': self.computed_goto,
                'complex': self.complex_pattern
            },
            'recommendations': {
                'automated': automated,
                'manual': manual,
                'conversion_strategy': self.get_conversion_strategy(automated, manual)
            },
            'details': dict(classifications)
        }

        return report

    def get_conversion_strategy(self, automated: int, manual: int) -> str:
        """Determine recommended conversion strategy."""
        total = automated + manual
        if total == 0:
            return 'No GOTO statements found'

        auto_percentage = (automated / total * 100) if total > 0 else 0

        if auto_percentage >= 90:
            return 'Fully automated conversion recommended'
        elif auto_percentage >= 70:
            return 'Mostly automated with minor manual review'
        elif auto_percentage >= 50:
            return 'Mixed: automated for simple patterns, manual for complex'
        else:
            return 'Primarily manual conversion required'


def main():
    parser = argparse.ArgumentParser(
        description='Analyze GOTO patterns in Fortran code',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python fortran_analyze_goto.py input.f90
  python fortran_analyze_goto.py input.f90 -o report.json
  python fortran_analyze_goto.py input.f90 --summary

Classification:
  simple_forward  - Simple forward jump (single target, easy to convert)
  error_handling  - Jump to end of routine (convert to RETURN)
  backward_jump   - Jump to earlier line (loop-like, needs DO WHILE)
  multi_target    - Multiple GOTOs to same label (needs careful refactoring)
  computed_goto   - Computed GOTO (GOTO (10,20,30), I) - convert to SELECT CASE
  complex         - Other complex patterns (manual conversion)

Output:
  JSON report with pattern counts, recommendations, and line-by-line details
        """
    )

    parser.add_argument('input', help='Input file')
    parser.add_argument('-o', '--output', help='Output JSON file (default: stdout)')
    parser.add_argument('--summary', action='store_true', help='Print summary only (not full JSON)')

    args = parser.parse_args()

    # Open input
    try:
        with open(args.input, 'r') as f:
            lines = [line.rstrip('\n\r') for line in f]
    except IOError as e:
        print(f"Error opening input file: {e}", file=sys.stderr)
        return 1

    # Analyze
    analyzer = GOTOAnalyzer()
    report = analyzer.analyze_file(lines)
    report['file'] = args.input

    # Output
    if args.summary:
        # Print human-readable summary
        print(f"File: {report['file']}")
        print(f"Total GOTO statements: {report['total_goto_count']}")
        print()
        print("Pattern breakdown:")
        for pattern, count in report['patterns'].items():
            print(f"  {pattern:20s}: {count:4d}")
        print()
        print(f"Automated conversion: {report['recommendations']['automated']} statements")
        print(f"Manual conversion:    {report['recommendations']['manual']} statements")
        print()
        print(f"Strategy: {report['recommendations']['conversion_strategy']}")
    else:
        # Output full JSON
        if args.output:
            try:
                with open(args.output, 'w') as f:
                    json.dump(report, f, indent=2)
            except IOError as e:
                print(f"Error writing output file: {e}", file=sys.stderr)
                return 1
        else:
            print(json.dumps(report, indent=2))

    return 0


if __name__ == '__main__':
    sys.exit(main())
