#!/usr/bin/env python3
"""
Analyze MUMPS test results from SQLite database.

This script provides various analyses of test results:
- Latest test session summary
- Historical trends
- Performance regressions
- Vendor comparisons
- Failure analysis
"""

import sqlite3
import sys
from pathlib import Path
from datetime import datetime
import argparse


class TestResultsAnalyzer:
    """Analyzer for MUMPS test results."""

    def __init__(self, db_path='mumps.sqlite'):
        self.db_path = db_path
        self.conn = sqlite3.connect(db_path)
        self.conn.row_factory = sqlite3.Row

    def latest_session_summary(self):
        """Print summary of latest test session."""
        cursor = self.conn.cursor()

        cursor.execute('''
            SELECT * FROM test_sessions
            ORDER BY id DESC
            LIMIT 1
        ''')

        session = cursor.fetchone()
        if not session:
            print("No test sessions found in database.")
            return

        print("=" * 70)
        print("LATEST TEST SESSION SUMMARY")
        print("=" * 70)
        print(f"Session ID:      {session['id']}")
        print(f"Timestamp:       {session['timestamp']}")
        print(f"Python Version:  {session['python_version']}")
        print(f"Platform:        {session['platform']}")
        print(f"BLAS Vendor:     {session['blas_vendor']}")
        print(f"Total Tests:     {session['num_tests']}")
        print(f"Passed:          {session['num_passed']} ({100*session['num_passed']/session['num_tests']:.1f}%)")
        print(f"Failed:          {session['num_failed']}")
        print(f"Skipped:         {session['num_skipped']}")
        print(f"Total Duration:  {session['total_duration']:.3f}s")
        print("=" * 70)

        # Show failures if any
        if session['num_failed'] > 0:
            print("\nFAILED TESTS:")
            cursor.execute('''
                SELECT test_name, duration, error_message
                FROM test_results
                WHERE session_id = ? AND status = 'failed'
            ''', (session['id'],))

            for row in cursor.fetchall():
                print(f"\n  ✗ {row['test_name']} ({row['duration']:.3f}s)")
                if row['error_message']:
                    # Print first 200 chars of error
                    error = row['error_message'][:200]
                    print(f"    Error: {error}...")

    def compare_sessions(self, session1_id, session2_id):
        """Compare two test sessions."""
        cursor = self.conn.cursor()

        for sid in [session1_id, session2_id]:
            cursor.execute('SELECT * FROM test_sessions WHERE id = ?', (sid,))
            session = cursor.fetchone()
            if not session:
                print(f"Session {sid} not found.")
                return

        cursor.execute('''
            SELECT
                s1.timestamp as ts1,
                s2.timestamp as ts2,
                s1.blas_vendor as vendor1,
                s2.blas_vendor as vendor2,
                s1.total_duration as dur1,
                s2.total_duration as dur2,
                s1.num_passed as pass1,
                s2.num_passed as pass2,
                s1.num_failed as fail1,
                s2.num_failed as fail2
            FROM test_sessions s1, test_sessions s2
            WHERE s1.id = ? AND s2.id = ?
        ''', (session1_id, session2_id))

        row = cursor.fetchone()

        print(f"\nSESSION COMPARISON: {session1_id} vs {session2_id}")
        print("=" * 70)
        print(f"{'Metric':<30} {'Session ' + str(session1_id):<20} {'Session ' + str(session2_id):<20}")
        print("-" * 70)
        print(f"{'Timestamp':<30} {row['ts1']:<20} {row['ts2']:<20}")
        print(f"{'BLAS Vendor':<30} {row['vendor1']:<20} {row['vendor2']:<20}")
        print(f"{'Total Duration':<30} {row['dur1']:.3f}s{'':<14} {row['dur2']:.3f}s")
        print(f"{'Passed':<30} {row['pass1']:<20} {row['pass2']:<20}")
        print(f"{'Failed':<30} {row['fail1']:<20} {row['fail2']:<20}")

        # Performance difference
        if row['dur1'] > 0:
            speedup = ((row['dur1'] - row['dur2']) / row['dur1']) * 100
            print(f"{'Performance Change':<30} {speedup:+.1f}%")

    def vendor_comparison(self):
        """Compare performance across BLAS vendors."""
        cursor = self.conn.cursor()

        cursor.execute('''
            SELECT
                blas_vendor,
                COUNT(*) as num_sessions,
                AVG(total_duration) as avg_duration,
                AVG(num_passed * 100.0 / num_tests) as avg_pass_rate
            FROM test_sessions
            WHERE blas_vendor != 'unknown'
            GROUP BY blas_vendor
            ORDER BY avg_duration
        ''')

        results = cursor.fetchall()
        if not results:
            print("No vendor data available.")
            return

        print("\nBLAS VENDOR COMPARISON")
        print("=" * 70)
        print(f"{'Vendor':<20} {'Sessions':<12} {'Avg Duration':<15} {'Avg Pass Rate':<15}")
        print("-" * 70)

        for row in results:
            print(f"{row['blas_vendor']:<20} {row['num_sessions']:<12} "
                  f"{row['avg_duration']:.3f}s{'':<9} {row['avg_pass_rate']:.1f}%")

    def performance_regression(self, threshold=10.0):
        """Detect performance regressions between consecutive sessions."""
        cursor = self.conn.cursor()

        cursor.execute('''
            SELECT
                t1.test_name,
                s1.id as session1,
                s2.id as session2,
                t1.duration as dur1,
                t2.duration as dur2,
                s1.timestamp as ts1,
                s2.timestamp as ts2,
                ((t2.duration - t1.duration) / t1.duration * 100) as pct_change
            FROM test_results t1
            JOIN test_results t2 ON t1.test_name = t2.test_name
            JOIN test_sessions s1 ON t1.session_id = s1.id
            JOIN test_sessions s2 ON t2.session_id = s2.id
            WHERE s2.id = s1.id + 1
              AND t1.duration > 0
              AND ((t2.duration - t1.duration) / t1.duration * 100) > ?
            ORDER BY pct_change DESC
        ''', (threshold,))

        results = cursor.fetchall()
        if not results:
            print(f"\nNo performance regressions detected (threshold: {threshold}%)")
            return

        print(f"\nPERFORMANCE REGRESSIONS (>{threshold}% slower)")
        print("=" * 70)
        print(f"{'Test Name':<40} {'Sessions':<15} {'% Change':<15}")
        print("-" * 70)

        for row in results:
            print(f"{row['test_name']:<40} {row['session1']}->{row['session2']:<11} "
                  f"{row['pct_change']:+.1f}%")

    def test_history(self, test_name):
        """Show history for a specific test."""
        cursor = self.conn.cursor()

        cursor.execute('''
            SELECT
                s.id as session_id,
                s.timestamp,
                s.blas_vendor,
                r.status,
                r.duration
            FROM test_results r
            JOIN test_sessions s ON r.session_id = s.id
            WHERE r.test_name = ?
            ORDER BY s.id DESC
            LIMIT 20
        ''', (test_name,))

        results = cursor.fetchall()
        if not results:
            print(f"No history found for test: {test_name}")
            return

        print(f"\nTEST HISTORY: {test_name}")
        print("=" * 70)
        print(f"{'Session':<10} {'Timestamp':<25} {'Vendor':<15} {'Status':<10} {'Duration':<10}")
        print("-" * 70)

        for row in results:
            status_symbol = "✓" if row['status'] == 'passed' else "✗"
            print(f"{row['session_id']:<10} {row['timestamp']:<25} {row['blas_vendor']:<15} "
                  f"{status_symbol} {row['status']:<8} {row['duration']:.3f}s")

    def statistics(self):
        """Show overall statistics."""
        cursor = self.conn.cursor()

        cursor.execute('SELECT COUNT(*) as total FROM test_sessions')
        total_sessions = cursor.fetchone()['total']

        cursor.execute('SELECT COUNT(*) as total FROM test_results')
        total_results = cursor.fetchone()['total']

        cursor.execute('''
            SELECT
                MIN(timestamp) as first_run,
                MAX(timestamp) as last_run
            FROM test_sessions
        ''')
        dates = cursor.fetchone()

        cursor.execute('''
            SELECT AVG(num_passed * 100.0 / num_tests) as avg_pass_rate
            FROM test_sessions
        ''')
        avg_pass_rate = cursor.fetchone()['avg_pass_rate']

        print("\nOVERALL STATISTICS")
        print("=" * 70)
        print(f"Total Test Sessions:     {total_sessions}")
        print(f"Total Test Results:      {total_results}")
        print(f"Average Pass Rate:       {avg_pass_rate:.1f}%")
        print(f"First Test Run:          {dates['first_run']}")
        print(f"Last Test Run:           {dates['last_run']}")

    def close(self):
        """Close database connection."""
        self.conn.close()


def main():
    parser = argparse.ArgumentParser(description='Analyze MUMPS test results')
    parser.add_argument('--db', default='mumps.sqlite', help='Path to SQLite database')
    parser.add_argument('--latest', action='store_true', help='Show latest session summary')
    parser.add_argument('--compare', nargs=2, type=int, metavar=('ID1', 'ID2'),
                        help='Compare two sessions')
    parser.add_argument('--vendors', action='store_true', help='Compare BLAS vendors')
    parser.add_argument('--regression', action='store_true', help='Detect performance regressions')
    parser.add_argument('--history', metavar='TEST_NAME', help='Show test history')
    parser.add_argument('--stats', action='store_true', help='Show overall statistics')
    parser.add_argument('--all', action='store_true', help='Show all analyses')

    args = parser.parse_args()

    # Check if database exists
    if not Path(args.db).exists():
        print(f"Database not found: {args.db}")
        print("Run tests first to create the database:")
        print("  LD_LIBRARY_PATH=lib:PORD/lib uv run pytest tests/ -v")
        sys.exit(1)

    analyzer = TestResultsAnalyzer(args.db)

    try:
        if args.all or args.latest:
            analyzer.latest_session_summary()

        if args.all or args.stats:
            analyzer.statistics()

        if args.all or args.vendors:
            analyzer.vendor_comparison()

        if args.all or args.regression:
            analyzer.performance_regression()

        if args.compare:
            analyzer.compare_sessions(args.compare[0], args.compare[1])

        if args.history:
            analyzer.test_history(args.history)

        # Default action if no arguments
        if not any([args.latest, args.compare, args.vendors, args.regression,
                    args.history, args.stats, args.all]):
            analyzer.latest_session_summary()

    finally:
        analyzer.close()


if __name__ == '__main__':
    main()
