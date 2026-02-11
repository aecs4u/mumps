"""
pytest plugin to store test results in SQLite database.

This plugin captures test results and stores them in mumps.sqlite for:
- Historical tracking
- Performance analysis
- Vendor comparison
- Regression detection
"""

import sqlite3
import datetime
import os
import platform
import pytest
from pathlib import Path


class SQLiteResultsPlugin:
    """Plugin to store pytest results in SQLite database."""

    def __init__(self, db_path):
        self.db_path = db_path
        self.conn = None
        self.session_id = None
        self.test_results = []

    def pytest_configure(self, config):
        """Initialize database and create tables."""
        self.conn = sqlite3.connect(self.db_path)
        self._create_tables()

    def pytest_sessionstart(self, session):
        """Record session start."""
        cursor = self.conn.cursor()

        # Get system information
        blas_vendor = os.environ.get('BLAS_VENDOR', 'unknown')
        python_version = platform.python_version()
        platform_info = platform.platform()

        cursor.execute('''
            INSERT INTO test_sessions (
                timestamp,
                python_version,
                platform,
                blas_vendor,
                num_tests
            ) VALUES (?, ?, ?, ?, ?)
        ''', (
            datetime.datetime.now().isoformat(),
            python_version,
            platform_info,
            blas_vendor,
            0  # Will be updated at session end
        ))

        self.session_id = cursor.lastrowid
        self.conn.commit()

    def pytest_runtest_logreport(self, report):
        """Record test result."""
        if report.when == 'call':  # Only record actual test execution, not setup/teardown
            test_name = report.nodeid
            status = report.outcome
            duration = report.duration
            error_msg = str(report.longrepr) if report.failed else None

            # Extract test class and function
            parts = test_name.split('::')
            test_file = parts[0] if len(parts) > 0 else ''
            test_class = parts[1] if len(parts) > 1 else ''
            test_function = parts[2] if len(parts) > 2 else parts[1] if len(parts) > 1 else ''

            cursor = self.conn.cursor()
            cursor.execute('''
                INSERT INTO test_results (
                    session_id,
                    test_name,
                    test_file,
                    test_class,
                    test_function,
                    status,
                    duration,
                    error_message,
                    timestamp
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                self.session_id,
                test_name,
                test_file,
                test_class,
                test_function,
                status,
                duration,
                error_msg,
                datetime.datetime.now().isoformat()
            ))
            self.conn.commit()

    def pytest_sessionfinish(self, session):
        """Update session with final statistics."""
        if self.session_id and self.conn:
            cursor = self.conn.cursor()

            # Get test counts
            cursor.execute('''
                SELECT
                    COUNT(*) as total,
                    SUM(CASE WHEN status = 'passed' THEN 1 ELSE 0 END) as passed,
                    SUM(CASE WHEN status = 'failed' THEN 1 ELSE 0 END) as failed,
                    SUM(CASE WHEN status = 'skipped' THEN 1 ELSE 0 END) as skipped,
                    SUM(duration) as total_duration
                FROM test_results
                WHERE session_id = ?
            ''', (self.session_id,))

            result = cursor.fetchone()
            total, passed, failed, skipped, total_duration = result

            # Update session
            cursor.execute('''
                UPDATE test_sessions
                SET num_tests = ?,
                    num_passed = ?,
                    num_failed = ?,
                    num_skipped = ?,
                    total_duration = ?
                WHERE id = ?
            ''', (total, passed, failed, skipped, total_duration, self.session_id))

            self.conn.commit()

    def pytest_unconfigure(self, config):
        """Close database connection."""
        if self.conn:
            self.conn.close()

    def _create_tables(self):
        """Create database tables if they don't exist."""
        cursor = self.conn.cursor()

        # Test sessions table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS test_sessions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                python_version TEXT,
                platform TEXT,
                blas_vendor TEXT,
                num_tests INTEGER,
                num_passed INTEGER DEFAULT 0,
                num_failed INTEGER DEFAULT 0,
                num_skipped INTEGER DEFAULT 0,
                total_duration REAL DEFAULT 0.0
            )
        ''')

        # Test results table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS test_results (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                session_id INTEGER,
                test_name TEXT NOT NULL,
                test_file TEXT,
                test_class TEXT,
                test_function TEXT,
                status TEXT NOT NULL,
                duration REAL,
                error_message TEXT,
                timestamp TEXT NOT NULL,
                FOREIGN KEY (session_id) REFERENCES test_sessions(id)
            )
        ''')

        # Create indices for faster queries
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_test_results_session
            ON test_results(session_id)
        ''')

        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_test_results_name
            ON test_results(test_name)
        ''')

        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_test_results_status
            ON test_results(status)
        ''')

        self.conn.commit()


def pytest_configure(config):
    """Register the plugin."""
    # Get database path from config or use default
    db_path = config.getoption('--sqlite-db', default='../mumps.sqlite')

    # Make path absolute relative to tests directory
    if not os.path.isabs(db_path):
        tests_dir = Path(__file__).parent
        db_path = tests_dir / db_path

    plugin = SQLiteResultsPlugin(str(db_path))
    config.pluginmanager.register(plugin, 'sqlite_results')


def pytest_addoption(parser):
    """Add command-line option for database path."""
    parser.addoption(
        '--sqlite-db',
        action='store',
        default='../mumps.sqlite',
        help='Path to SQLite database for storing test results'
    )
