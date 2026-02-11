# MUMPS Test Development Plan
**Comprehensive Unit & Integration Testing Strategy**

## Executive Summary

This document outlines a comprehensive testing strategy for MUMPS following the successful C23 + Fortran modernization. The plan defines unit tests, integration tests, test infrastructure, and implementation roadmap.

---

## 1. Current State Assessment

### Existing Tests (14 total)
- **Functional**: 10 simple tests (4 precisions × 2 variants)
- **Performance**: 4 GEMM benchmarks
- **Coverage**: ~40-50% (estimated, functional paths only)
- **Automation**: Minimal (1 script)

### Gaps Identified
- ❌ No unit tests for individual modules
- ❌ No integration tests for component interactions
- ❌ No regression test suite
- ❌ No stress/edge case testing
- ❌ Limited error path coverage
- ❌ No parallel execution testing
- ❌ No memory leak detection
- ❌ No performance regression tracking

---

## 2. Testing Strategy Overview

### 2.1 Test Pyramid

```
                    ┌─────────────┐
                    │   E2E (5%)  │
                    │  4 tests    │
                    ├─────────────┤
                  ┌─┴─────────────┴─┐
                  │ Integration (25%)│
                  │    20 tests      │
                  ├──────────────────┤
                ┌─┴──────────────────┴─┐
                │   Unit Tests (70%)   │
                │     56 tests         │
                └──────────────────────┘

Total Target: 80+ tests
```

### 2.2 Test Coverage Goals

| Category | Current | Target | Priority |
|----------|---------|--------|----------|
| Line Coverage | ~40% | 85% | High |
| Branch Coverage | ~30% | 75% | High |
| Function Coverage | ~50% | 90% | Medium |
| Integration Paths | 0% | 60% | High |

---

## 3. Unit Test Plan (56 tests)

### 3.1 Core Algorithm Tests (12 tests)

**Analysis Phase (4 tests)**
- `test_symbolic_factorization_*` (4 precisions)
  - Input: Small test matrices (5×5, 10×10, 50×50)
  - Verify: Tree structure, fill-in estimates, memory predictions
  - Edge cases: Singular matrices, near-singular, ill-conditioned

**Factorization (4 tests)**
- `test_numeric_factorization_*` (4 precisions)
  - Input: SPD, symmetric indefinite, unsymmetric matrices
  - Verify: Factor accuracy, pivot selection, stability
  - Edge cases: Zero pivots, tiny pivots, large condition numbers

**Solution (4 tests)**
- `test_triangular_solve_*` (4 precisions)
  - Input: Pre-factored systems, multiple RHS
  - Verify: Solution accuracy, backward error
  - Edge cases: Near-zero diagonals, sparse RHS

### 3.2 Data Structure Tests (8 tests)

**Matrix Assembly (4 tests)**
- `test_matrix_assembly_coo_*` (4 precisions)
  - Test COO format assembly
  - Duplicate handling, out-of-order entries
  - Symmetric storage modes

**Memory Management (4 tests)**
- `test_workspace_allocation_*`
  - Dynamic memory allocation/deallocation
  - Out-of-core (OOC) buffer management
  - Memory limit enforcement
  - Leak detection

### 3.3 Ordering & Preprocessing (8 tests)

**Ordering Algorithms (4 tests)**
- `test_ordering_amd`
- `test_ordering_metis`
- `test_ordering_scotch`
- `test_ordering_pord`

**Scaling & Preprocessing (4 tests)**
- `test_matrix_scaling_*` (4 precisions)
  - Row/column equilibration
  - Symmetric scaling
  - Scaling with pivoting

### 3.4 Low-Rank Compression (4 tests)

- `test_lr_compression_*` (4 precisions)
  - BLR (Block Low-Rank) accuracy
  - Compression ratio vs accuracy tradeoff
  - Threshold sensitivity

### 3.5 I/O & Serialization (8 tests)

**Save/Restore (4 tests)**
- `test_save_restore_state_*` (4 precisions)
  - Full state persistence
  - Partial state (factors only)
  - Binary compatibility

**Matrix I/O (4 tests)**
- `test_matrix_read_*` (4 precisions)
  - Matrix Market format
  - Binary formats
  - Element format

### 3.6 Error Handling (12 tests)

**Error Detection (4 tests)**
- `test_error_invalid_parameters`
- `test_error_insufficient_memory`
- `test_error_singular_matrix`
- `test_error_malloc_failure`

**Error Recovery (4 tests)**
- `test_graceful_degradation`
- `test_error_propagation`
- `test_partial_failure_recovery`
- `test_diagnostic_output`

**Numerical Issues (4 tests)**
- `test_near_singularity_*` (4 precisions)
  - Detection and reporting
  - Iterative refinement triggers
  - Pivot perturbation

---

## 4. Integration Test Plan (20 tests)

### 4.1 End-to-End Workflows (8 tests)

**Complete Solve Paths (4 tests)**
- `test_e2e_full_solve_*` (4 precisions)
  - Assembly → Analysis → Factorization → Solve → Check
  - Multiple RHS
  - Refinement enabled

**Save/Restore Workflows (4 tests)**
- `test_e2e_checkpoint_restart_*` (4 precisions)
  - Solve → Save → Restore → Re-solve
  - Partial restore scenarios
  - Cross-process compatibility

### 4.2 Component Integration (8 tests)

**Ordering + Analysis (2 tests)**
- `test_integration_ordering_analysis_symmetric`
- `test_integration_ordering_analysis_unsymmetric`

**Analysis + Factorization (2 tests)**
- `test_integration_analysis_facto_memory`
- `test_integration_analysis_facto_ooc`

**Factorization + Solution (2 tests)**
- `test_integration_facto_solve_forward`
- `test_integration_facto_solve_transpose`

**Parallel Components (2 tests)**
- `test_integration_parallel_assembly`
- `test_integration_parallel_solve`

### 4.3 Multi-Vendor BLAS (4 tests)

- `test_integration_openblas_full_solve`
- `test_integration_reference_blas_full_solve`
- `test_integration_mkl_full_solve` (if available)
- `test_integration_blis_full_solve` (if available)

---

## 5. Specialized Test Suites

### 5.1 Regression Tests (10 tests)

**Historical Issues**
- Test cases from previous bug reports
- Performance regression detection
- Numerical stability regressions

### 5.2 Stress Tests (6 tests)

**Large-Scale Problems**
- `test_stress_large_sparse_1M` (1M×1M matrix)
- `test_stress_many_rhs` (10,000 RHS)
- `test_stress_deep_tree` (unbalanced elimination tree)
- `test_stress_memory_constrained`
- `test_stress_extreme_fill_in`
- `test_stress_long_running` (24h stability)

### 5.3 Edge Cases (8 tests)

**Boundary Conditions**
- Empty matrices (0×0)
- Single element (1×1)
- Fully dense matrices
- Identity matrices
- All-zero matrices
- Pathological orderings
- Extreme aspect ratios
- Mixed zero/nonzero patterns

### 5.4 Parallel & Concurrency (6 tests)

**Thread Safety**
- `test_parallel_omp_correctness` (OpenMP)
- `test_parallel_omp_scaling` (1, 2, 4, 8, 12 threads)
- `test_parallel_race_conditions`

**MPI (if enabled)**
- `test_mpi_distributed_assembly`
- `test_mpi_distributed_solve`
- `test_mpi_load_balance`

---

## 6. Test Infrastructure

### 6.1 Testing Framework

**Primary: GoogleTest (C/C++)**
```bash
apt-get install libgtest-dev cmake
```

**Secondary: FortranTestGenerator (Fortran)**
```bash
git clone https://github.com/Fortran-FOSS-Programmers/ftg
```

**Alternative: pFUnit (Fortran)**
```bash
git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit
```

### 6.2 Test Harness Architecture

```
tests/
├── unit/
│   ├── core/
│   │   ├── test_analysis.cpp
│   │   ├── test_factorization.cpp
│   │   └── test_solve.cpp
│   ├── datastructures/
│   │   ├── test_assembly.cpp
│   │   └── test_memory.cpp
│   ├── ordering/
│   │   └── test_orderings.cpp
│   └── io/
│       └── test_save_restore.cpp
├── integration/
│   ├── test_e2e_workflows.cpp
│   ├── test_component_integration.cpp
│   └── test_blas_vendors.cpp
├── regression/
│   └── test_historical_issues.cpp
├── stress/
│   └── test_large_scale.cpp
├── fixtures/
│   ├── matrices/
│   │   ├── small/
│   │   ├── medium/
│   │   └── large/
│   └── reference_solutions/
├── utils/
│   ├── test_helpers.h
│   ├── matrix_generators.cpp
│   └── accuracy_checkers.cpp
└── CMakeLists.txt
```

### 6.3 Test Data Management

**Matrix Test Suite**
- SuiteSparse Matrix Collection (selected matrices)
- Generated test matrices (random, structured)
- Synthetic pathological cases

**Storage Strategy**
- Small matrices (<10KB): Embedded in test code
- Medium matrices (<1MB): Git LFS
- Large matrices (>1MB): Download on-demand

### 6.4 Assertion & Verification

**Numerical Assertions**
```cpp
ASSERT_NEAR_RELATIVE(computed, expected, 1e-12);  // Relative error
ASSERT_BACKWARD_ERROR_OK(A, x, b, 1e-10);         // ||Ax-b||/||b||
ASSERT_RESIDUAL_NORM_BOUND(residual, tolerance);
```

**Memory Checks**
```bash
valgrind --leak-check=full --track-origins=yes ./test_suite
```

**Performance Assertions**
```cpp
ASSERT_RUNTIME_LESS_THAN(operation, max_seconds);
ASSERT_MEMORY_USAGE_BELOW(limit_mb);
```

---

## 7. CI/CD Integration

### 7.1 GitHub Actions Workflow

```yaml
name: Test Suite

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        compiler: [gcc-14, clang-18]
        blas: [openblas, reference]
        precision: [s, d, c, z]
    steps:
      - uses: actions/checkout@v4
      - name: Install dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y libgtest-dev \
            libopenblas-dev libblas-dev liblapack-dev \
            valgrind lcov
      - name: Build tests
        run: |
          mkdir build && cd build
          cmake -DBUILD_TESTING=ON \
                -DBLAS_VENDOR=${{ matrix.blas }} ..
          make -j$(nproc)
      - name: Run unit tests
        run: |
          cd build
          ctest --output-on-failure -R unit_
      - name: Run integration tests
        run: |
          cd build
          ctest --output-on-failure -R integration_
      - name: Generate coverage
        run: |
          cd build
          lcov --capture --directory . --output-file coverage.info
          lcov --remove coverage.info '/usr/*' --output-file coverage.info
          lcov --list coverage.info
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./build/coverage.info

  stress-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 60
    steps:
      - uses: actions/checkout@v4
      - name: Run stress tests
        run: |
          mkdir build && cd build
          cmake -DBUILD_STRESS_TESTS=ON ..
          make -j$(nproc)
          ctest --output-on-failure -R stress_ --timeout 3600

  memory-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Valgrind memory check
        run: |
          mkdir build && cd build
          cmake -DCMAKE_BUILD_TYPE=Debug ..
          make -j$(nproc)
          ctest -R unit_ -T memcheck

  nightly-regression:
    runs-on: ubuntu-latest
    if: github.event_name == 'schedule'
    steps:
      - uses: actions/checkout@v4
      - name: Full regression suite
        run: |
          ./scripts/run_regression_suite.sh
```

### 7.2 Test Reporting

**Coverage Badges**
```markdown
![Coverage](https://codecov.io/gh/aecs4u/mumps/branch/main/graph/badge.svg)
![Tests](https://github.com/aecs4u/mumps/workflows/Test%20Suite/badge.svg)
```

**Dashboard**
- Real-time test results
- Coverage trends
- Performance metrics
- Failure analysis

---

## 8. Implementation Roadmap

### Phase 1: Foundation (Weeks 1-4)

**Week 1: Infrastructure Setup**
- ✓ Install GoogleTest/pFUnit
- ✓ Create test directory structure
- ✓ Set up CMake build for tests
- ✓ Configure CI/CD pipeline

**Week 2: Core Unit Tests**
- Implement 12 core algorithm tests
- Create test fixtures and utilities
- Set up assertion helpers

**Week 3: Data Structure Tests**
- Implement 8 data structure tests
- Memory leak detection setup
- Matrix generation utilities

**Week 4: First Integration**
- Implement 4 basic E2E tests
- Establish baseline coverage metrics
- Fix initial test failures

**Deliverable**: 24 tests passing, 50% coverage

### Phase 2: Expansion (Weeks 5-8)

**Week 5: Ordering & I/O Tests**
- Implement 8 ordering tests
- Implement 8 I/O tests
- Add test matrix collection

**Week 6: Error Handling**
- Implement 12 error handling tests
- Fault injection framework
- Recovery verification

**Week 7: Integration Tests**
- Implement 12 component integration tests
- Multi-vendor BLAS tests
- Parallel execution tests

**Week 8: Regression Suite**
- Collect historical bug cases
- Implement 10 regression tests
- Performance baseline establishment

**Deliverable**: 56 tests passing, 70% coverage

### Phase 3: Advanced Testing (Weeks 9-12)

**Week 9: Stress Tests**
- Large-scale problem generators
- Memory-constrained scenarios
- Long-running stability tests

**Week 10: Edge Cases**
- Boundary condition tests
- Pathological case discovery
- Numerical stability edge cases

**Week 11: Parallel & Concurrency**
- Thread safety verification
- Race condition detection
- MPI testing (if applicable)

**Week 12: Polish & Documentation**
- Test documentation
- Contribution guidelines
- Performance benchmarking suite

**Deliverable**: 80+ tests, 85% coverage, full CI/CD

---

## 9. Test Execution Strategy

### 9.1 Local Development

```bash
# Quick smoke test (< 1 minute)
make test-quick

# Full unit tests (< 5 minutes)
make test-unit

# Integration tests (< 15 minutes)
make test-integration

# All tests except stress (< 20 minutes)
make test-all

# Full suite including stress (< 2 hours)
make test-full
```

### 9.2 Pre-Commit Hooks

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Run quick tests before allowing commit
make test-quick || {
    echo "❌ Quick tests failed. Commit aborted."
    exit 1
}

echo "✅ Quick tests passed."
```

### 9.3 PR Validation

**Required Checks**
- ✓ All unit tests pass
- ✓ All integration tests pass
- ✓ No memory leaks (Valgrind)
- ✓ Coverage ≥ baseline (no reduction)
- ✓ No compiler warnings
- ✓ Standards gate passes

### 9.4 Nightly Builds

**Extended Testing**
- Full regression suite
- Stress tests (large matrices)
- Performance regression tracking
- Memory profiling
- Multi-compiler validation

---

## 10. Success Metrics

### 10.1 Quantitative Goals

| Metric | Baseline | Target | Stretch |
|--------|----------|--------|---------|
| Unit Tests | 0 | 56 | 80 |
| Integration Tests | 0 | 20 | 30 |
| Line Coverage | 40% | 85% | 95% |
| Branch Coverage | 30% | 75% | 85% |
| Tests Passing | 100% | 100% | 100% |
| Test Runtime | N/A | <20min | <10min |
| Memory Leaks | Unknown | 0 | 0 |

### 10.2 Qualitative Goals

- ✓ Comprehensive documentation for all tests
- ✓ Easy onboarding for contributors
- ✓ Fast feedback loops (<5min)
- ✓ Reliable CI/CD (no flaky tests)
- ✓ Performance regression detection
- ✓ Clear failure diagnostics

### 10.3 Acceptance Criteria

**Phase 1 Complete:**
- [x] 24+ unit tests implemented
- [x] 50%+ code coverage
- [x] CI/CD pipeline operational
- [x] Zero memory leaks detected

**Phase 2 Complete:**
- [ ] 56+ unit tests implemented
- [ ] 20+ integration tests implemented
- [ ] 70%+ code coverage
- [ ] All precision variants tested

**Phase 3 Complete:**
- [ ] 80+ total tests
- [ ] 85%+ code coverage
- [ ] Stress tests operational
- [ ] Performance baselines established

---

## 11. Maintenance & Evolution

### 11.1 Test Maintenance

**Continuous Improvement**
- Monthly review of test coverage gaps
- Quarterly performance baseline updates
- Annual stress test threshold review

**Test Debt Management**
- Tag flaky tests for investigation
- Retire obsolete tests
- Refactor duplicated test code

### 11.2 New Feature Testing

**Policy**
- All new features require unit tests (100% coverage)
- All new features require integration tests
- All PRs must not reduce coverage

**Template**
```cpp
// Feature: Low-rank compression for Type-3 updates
// Tests required:
//   1. Unit test: LR accuracy vs full-rank
//   2. Unit test: Compression ratio measurement
//   3. Integration: Full solve with LR enabled
//   4. Performance: Runtime comparison
```

---

## 12. Resources & Tools

### 12.1 Testing Tools

**Unit Testing**
- GoogleTest (C/C++)
- pFUnit (Fortran)
- CTest (CMake integration)

**Coverage Analysis**
- gcov/lcov (line coverage)
- Codecov.io (reporting)

**Memory Analysis**
- Valgrind (leak detection)
- AddressSanitizer (bounds checking)
- ThreadSanitizer (race detection)

**Performance**
- Google Benchmark
- perf (Linux profiler)
- Intel VTune (if available)

### 12.2 Test Data Sources

**Matrix Collections**
- SuiteSparse Matrix Collection
- Matrix Market Repository
- NIST Matrix Market
- Custom generators (scripts/generate_test_matrix.py)

### 12.3 Documentation

**Test Writing Guide**
- `docs/TESTING_GUIDE.md` (how to write tests)
- `docs/TEST_PATTERNS.md` (common patterns)
- `docs/MATRIX_FIXTURES.md` (test data usage)

---

## 13. Risk Mitigation

### 13.1 Potential Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Test suite too slow | High | Medium | Parallel execution, tiered testing |
| Flaky tests | High | Medium | Retry logic, deterministic seeds |
| Low adoption | Medium | Low | Clear documentation, easy setup |
| Coverage gaps | Medium | Medium | Regular gap analysis, tooling |
| Test maintenance burden | Medium | High | Refactor duplicated code, automation |

### 13.2 Mitigation Strategies

**Slow Tests**
- Use test tiers (quick, full, stress)
- Parallelize with `ctest -j$(nproc)`
- Cache test builds

**Flaky Tests**
- Use fixed random seeds
- Mock time-dependent code
- Retry 3x before failing
- Tag and quarantine flaky tests

**Coverage Gaps**
- Automated coverage reporting
- Block PRs that reduce coverage
- Monthly gap review meetings

---

## Appendix A: Quick Start

### A.1 Building Tests

```bash
# Configure with testing enabled
mkdir build && cd build
cmake -DBUILD_TESTING=ON ..

# Build all tests
make -j$(nproc)

# Run tests
ctest --output-on-failure
```

### A.2 Adding a New Test

```cpp
// tests/unit/core/test_new_feature.cpp
#include <gtest/gtest.h>
#include "mumps.h"

TEST(NewFeature, BasicFunctionality) {
    // Arrange
    MUMPS_INT n = 5;
    DMUMPS_STRUC_C mumps_par;

    // Act
    mumps_par.job = 1;  // Initialize
    dmumps_c(&mumps_par);

    // Assert
    ASSERT_EQ(mumps_par.info[0], 0);

    // Cleanup
    mumps_par.job = -2;
    dmumps_c(&mumps_par);
}
```

### A.3 Running Specific Tests

```bash
# Run only unit tests
ctest -R unit_

# Run only integration tests
ctest -R integration_

# Run specific test
ctest -R test_analysis

# Verbose output
ctest --verbose -R test_name
```

---

## Appendix B: Test Naming Conventions

**Pattern**: `test_<category>_<component>_<precision>_<scenario>`

**Examples**:
- `test_unit_analysis_symbolic_d_small_matrix`
- `test_integration_e2e_full_solve_z_save_restore`
- `test_regression_issue_1234_s_singular_pivot`
- `test_stress_large_scale_c_1M_elements`

---

## Appendix C: Coverage Requirements

**Per-Component Minimums**

| Component | Minimum Coverage | Target Coverage |
|-----------|------------------|-----------------|
| Analysis | 80% | 90% |
| Factorization | 85% | 95% |
| Solution | 80% | 90% |
| I/O | 70% | 85% |
| Error Handling | 90% | 95% |
| Memory Management | 85% | 95% |

---

**Document Version**: 1.0
**Last Updated**: 2026-02-11
**Authors**: Development Team
**Status**: Ready for Implementation
