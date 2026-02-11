# Test Language Selection Analysis for MUMPS

## Executive Summary

**Recommendation**: **Multi-language hybrid approach**
- **Primary**: C++ with GoogleTest (70% of tests)
- **Secondary**: Fortran with pFUnit (20% of tests)
- **Tertiary**: Python with pytest (10% of tests)

**Rationale**: Leverage each language's strengths while maintaining the core in C++ for speed, tooling, and CI/CD integration.

---

## 1. Language Options Analysis

### Option 1: Fortran (pFUnit)

#### ✅ Advantages
- **Native Language Match**: MUMPS core is Fortran
- **Direct API Access**: No FFI overhead
- **Type Safety**: Compile-time type checking
- **Precision Control**: Native REAL/DOUBLE PRECISION handling
- **Compiler Integration**: Same toolchain as production code

#### ❌ Disadvantages
- **Limited Ecosystem**: Fewer testing frameworks
- **Tooling**: Weaker IDE support, debugging tools
- **Modern Features**: Fortran 2023 features not widely supported
- **CI/CD**: Less mature integration (no native GitHub Actions)
- **Learning Curve**: Fewer developers familiar with Fortran testing
- **Mocking/Stubbing**: Very limited support
- **Coverage Tools**: Fewer options (gcov works but limited)

#### 📊 Framework: pFUnit
```fortran
@test
subroutine test_matrix_assembly()
    use pFUnit_mod
    use mumps_mod
    implicit none

    type(DMUMPS_STRUC) :: mumps_par
    integer :: info

    ! Arrange
    mumps_par%n = 5
    mumps_par%nnz = 10

    ! Act
    mumps_par%job = 1
    call dmumps(mumps_par)

    ! Assert
    @assertEqual(0, mumps_par%info(1))

    ! Cleanup
    mumps_par%job = -2
    call dmumps(mumps_par)
end subroutine
```

**Complexity**: ⭐⭐⭐☆☆ (3/5)
**Speed**: ⭐⭐⭐⭐⭐ (5/5 - native, no overhead)
**Tooling**: ⭐⭐☆☆☆ (2/5)
**CI/CD**: ⭐⭐⭐☆☆ (3/5)

---

### Option 2: Python (pytest + NumPy)

#### ✅ Advantages
- **Developer Friendly**: Easy to write and maintain
- **Rich Ecosystem**: pytest, hypothesis, tox, coverage.py
- **Fast Iteration**: No compilation, quick feedback
- **NumPy Integration**: Excellent matrix handling
- **Fixtures**: Powerful test data management
- **CI/CD**: Excellent GitHub Actions support
- **Mocking**: Extensive mocking capabilities
- **Reporting**: Beautiful HTML reports, coverage badges

#### ❌ Disadvantages
- **FFI Overhead**: ctypes/cffi performance penalty
- **Type Mismatches**: Python types ↔ Fortran types conversion
- **Precision Issues**: Potential float64 vs DOUBLE PRECISION confusion
- **Wrapper Maintenance**: Requires Python bindings
- **Runtime Errors**: No compile-time checks
- **Debugging**: Harder to debug Fortran crashes from Python
- **Dependencies**: Requires NumPy, SciPy, etc.

#### 📊 Framework: pytest
```python
import pytest
import numpy as np
from pymumps import DMUMPS  # Hypothetical wrapper

def test_matrix_assembly():
    # Arrange
    n = 5
    A = np.array([[2, 3], [3, 2]], dtype=np.float64)
    b = np.array([8, 13], dtype=np.float64)

    # Act
    mumps = DMUMPS()
    mumps.set_matrix(A)
    mumps.set_rhs(b)
    x = mumps.solve()

    # Assert
    np.testing.assert_allclose(x, [4.6, -0.4], rtol=1e-12)
    assert mumps.info[0] == 0

@pytest.fixture
def sample_matrix():
    return np.random.randn(100, 100)
```

**Complexity**: ⭐⭐☆☆☆ (2/5 - easiest)
**Speed**: ⭐⭐⭐☆☆ (3/5 - FFI overhead)
**Tooling**: ⭐⭐⭐⭐⭐ (5/5 - excellent)
**CI/CD**: ⭐⭐⭐⭐⭐ (5/5 - excellent)

---

### Option 3: Rust (cargo test)

#### ✅ Advantages
- **Memory Safety**: Catch bugs at compile time
- **Modern Tooling**: cargo, rustfmt, clippy
- **Performance**: Zero-cost abstractions, fast execution
- **Concurrency**: Safe parallel testing
- **Error Handling**: Excellent Result/Option types
- **CI/CD**: Great GitHub Actions support
- **Ecosystem**: Growing scientific computing ecosystem

#### ❌ Disadvantages
- **Steep Learning Curve**: Complex borrow checker
- **FFI Complexity**: Unsafe blocks for C/Fortran interop
- **Limited Fortran Support**: Harder than C FFI
- **Ecosystem Immaturity**: Fewer scientific libraries
- **Team Expertise**: Requires Rust knowledge
- **Compilation Time**: Slower than C++
- **Overhead**: Marshaling Rust ↔ Fortran types

#### 📊 Framework: cargo test + ndarray
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use ndarray::{array, Array2};

    #[test]
    fn test_matrix_assembly() {
        // Arrange
        let mut mumps = DMumps::new();
        let a = array![[2.0, 3.0], [3.0, 2.0]];
        let b = array![8.0, 13.0];

        // Act
        mumps.set_matrix(&a);
        mumps.set_rhs(&b);
        let x = mumps.solve().expect("Solve failed");

        // Assert
        assert_eq!(mumps.info()[0], 0);
        assert!((x[0] - 4.6).abs() < 1e-12);
        assert!((x[1] + 0.4).abs() < 1e-12);
    }
}
```

**Complexity**: ⭐⭐⭐⭐☆ (4/5 - steep)
**Speed**: ⭐⭐⭐⭐⭐ (5/5 - native speed)
**Tooling**: ⭐⭐⭐⭐☆ (4/5 - modern)
**CI/CD**: ⭐⭐⭐⭐☆ (4/5 - good)

---

### Option 4: C++ (GoogleTest)

#### ✅ Advantages
- **Industry Standard**: GoogleTest is battle-tested
- **C Interop**: Easy to call C interfaces (MUMPS has C wrappers)
- **Performance**: Native speed, minimal overhead
- **Tooling**: Excellent (CLion, VS Code, gdb, valgrind)
- **CI/CD**: Mature GitHub Actions integration
- **Mocking**: GMock for complex scenarios
- **Coverage**: gcov/lcov integration
- **Assertions**: Rich assertion library
- **Fixtures**: Test fixtures, parameterized tests

#### ❌ Disadvantages
- **C++ Complexity**: Template errors, undefined behavior
- **Compilation Time**: Slower than Python
- **Verbosity**: More boilerplate than Python
- **Fortran Interop**: Requires extern "C" wrappers

#### 📊 Framework: GoogleTest + Eigen
```cpp
#include <gtest/gtest.h>
#include <Eigen/Dense>
extern "C" {
    #include "dmumps_c.h"
}

class MumpsTest : public ::testing::Test {
protected:
    DMUMPS_STRUC_C mumps_par;

    void SetUp() override {
        mumps_par.comm_fortran = 0;  // Sequential
        mumps_par.par = 1;
        mumps_par.sym = 0;
        mumps_par.job = -1;  // Initialize
        dmumps_c(&mumps_par);
    }

    void TearDown() override {
        mumps_par.job = -2;  // Destroy
        dmumps_c(&mumps_par);
    }
};

TEST_F(MumpsTest, MatrixAssembly) {
    // Arrange
    mumps_par.n = 2;
    mumps_par.nnz = 4;
    int irn[] = {1, 1, 2, 2};
    int jcn[] = {1, 2, 1, 2};
    double a[] = {2.0, 3.0, 3.0, 2.0};

    mumps_par.irn = irn;
    mumps_par.jcn = jcn;
    mumps_par.a = a;

    // Act: Analysis
    mumps_par.job = 1;
    dmumps_c(&mumps_par);

    // Assert
    ASSERT_EQ(mumps_par.info[0], 0);
    EXPECT_GT(mumps_par.infog[2], 0);  // Estimated real space
}

TEST_F(MumpsTest, FullSolve) {
    // Arrange
    Eigen::Matrix2d A;
    A << 2, 3, 3, 2;
    Eigen::Vector2d b(8, 13);

    // ... setup mumps_par ...

    // Act
    mumps_par.job = 6;  // Analyze + Factor + Solve
    dmumps_c(&mumps_par);

    // Assert
    ASSERT_EQ(mumps_par.info[0], 0);
    EXPECT_NEAR(mumps_par.rhs[0], 4.6, 1e-12);
    EXPECT_NEAR(mumps_par.rhs[1], -0.4, 1e-12);
}
```

**Complexity**: ⭐⭐⭐☆☆ (3/5)
**Speed**: ⭐⭐⭐⭐⭐ (5/5 - native)
**Tooling**: ⭐⭐⭐⭐⭐ (5/5 - excellent)
**CI/CD**: ⭐⭐⭐⭐⭐ (5/5 - excellent)

---

## 2. Comparative Analysis

### 2.1 Feature Matrix

| Feature | Fortran | Python | Rust | C++ |
|---------|---------|--------|------|-----|
| **Speed** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Ease of Use** | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐☆☆☆ | ⭐⭐⭐☆☆ |
| **Tooling** | ⭐⭐☆☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐☆ | ⭐⭐⭐⭐⭐ |
| **CI/CD** | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐☆ | ⭐⭐⭐⭐⭐ |
| **Native Interop** | ⭐⭐⭐⭐⭐ | ⭐⭐☆☆☆ | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐☆ |
| **Mocking** | ⭐☆☆☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐☆ | ⭐⭐⭐⭐⭐ |
| **Coverage** | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐☆ | ⭐⭐⭐⭐⭐ |
| **Team Familiarity** | ⭐⭐☆☆☆ | ⭐⭐⭐⭐⭐ | ⭐⭐☆☆☆ | ⭐⭐⭐⭐☆ |
| **Maintenance** | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐☆ | ⭐⭐⭐☆☆ | ⭐⭐⭐⭐☆ |

### 2.2 Use Case Suitability

| Test Type | Best Choice | Runner-up |
|-----------|-------------|-----------|
| Unit Tests (Core) | **C++** | Fortran |
| Integration Tests | **C++** | Python |
| E2E Tests | **Python** | C++ |
| Performance Tests | **C++** | Fortran |
| Regression Tests | **Python** | C++ |
| Stress Tests | **C++** | Rust |
| Fuzzing | **Rust** | C++ |
| API Tests | **Python** | C++ |

---

## 3. Recommended Hybrid Approach

### 3.1 Language Distribution

```
Test Suite Composition:
┌─────────────────────────────────────────┐
│ C++ (GoogleTest)        70%   [████████]│
│ Fortran (pFUnit)        20%   [███     ]│
│ Python (pytest)         10%   [█       ]│
└─────────────────────────────────────────┘
```

### 3.2 Allocation by Category

**C++ Tests (70% - 56 tests)**
- ✓ All unit tests (core algorithms, data structures)
- ✓ Integration tests (component interaction)
- ✓ Stress tests (performance, memory)
- ✓ Memory leak detection (Valgrind)

**Fortran Tests (20% - 16 tests)**
- ✓ Precision-specific numerical tests
- ✓ Fortran module interface tests
- ✓ Template-generated code validation
- ✓ Fortran-to-Fortran integration

**Python Tests (10% - 8 tests)**
- ✓ E2E workflow tests (high-level)
- ✓ Regression tests (bug reproduction)
- ✓ Matrix generation and validation
- ✓ Test data management scripts

### 3.3 Directory Structure

```
tests/
├── cpp/                    # C++ tests (70%)
│   ├── unit/
│   ├── integration/
│   ├── stress/
│   └── CMakeLists.txt
├── fortran/                # Fortran tests (20%)
│   ├── unit/
│   ├── testSuites.inc
│   └── CMakeLists.txt
├── python/                 # Python tests (10%)
│   ├── e2e/
│   ├── regression/
│   ├── conftest.py
│   └── pytest.ini
└── common/                 # Shared test data
    ├── matrices/
    └── fixtures/
```

---

## 4. Implementation Strategy

### 4.1 Phase 1: C++ Core (Weeks 1-8)

**Priority**: Establish solid foundation

1. Set up GoogleTest framework
2. Implement all unit tests in C++
3. Create integration tests in C++
4. Achieve 70% coverage baseline

**Why C++ first?**
- Fastest feedback loop
- Best tooling (debuggers, profilers)
- Easiest CI/CD integration
- Industry-standard approach

### 4.2 Phase 2: Fortran Precision Tests (Weeks 9-10)

**Priority**: Validate numerical accuracy

1. Set up pFUnit framework
2. Implement precision-specific tests
3. Test Fortran module interfaces
4. Validate template-generated code

**Why Fortran second?**
- Covers gaps C++ can't (native precision handling)
- Validates Fortran-to-Fortran paths
- Ensures template generation correctness

### 4.3 Phase 3: Python E2E (Weeks 11-12)

**Priority**: High-level validation

1. Create Python bindings (ctypes/cffi)
2. Implement E2E workflow tests
3. Create regression test suite
4. Build test data generators

**Why Python last?**
- Requires bindings (dependency on C++ core)
- High-level tests need solid foundation
- Most value after core is stable

---

## 5. Detailed Recommendations

### 5.1 For Unit Tests: **C++ (GoogleTest)**

**Rationale**:
- ✅ Fastest execution (native code)
- ✅ Best debugging tools (gdb, valgrind)
- ✅ Mature ecosystem (GMock, coverage)
- ✅ Industry standard
- ✅ Easy CI/CD integration

**Example**:
```cpp
// Fast, native, excellent tooling
TEST(Analysis, SymbolicFactorization) {
    // Test runs in microseconds
    // Valgrind catches memory errors
    // Coverage tracked automatically
}
```

### 5.2 For Integration Tests: **C++ (GoogleTest)**

**Rationale**:
- ✅ Can test full workflows end-to-end
- ✅ Performance matters (integration tests run often)
- ✅ Easy to parametrize (TYPED_TEST, TEST_P)
- ✅ Good fixture support

**Example**:
```cpp
class IntegrationTest : public ::testing::TestWithParam<BLASVendor> {
    // Parameterized test across OpenBLAS, MKL, etc.
};
```

### 5.3 For Numerical Precision: **Fortran (pFUnit)**

**Rationale**:
- ✅ Native REAL vs DOUBLE PRECISION handling
- ✅ Direct access to Fortran modules
- ✅ No type conversion errors
- ✅ Validates Fortran-to-Fortran paths

**Example**:
```fortran
@test
subroutine test_precision_d()
    ! Native Fortran, guaranteed precision
end subroutine
```

### 5.4 For E2E/Regression: **Python (pytest)**

**Rationale**:
- ✅ Easy to write complex workflows
- ✅ Great for reproducing bug reports
- ✅ Excellent fixtures and parametrization
- ✅ Fast iteration (no compilation)

**Example**:
```python
@pytest.mark.parametrize("matrix_file", matrices)
def test_regression_issue_1234(matrix_file):
    # Easy to iterate and debug
```

---

## 6. CI/CD Configuration

### 6.1 GitHub Actions Workflow

```yaml
name: Multi-Language Test Suite

jobs:
  cpp-tests:
    runs-on: ubuntu-latest
    steps:
      - name: Build C++ tests
        run: |
          cmake -DBUILD_TESTING=ON .
          make -j$(nproc)
      - name: Run C++ tests
        run: ctest --output-on-failure
      - name: Coverage
        run: lcov --capture --directory . -o coverage.info

  fortran-tests:
    runs-on: ubuntu-latest
    steps:
      - name: Build Fortran tests
        run: |
          cmake -DBUILD_FORTRAN_TESTS=ON .
          make -j$(nproc) fortran-tests
      - name: Run Fortran tests
        run: ctest -R fortran_

  python-tests:
    runs-on: ubuntu-latest
    steps:
      - name: Install Python deps
        run: pip install pytest numpy cffi
      - name: Build Python bindings
        run: python setup.py build_ext --inplace
      - name: Run Python tests
        run: pytest tests/python/
```

### 6.2 Local Development

```bash
# Run all tests
make test-all

# Run only C++ tests (fastest)
make test-cpp

# Run only Fortran tests
make test-fortran

# Run only Python tests
make test-python
```

---

## 7. Decision Matrix

### 7.1 Quick Decision Guide

**Choose C++ if:**
- ✓ Testing core algorithms
- ✓ Need fast execution
- ✓ Need memory leak detection
- ✓ Want best tooling

**Choose Fortran if:**
- ✓ Testing Fortran-specific features
- ✓ Need exact precision control
- ✓ Testing template-generated code
- ✓ Validating Fortran modules

**Choose Python if:**
- ✓ Writing E2E workflows
- ✓ Creating regression tests
- ✓ Need fast iteration
- ✓ Generating test data

**Avoid Rust because:**
- ❌ Steep learning curve
- ❌ Limited team expertise
- ❌ Fortran FFI complexity
- ❌ Immature scientific ecosystem
- ⚠️ Consider for future fuzzing

---

## 8. Final Recommendation

### Primary: **C++ (GoogleTest)** - 70% of tests

**Why**: Best balance of speed, tooling, and ecosystem maturity.

```bash
# Quick start
sudo apt-get install libgtest-dev cmake
cd tests/cpp && mkdir build && cd build
cmake .. && make -j$(nproc)
ctest --output-on-failure
```

### Secondary: **Fortran (pFUnit)** - 20% of tests

**Why**: Covers Fortran-specific numerical precision validation.

```bash
# Quick start
git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit
cd tests/fortran
cmake -DPFUNIT_DIR=/path/to/pfunit ..
make && ctest
```

### Tertiary: **Python (pytest)** - 10% of tests

**Why**: Excellent for high-level E2E and regression tests.

```bash
# Quick start
pip install pytest numpy cffi
pytest tests/python/ -v
```

---

## 9. Migration Path (If Starting from Scratch)

**Week 1-2**: C++ infrastructure
**Week 3-6**: C++ unit tests
**Week 7-8**: C++ integration tests
**Week 9**: Fortran infrastructure
**Week 10**: Fortran precision tests
**Week 11**: Python bindings
**Week 12**: Python E2E tests

---

## Conclusion

The **hybrid C++ + Fortran + Python** approach maximizes strengths:
- **C++**: Speed, tooling, coverage (70%)
- **Fortran**: Numerical precision, native interop (20%)
- **Python**: Ease of use, regression tests (10%)

**Rust** is not recommended at this time due to steep learning curve and limited Fortran interop, but could be considered for future fuzzing initiatives.

---

**Document Version**: 1.0
**Last Updated**: 2026-02-11
**Recommendation**: Approved for implementation
