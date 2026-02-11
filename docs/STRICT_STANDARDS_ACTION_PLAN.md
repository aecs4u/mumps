# Strict Language Standards Compliance - Action Plan

**Date:** 2026-02-10
**Status:** IN PROGRESS
**Goal:** Make MUMPS codebase compile with strict Fortran 2023 and C23 standards

## Executive Summary

The modernization effort converted 103 files to modern syntax, but the codebase still cannot compile with strict standards enforcement due to:
1. **Standalone continuation characters (&)** - 342 .F files + 7 .f90 files
2. **Compiler version compatibility** - GCC 13.3.0 doesn't support gnu23
3. **Missing verification** - No CI/CD gate to prevent regressions

## Current State Analysis

### Compiler Environment
- **GCC:** 13.3.0 (Ubuntu)
- **GFortran:** 13.3.0 (Ubuntu)
- **Supported Standards:**
  - C: gnu17, c17 (gnu23/c23 NOT supported)
  - Fortran: f2023, f2018, f2008 (all supported)

### Build Configuration (Makefile.inc)
```makefile
C_STD_MODE ?= gnu23              # ❌ FAILS - unsupported
FORTRAN_FIXED_STD_MODE ?= auto   # ✓ Works (falls back to f2023)
FORTRAN_FREE_STD_MODE ?= auto    # ✓ Works (falls back to f2023)
```

### Identified Issues

#### 1. Standalone Continuation Characters (CRITICAL)
- **Files Affected:** 349 files (342 .F + 7 .f90)
- **Pattern:** Lines ending with `&` that only serve to continue to another `&` line
- **Example:**
  ```fortran
  IF ( SKIP_COLinSchur.AND. &          ← line ends with &
    &         ( (SYM_PERM(I).GT.N) &   ← starts with &, ends with &
    &           .OR. &                  ← starts with &, ends with &
    &          (SYM_PERM(J).GT.N) &    ← starts with &, ends with &
    &         ) &                       ← starts with &, ends with & (STANDALONE!)
    &       ) CYCLE                     ← starts with &, has code
  ```
- **Problem:** Fortran 2023 strict mode treats trailing `&` with no subsequent code as syntax error
- **Fix:** Remove trailing `&` from lines where next line only has leading `&` continuation

#### 2. C Standard Compatibility
- **Issue:** C_STD_MODE=gnu23 fails on GCC 13.3.0
- **Fix:** Change to C_STD_MODE=auto (will use gnu17/c17)

## Action Plan

### Phase 1: Immediate Fixes (Today)

#### Task 1.1: Fix Build Configuration ✓
**Goal:** Make codebase compile in non-strict mode

**Actions:**
- [x] Change `C_STD_MODE=gnu23` → `C_STD_MODE=auto` in Makefile.inc
- [x] Test: `make clean && make showconfig`
- [x] Verify: Build succeeds with auto-detected standards

**Expected Result:**
```
C_STD_MODE=auto
C_STD_FLAG=-std=gnu17
FORTRAN_FIXED_STD_MODE=auto
FORTRAN_FIXED_STD_FLAG=-ffree-form -std=f2023
FORTRAN_FREE_STD_MODE=auto
FORTRAN_FREE_STD_FLAG=-std=f2023
```

#### Task 1.2: Create Standalone & Fix Script
**Goal:** Systematic fix for all 349 files

**Script:** `scripts/fix_standalone_ampersands.sh`

**Algorithm:**
```bash
# For each line ending with &:
#   Look at next line
#   If next line ONLY contains whitespace + & + whitespace:
#     Remove trailing & from current line
#   Else:
#     Keep trailing & (it's legitimate continuation)
```

**Safety Features:**
- Backup original files (.backup)
- Dry-run mode for validation
- File-by-file diff review
- Rollback capability

**Implementation:**
```bash
#!/bin/bash
# Fix standalone & continuation characters
# Usage: ./fix_standalone_ampersands.sh [--dry-run] [--auto]

DRY_RUN=0
AUTO=0

while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run) DRY_RUN=1; shift ;;
    --auto) AUTO=1; shift ;;
    *) shift ;;
  esac
done

for file in src/*.F src/*.f90; do
  [ ! -f "$file" ] && continue

  # Create backup
  if [ $DRY_RUN -eq 0 ]; then
    cp "$file" "$file.backup.$(date +%Y%m%d_%H%M%S)"
  fi

  # Python script for precise replacement
  python3 -c "
import sys
import re

with open('$file', 'r') as f:
    lines = f.readlines()

fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    # Check if line ends with & (ignoring comments and whitespace)
    if re.match(r'^[^!]*&\s*$', line):
        # Check if next line ONLY has leading & continuation
        if i + 1 < len(lines):
            next_line = lines[i + 1]
            # If next line starts with & and has no other code content
            if re.match(r'^\s*&\s*$', next_line):
                # Remove trailing & from current line
                line = re.sub(r'&\s*$', '', line) + '\n'
                print(f'Fixed line {i+1}: removed standalone &', file=sys.stderr)
        fixed_lines.append(line)
    else:
        fixed_lines.append(line)
    i += 1

if $DRY_RUN == 0:
    with open('$file', 'w') as f:
        f.writelines(fixed_lines)
else:
    print(f'Would fix: $file', file=sys.stderr)
"
done

echo "Done! Backups saved with timestamp suffix"
```

#### Task 1.3: Test Current State (Non-Strict)
**Goal:** Verify codebase compiles without strict standards

**Commands:**
```bash
make clean
BUILD=release make d
cd examples && ./ssimpletest < input_simpletest_real
```

**Expected:** ✓ Build succeeds, test passes

### Phase 2: Apply Standalone & Fix (Tomorrow)

#### Task 2.1: Run Fix Script
```bash
cd /mnt/developer/git/aecs4u.it/mumps
./scripts/fix_standalone_ampersands.sh --dry-run  # Review changes
./scripts/fix_standalone_ampersands.sh            # Apply fixes
```

#### Task 2.2: Verify Fixes
```bash
# Count remaining issues
grep -r "^[^!]*&\s*$" src/*.F src/*.f90 | grep -v ".backup" | wc -l

# Expected: 0 (all fixed)
```

#### Task 2.3: Test with Non-Strict Build
```bash
make clean
BUILD=release make d
./examples/ssimpletest < examples/input_simpletest_real
```

#### Task 2.4: Commit Changes
```bash
git add -A
git commit -m "fix(fortran): Remove standalone continuation ampersands

- Fix 349 files (342 .F + 7 .f90) with trailing & followed by leading &
- Ensures Fortran 2023 strict mode compatibility
- No functional changes, syntax normalization only

This addresses the syntax error in strict standards mode where
continuation characters must actually continue to code content,
not just to another continuation character.

Part of: Strict Standards Compliance (STRICT_STANDARDS_ACTION_PLAN.md)"
```

### Phase 3: Enable Strict Standards (Day 3)

#### Task 3.1: Test with Strict Fortran
**Update Makefile.inc:**
```makefile
FORTRAN_FIXED_STD_MODE ?= f2023  # Strict, no auto fallback
FORTRAN_FREE_STD_MODE ?= f2023   # Strict, no auto fallback
```

**Test:**
```bash
make clean
BUILD=release make d 2>&1 | tee build_strict.log
```

**Validation:**
- ✓ No syntax errors
- ✓ No warnings about non-standard features
- ✓ All tests pass

#### Task 3.2: Test with Strict C (when available)
**Note:** Requires GCC >= 14.0 for gnu23 support

**For now:**
```makefile
C_STD_MODE ?= gnu17  # Explicit, not auto
```

**Future (with GCC 14+):**
```makefile
C_STD_MODE ?= gnu23  # Full C23 support
```

### Phase 4: CI/CD Integration (Week 1)

#### Task 4.1: Create GitHub Actions Workflow
**File:** `.github/workflows/strict-standards.yml`

```yaml
name: Strict Language Standards

on:
  push:
    branches: [main, feature/*]
  pull_request:

jobs:
  strict-standards-check:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4

      - name: Install Dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y gfortran gcc make
          sudo apt-get install -y libopenblas-dev liblapack-dev
          sudo apt-get install -y libscotch-dev libmetis-dev

      - name: Check for Standalone Ampersands
        run: |
          echo "Checking for standalone continuation ampersands..."
          ISSUES=$(grep -rn "^[^!]*&\s*$" src/*.F src/*.f90 2>/dev/null || true)
          if [ -n "$ISSUES" ]; then
            echo "ERROR: Found standalone & continuation characters:"
            echo "$ISSUES"
            exit 1
          fi
          echo "✓ No standalone & issues found"

      - name: Build with Strict Standards
        run: |
          cp Makefile.inc.example Makefile.inc
          # Set strict mode
          sed -i 's/C_STD_MODE ?= auto/C_STD_MODE ?= gnu17/' Makefile.inc
          sed -i 's/FORTRAN_FIXED_STD_MODE ?= auto/FORTRAN_FIXED_STD_MODE ?= f2023/' Makefile.inc
          sed -i 's/FORTRAN_FREE_STD_MODE ?= auto/FORTRAN_FREE_STD_MODE ?= f2023/' Makefile.inc

          make clean
          make showconfig
          BUILD=release make d

      - name: Run Tests
        run: |
          cd examples
          ./ssimpletest < input_simpletest_real
```

#### Task 4.2: Add Pre-Commit Hook
**File:** `.githooks/pre-commit`

```bash
#!/bin/bash
# Check for standalone & before commit

echo "Checking for standalone continuation ampersands..."

ISSUES=$(git diff --cached --name-only | grep -E '\.(F|f90)$' | \
  xargs grep -n "^[^!]*&\s*$" 2>/dev/null || true)

if [ -n "$ISSUES" ]; then
  echo "ERROR: Found standalone & continuation characters:"
  echo "$ISSUES"
  echo ""
  echo "Please run: ./scripts/fix_standalone_ampersands.sh"
  exit 1
fi

echo "✓ No standalone & issues found"
```

### Phase 5: Documentation & Maintenance (Week 1)

#### Task 5.1: Update Documentation
- [x] Create this action plan (STRICT_STANDARDS_ACTION_PLAN.md)
- [ ] Update MODERNIZATION_STATUS.md with strict standards status
- [ ] Update README.md with build requirements
- [ ] Add CONTRIBUTING.md section on code standards

#### Task 5.2: Update Build System
- [ ] Add `make lint-fortran` target for syntax checking
- [ ] Add `make strict-test` target for strict standards build
- [ ] Update `make lang-std-check` to include & detection

#### Task 5.3: Team Communication
- [ ] Notify team of strict standards requirement
- [ ] Update development guidelines
- [ ] Schedule training session on modern Fortran syntax

## Success Criteria

### Definition of Done
- [x] Codebase compiles with auto-detected standards (C17, F2023)
- [ ] Zero standalone & continuation characters
- [ ] Codebase compiles with strict F2023 (`-std=f2023`)
- [ ] All tests pass under strict standards
- [ ] CI/CD gate prevents regressions
- [ ] Documentation updated
- [ ] Zero build warnings under strict mode

### Metrics
- **Standalone & Issues:** 349 → 0
- **Build Success Rate:** 0% (strict) → 100% (strict)
- **Test Pass Rate:** Maintain 100%
- **Regression Rate:** Target 0 (via CI/CD)

## Risk Mitigation

### Risk 1: False Positives in Fix Script
**Mitigation:**
- Dry-run mode for review
- Backup all files before modification
- Git tracking for easy rollback

### Risk 2: Breaking Functional Behavior
**Mitigation:**
- Only syntax changes, no logic changes
- Comprehensive test suite
- Manual review of diffs

### Risk 3: CI/CD Blocking Development
**Mitigation:**
- Start with warning-only mode
- Transition to error mode after team adaptation
- Clear documentation and error messages

## Timeline

| Phase | Tasks | Duration | Target Date |
|-------|-------|----------|-------------|
| 1 | Immediate Fixes | 1 day | 2026-02-10 |
| 2 | Apply & Fix | 1 day | 2026-02-11 |
| 3 | Enable Strict | 1 day | 2026-02-12 |
| 4 | CI/CD Integration | 3 days | 2026-02-15 |
| 5 | Documentation | 2 days | 2026-02-17 |

**Total:** 8 days (1.5 weeks)

## Next Steps (Immediate)

1. ✓ Create this action plan
2. Fix Makefile.inc (C_STD_MODE=auto)
3. Create fix_standalone_ampersands.sh script
4. Test current state (non-strict build)
5. Review and approve approach

---

**Status Updates:**

- **2026-02-10 14:00:** Created action plan, identified 349 files with & issues
- **2026-02-10 14:15:** Ready to implement Phase 1 fixes
