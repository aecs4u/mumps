# MUMPS Template Rollout Plan

## Overview

This document tracks the conversion of 103 duplicated source file groups (412 precision variants) to a template-based generation system, eliminating 92% code duplication.

## Current Status

**Phase 0: Infrastructure Setup** ✅ IN PROGRESS
- [x] Test harness created: `scripts/verify_template_generation.sh`
- [x] .gitignore updated for generated files
- [ ] Makefile integration enhanced
- [ ] Full workflow tested

**Pilot Template Completed**: `mumps_config_file.f90.in` ✅

## Implementation Plan Summary

### Timeline: 14-21 days
- **Phase 0**: Infrastructure Setup (1-2 days) - IN PROGRESS
- **Phase 1**: Config/Utility Modules (0.5-1 day) - 5 files
- **Phase 2**: Analysis Modules (2-3 days) - 9 files
- **Phase 3**: Solution Modules (2-3 days) - 13 files
- **Phase 4**: Factorization Modules (5-7 days) - 51 files
- **Phase 5**: Support Modules (2-3 days) - 25 files
- **Phase 6**: Final Integration & Testing (1-2 days)

## Phase Details

### Phase 0: Infrastructure Setup (CURRENT)

**Goal**: Establish robust template infrastructure

**Completed**:
1. ✅ Test harness: `scripts/verify_template_generation.sh`
   - Verifies byte-identical output
   - Tests all 4 precisions
   - Compilation testing
   - Color-coded pass/fail output

2. ✅ `.gitignore` updated
   - Excludes `/src/generated/`
   - Keeps templates tracked

**Remaining**:
3. Makefile integration
   - Add VPATH for generated files
   - Pattern rules for template → generated
   - Dependency tracking
   - Auto-generation before compile

4. Full workflow test
   - Clean build from templates
   - Verify dependency chains
   - Test incremental builds

### Phase 1: Config/Utility Modules

**Files** (5 base files, 20 variants):
1. ✅ `mumps_config_file.f90` (17 lines) - PILOT DONE
2. `omp_tps_m.f90` (21 lines)
3. `mumps_iXamax.f90` (22 lines)
4. `mumps_mpi3_mod.f90`
5. `mumps_struc_def.f90`

**Process**:
- Create template in `src/templates/`
- Generate all 4 precisions
- Byte-compare with originals
- Compile test all precisions
- Commit template + updates

### Phase 2: Analysis Modules

**Batch 2A - Core analysis** (5 files):
1. `ana_aux.F` (4307 lines) - CRITICAL
2. `ana_driver.F` (5174 lines) - CRITICAL
3. `ana_aux_par.F`
4. `ana_lr.F`
5. `ana_dist_m.F`

**Batch 2B - Analysis support** (4 files):
6. `ana_aux_ELT.F`
7. `ana_LDLT_preprocess.F`
8. `ana_mtrans.F`
9. `ana_reordertree.F`

### Phase 3: Solution Modules

**Batch 3A - Core solution** (4 files):
1. `sol_driver.F`
2. `sol_aux.F` (1731 lines)
3. `sol_fwd.F`
4. `sol_bwd.F`

**Batch 3B - Solution support** (5 files):
5. `sol_fwd_aux.F`
6. `sol_bwd_aux.F`
7. `sol_lr.F`
8. `sol_omp_m.F`
9. `sol_root_parallel.F`

**Batch 3C - Solution utilities** (4 files):
10. `sol_c.F`
11. `sol_distrhs.F`
12. `sol_distsol.f90`
13. `sol_matvec.F`

### Phase 4: Factorization Modules (51 files in 8 batches)

See [TEMPLATE_ROLLOUT_PLAN_DETAILED.md](./TEMPLATE_ROLLOUT_PLAN_DETAILED.md) for complete file lists.

**Summary**:
- Batch 4A: Core factorization (3 files)
- Batch 4B: Assembly operations (6 files)
- Batch 4C: Front operations (6 files)
- Batch 4D: Memory management (6 files)
- Batch 4E-F: Process operations (17 files)
- Batch 4G: Support operations (6 files)
- Batch 4H: Factorization utilities (7 files)

### Phase 5: Support Modules (25 files in 5 batches)

**Summary**:
- Batch 5A: Drivers and initialization (7 files)
- Batch 5B: Low-rank factorization (3 files)
- Batch 5C: Data structures (5 files)
- Batch 5D: OOC and utilities (5 files)
- Batch 5E: Miscellaneous (5 files)

### Phase 6: Final Integration & Testing

**Tasks**:
1. Full clean build from templates
2. Complete test suite execution
3. Performance validation
4. Documentation updates
5. CI/CD updates

## Template System Usage

### Creating a Template

1. **Choose a reference file** (e.g., `src/sana_aux.F`)

2. **Create template** in `src/templates/`:
   ```bash
   cp src/sana_aux.F src/templates/ana_aux.F.in
   ```

3. **Replace precision-specific elements**:
   - Module names: `SANA_AUX` → `@MUMPS_PREFIX@ANA_AUX`
   - Function names: `SANA_FUNC` → `@MUMPS_PREFIX@ANA_FUNC`
   - Type declarations: `REAL` → `@MUMPS_TYPE@`
   - Structure types: `SMUMPS_STRUC` → `@MUMPS_PREFIX@MUMPS_STRUC`

4. **Generate files**:
   ```bash
   ./scripts/generate_from_template.sh src/templates/ana_aux.F.in src/generated/
   ```

5. **Verify**:
   ```bash
   ./scripts/verify_template_generation.sh ana_aux.F
   ```

### Makefile Integration (Planned)

The Makefile will automatically generate files from templates:

```makefile
# VPATH for generated files
VPATH = .:generated

# Pattern rule for template generation
generated/$(ARITH)%.F: templates/%.F.in scripts/generate_from_template.sh | generated
	@./scripts/generate_from_template.sh $< generated/

generated:
	mkdir -p generated
```

## Template Variables

| Variable | Purpose | Example Values |
|----------|---------|----------------|
| `@MUMPS_PREFIX@` | Function/module prefix | S, D, C, Z |
| `@MUMPS_TYPE@` | Data type declaration | REAL, DOUBLE PRECISION, COMPLEX, COMPLEX*16 |

## Validation Checklist (Per File/Batch)

- [ ] Template created in `src/templates/`
- [ ] Generate script produces 4 files
- [ ] Verification script shows PASS for all 4 precisions
- [ ] `diff -q` confirms byte-identical (or whitespace-only diffs)
- [ ] Compilation succeeds: `make ARITH=s/d/c/z`
- [ ] Examples run without errors
- [ ] Numerical results match reference
- [ ] Performance within 1% of baseline
- [ ] Git commit with descriptive message
- [ ] Update this progress file

## Progress Tracking

### Completed Templates
- [x] `mumps_config_file.f90` - Pilot (Phase 1)

### In Progress
- [ ] Infrastructure setup (Phase 0)

### Pending
- [ ] 102 remaining templates

## Commands Reference

### Generate from Template
```bash
./scripts/generate_from_template.sh src/templates/<name>.in src/generated/
```

### Verify Template
```bash
./scripts/verify_template_generation.sh <template_name>
```

### Verify All Templates
```bash
./scripts/verify_template_generation.sh
```

### Clean Generated Files
```bash
rm -rf src/generated/
```

### Build with Templates
```bash
make clean
make d  # or s, c, z
```

## Success Metrics

- **Code Reduction**: 412 files → 103 templates (75% reduction)
- **Byte-Identical**: All generated files match originals
- **Compilation**: All 4 precisions build successfully
- **Testing**: Full test suite passes
- **Performance**: Within 2% of baseline
- **Generation Time**: < 1 second for all templates

## Documentation

Related documents:
- [CODE_DEDUPLICATION_ANALYSIS.md](CODE_DEDUPLICATION_ANALYSIS.md) - Full analysis
- [PILOT_TEMPLATE_DEMO.md](PILOT_TEMPLATE_DEMO.md) - Pilot demonstration
- [BUILD_SYSTEM_IMPROVEMENTS_SUMMARY.md](BUILD_SYSTEM_IMPROVEMENTS_SUMMARY.md) - All improvements

## Repository Structure

```
mumps/
├── src/
│   ├── templates/           # Template source files (*.in)
│   │   └── mumps_config_file.f90.in
│   ├── generated/           # Auto-generated files (gitignored)
│   │   ├── smumps_config_file.f90
│   │   ├── dmumps_config_file.f90
│   │   ├── cmumps_config_file.f90
│   │   └── zmumps_config_file.f90
│   └── [original files remain until conversion complete]
├── scripts/
│   ├── generate_from_template.sh         # Template generator
│   └── verify_template_generation.sh     # Verification harness
└── TEMPLATE_ROLLOUT_PLAN.md             # This file
```

## Risk Mitigation

1. **Backup Strategy**: Git history provides complete rollback capability
2. **Incremental Approach**: Small batches with verification at each step
3. **Parallel Development**: Original files remain until templates proven
4. **Automated Testing**: Verification script catches issues immediately
5. **Performance Monitoring**: Benchmarks run after each phase

## Next Steps

1. Complete Phase 0 Makefile integration
2. Test full workflow with pilot template
3. Begin Phase 1 (config/utility modules)
4. Document any issues or patterns discovered
5. Adjust plan based on initial batch experience

---

**Status**: Phase 0 Infrastructure (IN PROGRESS)
**Last Updated**: 2026-02-09
**Completion**: 1/103 templates (1%)
