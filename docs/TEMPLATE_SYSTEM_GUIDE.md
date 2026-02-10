# MUMPS Template System Guide

## Overview

The MUMPS template system generates precision-specific source files (s/d/c/z) from single template files (.in), reducing code duplication from 400+ files to ~100 templates.

## Current Status

**45 templates generating 180 files:**
- Analysis modules: 9 templates → 36 files
- Solution modules: 13 templates → 52 files
- Config/utility: 4 templates → 16 files
- Support modules: 10 templates → 40 files
- Factorization: 9 templates → 36 files

## Template Variables (9 total)

| Variable | Values | Purpose |
|----------|--------|---------|
| `@MUMPS_PREFIX@` | S, D, C, Z | Uppercase precision prefix |
| `@MUMPS_PREFIX_LOWER@` | s, d, c, z | Lowercase precision prefix |
| `@MUMPS_TYPE@` | REAL, DOUBLE PRECISION, COMPLEX, COMPLEX*16 | Main arithmetic type |
| `@MUMPS_REAL_TYPE@` | REAL, DOUBLE PRECISION, REAL, DOUBLE PRECISION | Real-valued type (even for complex) |
| `@MUMPS_REAL_CONV@` | real, dble, real, dble | Real conversion function |
| `@MUMPS_REAL_LIT@` | E0, D0, E0, D0 | Real literal suffix |
| `@MUMPS_ARITH@` | real, real, complex, complex | Arithmetic type string |
| `@MUMPS_RHS_WRITE@` | (varies) | RHS array write format |
| `@MUMPS_MPI_TYPE@` | MPI_REAL, MPI_DOUBLE_PRECISION, MPI_COMPLEX, MPI_DOUBLE_COMPLEX | MPI data type |

## Conditional Blocks

For precision-specific code that can't use simple variable substitution:

```fortran
@IF_REAL@
! Code for s/d precisions only
PARAMETER( ZERO = 0.0@MUMPS_REAL_LIT@ )
@ENDIF@

@IF_COMPLEX@
! Code for c/z precisions only  
PARAMETER( ZERO = (0.0@MUMPS_REAL_LIT@,0.0@MUMPS_REAL_LIT@) )
K149 = K149 * 2  ! Complex types are twice the size
@ENDIF@
```

## Common Patterns

### 1. ZERO Parameter Initialization

**Problem:** Complex uses `(0.0,0.0)`, real uses `0.0`

**Solution:**
```fortran
@MUMPS_TYPE@ ZERO
@IF_REAL@
PARAMETER( ZERO = 0.0@MUMPS_REAL_LIT@ )
@ENDIF@
@IF_COMPLEX@
PARAMETER( ZERO = (0.0@MUMPS_REAL_LIT@,0.0@MUMPS_REAL_LIT@) )
@ENDIF@
```

### 2. Complex Type Size

**Problem:** Complex types need `K149 = K149 * 2` for size calculations

**Solution:**
```fortran
K149 = K150
@IF_COMPLEX@
K149 = K149 * 2
@ENDIF@
```

### 3. MPI Data Types

**Problem:** MPI types vary by precision

**Solution:** Use `@MUMPS_MPI_TYPE@` variable
```fortran
CALL MPI_SEND(buffer, count, @MUMPS_MPI_TYPE@, dest, tag, comm, ierr)
```

### 4. Precision-Specific Variable Names

**Problem:** Variable names include precision prefix (e.g., `sswap`, `dswap`, `cswap`)

**Solution:** Use `@MUMPS_PREFIX_LOWER@` variable
```fortran
@MUMPS_TYPE@ @MUMPS_PREFIX_LOWER@swap
```

## Critical Success Pattern

**Always create templates from COMPLEX (c prefix) versions, not REAL (s prefix).**

Reason: Complex files have both:
- `COMPLEX` declarations → become `@MUMPS_TYPE@` (precision-specific)
- `REAL` declarations → become `@MUMPS_REAL_TYPE@` (stays real for all)

Real files only have REAL declarations, making it impossible to distinguish.

## Template Creation Workflow

1. **Start with complex version** (c prefix file)

2. **Apply standard substitutions:**
   ```bash
   sed -e 's/CMUMPS/@MUMPS_PREFIX@MUMPS/g' \
       -e 's/COMPLEX,POINTER,DIMENSION/@MUMPS_TYPE@,POINTER,DIMENSION/g' \
       -e 's/COMPLEX /@MUMPS_TYPE@ /g' \
       -e 's/COMPLEX$/@MUMPS_TYPE@/g' \
       -e 's/COMPLEX,/@MUMPS_TYPE@,/g' \
       -e 's/COMPLEX(/@MUMPS_TYPE@(/g' \
       -e 's/MPI_COMPLEX/@MUMPS_MPI_TYPE@/g' \
       src/cfile.F > src/templates/file.F.in
   ```

3. **Add conditional blocks** for:
   - ZERO parameter initialization
   - Complex size calculations (`K149 * 2`)
   - Any remaining precision-specific logic

4. **Verify generation:**
   ```bash
   ./scripts/generate_from_template.sh src/templates/file.F.in src/generated
   
   # Check s/d/c for byte-identical match
   diff src/sfile.F src/generated/sfile.F
   diff src/dfile.F src/generated/dfile.F  
   diff src/cfile.F src/generated/cfile.F
   
   # Check z for functional equivalence (COMPLEX*16 vs COMPLEX(kind=8) ok)
   diff src/zfile.F src/generated/zfile.F
   ```

## Functional Equivalence

These differences are acceptable (functionally equivalent):

1. **Type syntax:** `COMPLEX*16` ↔ `COMPLEX(kind=8)`
2. **Case:** `REAL` ↔ `real` (Fortran is case-insensitive)
3. **Whitespace:** Alignment spaces after commas

## Non-Templateable Files

### Factorization Files (46)

Fundamental algorithmic differences:
- Determinant: `fraction()` (real) vs complex magnitude
- Initialization: `1.0` (real) vs `cmplx(1.0, 0.0)` (complex)

**Decision:** Accept as non-template (low modification frequency)

### Support Files with Algorithmic Tuning (8)

Files with precision-specific algorithm parameters:
1. `ini_defaults.F` - KEEP(421) = 3000 (real) vs 1000 (complex)
2. `rank_revealing.F` - LWK_RR = 5*SIZE_ROOT (real) vs SIZE_ROOT (complex)
3. `tools.F` - Various KEEP tuning parameters
4. `mumps_driver.F` - Driver-specific logic
5. `lr_core.F` - Low-rank core algorithms
6. `mumps_comm_buffer.F` - Buffer size tuning
7. `mumps_ooc_buffer.F` - Out-of-core buffer management

**Decision:** Cannot template - precision-specific tuning is intentional

## Working Support Templates (10)

Successfully templated with conditionals:
1. `arrowheads.F` - ZERO init, MPI types
2. `lr_type.f90` - Simple type substitution
3. `mumps_f77.F` - Simple type substitution
4. `mumps_intr_types.F` - Simple type substitution
5. `mumps_lr_data_m.F` - Simple type substitution
6. `mumps_ooc.F` - Simple type substitution
7. `mumps_save_restore.F` - Simple type substitution
8. `ooc_panel_piv.F` - K149*2 conditional
9. `static_ptr_m.F` - Simple type substitution
10. `type3_root.F` - K149*2 conditional

## File Locations

- Templates: `src/templates/*.in`
- Generator script: `scripts/generate_from_template.sh`
- Generated files: `src/generated/` (for verification)
- Original files: `src/[sdcz]*.{F,f90,c,h}`

## Usage

```bash
# Generate all files from all templates
make generate

# Generate single template
./scripts/generate_from_template.sh src/templates/file.F.in src/generated

# Verify template generates byte-identical files
./scripts/verify_template_generation.sh src/templates/file.F.in
```

## Benefits

1. **Reduced duplication:** ~400 files → ~100 templates
2. **Easier maintenance:** Fix bug once in template vs 4 times
3. **Consistency:** Guaranteed identical logic across precisions
4. **Type safety:** Automatic type conversions prevent errors

## Limitations

1. **Not universal:** ~58 files cannot be templated (algorithmic differences)
2. **Conditional complexity:** Some templates need precision-specific blocks
3. **Z functional equivalence:** `COMPLEX*16` vs `COMPLEX(kind=8)` syntax differs
