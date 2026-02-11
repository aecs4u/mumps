# Language Standards Completion Checklist

This checklist defines the **strict completion criteria** for declaring the codebase fully migrated to C23 and Fortran 2023.

## Scope

- C sources: `*.c`
- Fortran sources/templates: `src/*.F`, `src/*.f90`, `src/templates/*.in`
- Build-system resolution: Make and CMake language mode/flag settings

## Completion Criteria (Strict)

### Fortran 2023

1. No fixed-form Fortran sources remain in `src/` (`src/*.F = 0`).
2. No fixed-form templates remain in `src/templates/` (`src/templates/*.F.in = 0`).
3. No `MUMPS_NOF2003` compatibility references remain in source/templates.
4. `FORTRAN_FREE_STD_MODE=f2023` (not `auto`).
5. Resolved free-form flag is exactly `-std=f2023`.
6. `FORTRAN_FIXED_STD_MODE=none`.
7. Resolved fixed-form Fortran flag is empty.

### C23

1. `C_STD_MODE` is pinned to `gnu23`.
2. Resolved C flag is exactly `-std=gnu23`.

## Automated Checker

Use:

```bash
./scripts/check_language_standards_completion.sh --report
./scripts/check_language_standards_completion.sh --gate
```

- `--report`: non-blocking audit (always exits `0`).
- `--gate`: fail-fast mode (exits `1` when any strict criterion is not met).

Makefile wrappers:

```bash
make lang-std-check
make lang-std-gate
```

## CI Gate Policy

The CI gate must run:

```bash
./scripts/check_language_standards_completion.sh --gate
```

The pipeline should be considered failed if this command fails.
