# Fortran 2023 + C23 Porting Tracker

**Last updated:** 2026-02-10  
**Scope:** Track migration progress to modern language standards across build system, source code, and verification.

## Status Legend

- `DONE` = completed and verified
- `IN_PROGRESS` = actively being worked
- `BLOCKED` = waiting on a dependency or decision
- `TODO` = not started

## Baseline Snapshot (2026-02-10)

### Fortran side

- `src/*.F` (fixed-form): **386**
- `src/*.f90` (free-form): **53**
- `src/templates/*.in`: **99**
- `MUMPS_NOF2003` references in `src/`: **525**
- Phase 1 Fortran modernization tooling and pilot conversions: **DONE** (`docs/PHASE1_RESULTS.md`)

### C side

- `*.c` files repo-wide: **54**
- `*.h` files repo-wide: **49**
- C23 assessment document available: **DONE** (`docs/C23_MIGRATION_ASSESSMENT.md`)

### Build-system snapshot

- Makefile path:
  - C standard auto-detection for C23 family (`C_STD_MODE=auto`): **DONE** (`Makefile.inc`)
  - Free-form Fortran standard auto-detection (`FORTRAN_FREE_STD_MODE=auto`): **DONE** (`Makefile.inc`)
- CMake path:
  - `CMAKE_C_STANDARD` still set to `99`: **TODO** (`CMakeLists.txt:40`)

## Workstream Tracker

### Fortran 2023 Workstream

| ID | Item | Status | Notes / Exit criteria |
|---|---|---|---|
| F23-01 | Conversion tools maintained (`fixed_to_free`, `modernize_do`, `goto analysis`) | DONE | Tools already implemented and validated in pilot phase |
| F23-02 | Semantic verification workflow for modernized templates | DONE | `scripts/verify_template_generation.sh --semantic` available |
| F23-03 | Pilot template modernization (initial tranche) | DONE | 3 pilot templates completed per `docs/PHASE1_RESULTS.md` |
| F23-04 | Batch conversion of remaining fixed-form templates | IN_PROGRESS | Continue template-first modernization in batches with semantic verification |
| F23-05 | Reduce/remove `MUMPS_NOF2003` dependency hotspots | TODO | Define target files and remove compatibility branches where safe |
| F23-06 | Full precision validation (`s/d/c/z`) after each migration batch | IN_PROGRESS | Keep as ongoing gate; no batch should land without this |
| F23-07 | Final policy for fixed-form (`.F`) legacy support | TODO | Decide whether to keep legacy mode long-term or deprecate by release |

### C23 Workstream

| ID | Item | Status | Notes / Exit criteria |
|---|---|---|---|
| C23-01 | C23 migration assessment | DONE | Completed in `docs/C23_MIGRATION_ASSESSMENT.md` |
| C23-02 | Makefile C23 capability (auto/fallback) | DONE | Already in `Makefile.inc` |
| C23-03 | CMake C standard alignment with migration target | TODO | Update `CMakeLists.txt` and verify supported compilers |
| C23-04 | K&R-style declaration modernization (priority set) | TODO | Start with PORD + identified core files from assessment |
| C23-05 | Pointer style cleanup (`0` -> `NULL`, optional `nullptr`) | TODO | Apply with static checks to avoid integer false positives |
| C23-06 | Header guard completion for missing headers | TODO | Add guards to listed headers in assessment |
| C23-07 | Compiler matrix validation (GCC/Clang, strict warnings) | IN_PROGRESS | Fail-fast gate added (`scripts/check_language_standards_completion.sh --gate`, `.github/workflows/language-standards-gate.yml`) |

## Milestone Gates

### Gate A: Build Standards Ready

- Fortran free-form compiles in `f2023` (or validated fallback) through default Makefile flow
- C path compiles in C23-family mode via Makefile and CMake
- `make showconfig` reports expected standard flags

### Gate B: Source Modernization Stable

- Fortran modernization batches merged with semantic verification and no precision regressions
- C legacy patterns (K&R, missing guards, pointer style) reduced according to planned scope

### Gate C: Release Readiness

- Documentation updated (`INSTALL`, `docs/README.md`, migration tracker)
- CI/release scripts exercise modern standard modes
- Remaining legacy exceptions explicitly documented

## Immediate Next Actions

1. Start weekly tracker updates in this file (date + delta).
2. Execute first C23 implementation task: `C23-03` (CMake standard alignment).
3. Define and execute next Fortran batch for `F23-04` with semantic verification output attached.
4. Create a small checklist artifact per merged batch (files touched, s/d/c/z build result, known caveats).

## Weekly Update Template

```markdown
### YYYY-MM-DD
- Completed:
  - ...
- In progress:
  - ...
- Risks/blocks:
  - ...
- Next:
  - ...
```
