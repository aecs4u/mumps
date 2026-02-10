# MUMPS Project Documentation

This directory contains project-specific documentation for the enhanced MUMPS build system and template infrastructure.

## Build System Documentation

### Quick Start
- **[QUICKSTART.md](QUICKSTART.md)** - Fast-track guide to building and using MUMPS

### Build Systems
- **[CMAKE_BUILD_GUIDE.md](CMAKE_BUILD_GUIDE.md)** - Complete guide to CMake-based builds
- **[CMAKE_SMART_BUILD_GUIDE.md](CMAKE_SMART_BUILD_GUIDE.md)** - Smart lazy evaluation system for CMake
- **[INCREMENTAL_BUILD_GUIDE.md](INCREMENTAL_BUILD_GUIDE.md)** - Incremental build system for Makefile
- **[BUILD_SYSTEM_IMPROVEMENTS_SUMMARY.md](BUILD_SYSTEM_IMPROVEMENTS_SUMMARY.md)** - Overview of all build system enhancements

### BLAS Configuration
- **[BLIS_SETUP.md](BLIS_SETUP.md)** - Setting up AMD BLIS BLAS library

## Template System Documentation

### Core Guides
- **[TEMPLATE_SYSTEM_GUIDE.md](TEMPLATE_SYSTEM_GUIDE.md)** - Complete guide to the template system
- **[TEMPLATE_ROLLOUT_FINAL_STATUS.md](TEMPLATE_ROLLOUT_FINAL_STATUS.md)** - Final status: 46 working templates generating 184 files

### Historical/Reference
- **[TEMPLATE_ROLLOUT_PLAN.md](TEMPLATE_ROLLOUT_PLAN.md)** - Original rollout plan
- **[TEMPLATE_ROLLOUT_STATUS.md](TEMPLATE_ROLLOUT_STATUS.md)** - Intermediate rollout status
- **[PILOT_TEMPLATE_DEMO.md](PILOT_TEMPLATE_DEMO.md)** - Initial template system demonstration

## Analysis and Research
- **[CODE_DEDUPLICATION_ANALYSIS.md](CODE_DEDUPLICATION_ANALYSIS.md)** - Analysis of code duplication across precisions
- **[ORDERING_VERIFICATION.md](ORDERING_VERIFICATION.md)** - Verification of ordering library support

## Official MUMPS Documentation

The upstream MUMPS documentation is in the `../doc/` directory:
- User Guide: `../doc/userguide_5.8.2.pdf`
- License: `../doc/CeCILL-C_V1-en.txt` (English), `../doc/CeCILL-C_V1-fr.txt` (French)

## Root-Level Files

Essential project files remain in the root directory:
- `README.md` - Main project README
- `LICENSE` - Project license
- `INSTALL` - Installation instructions
- `ChangeLog` - Version history
- `CREDITS` - Contributors and acknowledgments
- `VERSION` - Current version number

## Getting Started

1. **Quick build**: See [QUICKSTART.md](QUICKSTART.md)
2. **Template system**: See [TEMPLATE_SYSTEM_GUIDE.md](TEMPLATE_SYSTEM_GUIDE.md)
3. **Performance builds**: See [INCREMENTAL_BUILD_GUIDE.md](INCREMENTAL_BUILD_GUIDE.md)
4. **CMake builds**: See [CMAKE_SMART_BUILD_GUIDE.md](CMAKE_SMART_BUILD_GUIDE.md)
