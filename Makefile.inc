#
#  This file is part of MUMPS 5.8.2, released
#  on Mon Jan 12 15:17:08 UTC 2026
#
# These settings for a PC under Debian/linux with standard packages :
# metis (parmetis), scotch (ptscotch), openmpi, gfortran

# packages installation: 
# apt-get install libmetis-dev libparmetis-dev libscotch-dev libptscotch-dev libatlas-base-dev openmpi-bin libopenmpi-dev liblapack-dev libscalapack-openmpi-dev

# Begin orderings
LSCOTCHDIR = /usr/lib
ISCOTCH   = -I/usr/include/scotch

#LSCOTCH   = -L$(LSCOTCHDIR) -lptesmumps -lptscotch -lptscotcherr
LSCOTCH   = -L$(LSCOTCHDIR) -lesmumps -lscotch -lscotcherr

LPORDDIR = $(topdir)/PORD/lib/
IPORD    = -I$(topdir)/PORD/include/
LPORD    = -L$(LPORDDIR) -lpord$(PLAT)

LMETISDIR = /usr/lib 
#IMETIS    = -I/usr/include/parmetis
IMETIS    = -I/usr/include/metis

# LMETIS    = -L$(LMETISDIR) -lparmetis -lmetis
LMETIS    = -L$(LMETISDIR) -lmetis

# Corresponding variables reused later
#ORDERINGSF = -Dmetis -Dpord -Dparmetis -Dscotch -Dptscotch
ORDERINGSF = -Dmetis -Dpord -Dscotch
ORDERINGSC  = $(ORDERINGSF)

LORDERINGS = $(LMETIS) $(LPORD) $(LSCOTCH)
IORDERINGSF = $(ISCOTCH)
IORDERINGSC = $(IMETIS) $(IPORD) $(ISCOTCH)
# End orderings
################################################################################

PLAT    =
LIBEXT_SHARED  = .so
SONAME = -soname
SHARED_OPT = -shared
FPIC_OPT = -fPIC
# Adapt/uncomment RPATH_OPT to avoid modifying
# LD_LIBRARY_PATH in case of shared libraries
# RPATH_OPT = -Wl,-rpath,/path/to/MUMPS_x.y.z/lib/
LIBEXT  = .a
OUTC    = -o 
OUTF    = -o 
RM = /bin/rm -f
CC = gcc
FC = gfortran
FL = gfortran
AR = ar vr 
RANLIB = ranlib

# BLAS/LAPACK vendor selection.
# BLAS_VENDOR can be: auto (default), openblas, mkl, blis, pkgconfig, reference.
BLAS_VENDOR ?= auto
HAVE_PKG_CONFIG := $(shell command -v pkg-config >/dev/null 2>&1 && echo yes)
MKL_PC_NAME := $(shell command -v pkg-config >/dev/null 2>&1 && for p in mkl-dynamic-lp64-seq mkl-dynamic-ilp64-seq mkl-sdl; do pkg-config --exists $$p >/dev/null 2>&1 && { echo $$p; exit 0; }; done)
AMD_VITIS_PC_NAME := $(shell command -v pkg-config >/dev/null 2>&1 && for p in blis-mt amdblis aocl-blis blis; do pkg-config --exists $$p >/dev/null 2>&1 && { echo $$p; exit 0; }; done)

ifeq ($(BLAS_VENDOR),auto)
  ifeq ($(HAVE_PKG_CONFIG),yes)
    ifneq ($(shell pkg-config --exists openblas >/dev/null 2>&1 && echo yes),)
      BLAS_VENDOR := openblas
    else ifneq ($(MKL_PC_NAME),)
      BLAS_VENDOR := mkl
    else ifneq ($(shell pkg-config --exists blas >/dev/null 2>&1 && pkg-config --exists lapack >/dev/null 2>&1 && echo yes),)
      BLAS_VENDOR := pkgconfig
    else
      BLAS_VENDOR := reference
    endif
  else
    BLAS_VENDOR := reference
  endif
endif

DEFAULT_LAPACK = -llapack
DEFAULT_LIBBLAS = -lblas
DEFAULT_LIBOTHERS = -lpthread

ifeq ($(BLAS_VENDOR),openblas)
  ifeq ($(HAVE_PKG_CONFIG),yes)
    DEFAULT_LIBBLAS = $(shell pkg-config --libs openblas 2>/dev/null)
    ifneq ($(shell pkg-config --exists lapack >/dev/null 2>&1 && echo yes),)
      DEFAULT_LAPACK = $(shell pkg-config --libs lapack 2>/dev/null)
    endif
  else
    DEFAULT_LIBBLAS = -lopenblas
  endif
else ifeq ($(BLAS_VENDOR),mkl)
  ifneq ($(MKL_PC_NAME),)
    DEFAULT_LIBBLAS = $(shell pkg-config --libs $(MKL_PC_NAME) 2>/dev/null)
    DEFAULT_LAPACK =
    DEFAULT_LIBOTHERS =
  else
    DEFAULT_LIBBLAS = -lmkl_rt
    DEFAULT_LAPACK =
    DEFAULT_LIBOTHERS = -lpthread -lm -ldl
  endif
else ifeq ($(BLAS_VENDOR),blis)
  ifneq ($(BLIS_LIBBLAS),)
    DEFAULT_LIBBLAS = $(BLIS_LIBBLAS)
    DEFAULT_LAPACK = $(BLIS_LAPACK)
    ifneq ($(BLIS_LIBOTHERS),)
      DEFAULT_LIBOTHERS = $(BLIS_LIBOTHERS)
    endif
  else ifneq ($(AMD_VITIS_PC_NAME),)
    DEFAULT_LIBBLAS = $(shell pkg-config --libs $(AMD_VITIS_PC_NAME) 2>/dev/null)
    ifneq ($(shell pkg-config --exists lapack >/dev/null 2>&1 && echo yes),)
      DEFAULT_LAPACK = $(shell pkg-config --libs lapack 2>/dev/null)
    else
      DEFAULT_LAPACK = -llapack
    endif
  else
    # Fallback to direct linking with libblis or libblis-mt
    DEFAULT_LIBBLAS = -lblis-mt -lm -lpthread
    DEFAULT_LAPACK = -llapack
  endif
else ifeq ($(BLAS_VENDOR),amd-vitis)
  # Keep amd-vitis as alias for backward compatibility
  $(warning BLAS_VENDOR='amd-vitis' is deprecated, use 'blis' instead)
  ifneq ($(AMD_VITIS_LIBBLAS),)
    DEFAULT_LIBBLAS = $(AMD_VITIS_LIBBLAS)
    DEFAULT_LAPACK = $(AMD_VITIS_LAPACK)
    ifneq ($(AMD_VITIS_LIBOTHERS),)
      DEFAULT_LIBOTHERS = $(AMD_VITIS_LIBOTHERS)
    endif
  else ifneq ($(AMD_VITIS_PC_NAME),)
    DEFAULT_LIBBLAS = $(shell pkg-config --libs $(AMD_VITIS_PC_NAME) 2>/dev/null)
    ifneq ($(shell pkg-config --exists lapack >/dev/null 2>&1 && echo yes),)
      DEFAULT_LAPACK = $(shell pkg-config --libs lapack 2>/dev/null)
    else
      DEFAULT_LAPACK = -llapack
    endif
  else
    DEFAULT_LIBBLAS = -lblis-mt -lm -lpthread
    DEFAULT_LAPACK = -llapack
  endif
else ifeq ($(BLAS_VENDOR),pkgconfig)
  ifeq ($(HAVE_PKG_CONFIG),yes)
    DEFAULT_LIBBLAS = $(shell pkg-config --libs blas 2>/dev/null)
    DEFAULT_LAPACK = $(shell pkg-config --libs lapack 2>/dev/null)
  endif
else ifeq ($(BLAS_VENDOR),reference)
  # Keep default reference values.
else
$(error Unsupported BLAS_VENDOR='$(BLAS_VENDOR)'; use auto/openblas/mkl/blis/pkgconfig/reference)
endif

LAPACK ?= $(DEFAULT_LAPACK)
LIBBLAS ?= $(DEFAULT_LIBBLAS)
LIBOTHERS ?= $(DEFAULT_LIBOTHERS)

INCSEQ = -I$(topdir)/libseq
LIBSEQ  = $(LAPACK) -L$(topdir)/libseq -lmpiseq$(PLAT)

#Preprocessor defs for calling Fortran from C (-DAdd_ or -DAdd__ or -DUPPER)
CDEFS   = -DAdd_

# Build profile: BUILD=release (default) or BUILD=debug
BUILD ?= release
COMMON_OMP_FLAGS = -fopenmp
# Keep compatibility with stricter argument checking introduced in gfortran >= 10.
FORTRAN_COMPAT_FLAGS = -fallow-argument-mismatch

ifeq ($(BUILD),release)
OPTF    = -O3 $(COMMON_OMP_FLAGS) $(FORTRAN_COMPAT_FLAGS)
OPTL    = -O3 $(COMMON_OMP_FLAGS)
OPTC    = -O3 $(COMMON_OMP_FLAGS)
else ifeq ($(BUILD),debug)
OPTF    = -O0 -g $(COMMON_OMP_FLAGS) $(FORTRAN_COMPAT_FLAGS) -fcheck=all -fbacktrace
OPTL    = -O0 -g $(COMMON_OMP_FLAGS)
OPTC    = -O0 -g $(COMMON_OMP_FLAGS) -fno-omit-frame-pointer
else
$(error Unsupported BUILD='$(BUILD)'; use BUILD=release or BUILD=debug)
endif
 
INCS = $(INCSEQ)
LIBS = $(LIBSEQ)
LIBSEQNEEDED = libseqneeded
