#
#  This file is part of MUMPS 5.8.2, released
#  on Mon Jan 12 15:17:08 UTC 2026
#
SRCDIR ?= $(CURDIR)
SRCDIR := $(abspath $(SRCDIR))
BUILDROOT := $(abspath $(CURDIR))

topdir = .
libdir = $(topdir)/lib
OUT_OF_TREE := $(if $(filter $(BUILDROOT),$(SRCDIR)),0,1)

default: d

.PHONY: default clean distclean
.PHONY: all s d c z prerequisites libseqneeded
.PHONY: allshared sshared dshared cshared zshared prerequisitesshared libseqneededsharedlibseq sharedlibseq
.PHONY: pkgconfig install install-libs install-headers install-pkgconfig FORCE
.PHONY: showconfig cache-key cache-restore cache-save cache-clean out-of-tree-prepare out
.PHONY: bench-blas bench-blas-dense bench-correctness bench-db bench-all-db bench-mkl-amd-vitis-db bench-web
.PHONY: webapp webapp-stop webapp-open webapp-health

all: out-of-tree-prepare prerequisites
	$(call run_cached_build,all,all)

s: out-of-tree-prepare prerequisites
	$(call run_cached_build,s,s)

d: out-of-tree-prepare prerequisites
	$(call run_cached_build,d,d)

c: out-of-tree-prepare prerequisites
	$(call run_cached_build,c,c)

z: out-of-tree-prepare prerequisites
	$(call run_cached_build,z,z)


allshared: out-of-tree-prepare prerequisitesshared
	$(call run_cached_build,allshared,all)

sshared: out-of-tree-prepare prerequisitesshared
	$(call run_cached_build,sshared,s)

dshared: out-of-tree-prepare prerequisitesshared
	$(call run_cached_build,dshared,d)

cshared: out-of-tree-prepare prerequisitesshared
	$(call run_cached_build,cshared,c)

zshared: out-of-tree-prepare prerequisitesshared
	$(call run_cached_build,zshared,z)


MAKEFILE_INC_PATH := $(firstword $(wildcard Makefile.inc $(SRCDIR)/Makefile.inc))
ifeq ($(MAKEFILE_INC_PATH),)
$(error Missing Makefile.inc. Provide one in $(CURDIR) or $(SRCDIR))
endif
include $(MAKEFILE_INC_PATH)
-include Makefile.vendor
-include $(SRCDIR)/Makefile.vendor

BUILD_CACHE ?= 1
BUILD_CACHE_DIR ?= $(topdir)/.build-cache
HASH_TOOL := $(shell command -v sha1sum >/dev/null 2>&1 && echo sha1sum || (command -v shasum >/dev/null 2>&1 && echo shasum || echo))
CACHE_SIGNATURE = BUILD=$(BUILD);BLAS_VENDOR=$(BLAS_VENDOR);CC=$(CC);FC=$(FC);FL=$(FL);CDEFS=$(CDEFS);OPTF=$(OPTF);OPTF90=$(OPTF90);OPTL=$(OPTL);OPTC=$(OPTC);C_STD_MODE=$(C_STD_MODE);C_STD_FLAG=$(C_STD_FLAG);FORTRAN_FIXED_STD_MODE=$(FORTRAN_FIXED_STD_MODE);FORTRAN_FIXED_STD_FLAG=$(FORTRAN_FIXED_STD_FLAG);FORTRAN_FREE_STD_MODE=$(FORTRAN_FREE_STD_MODE);FORTRAN_FREE_STD_FLAG=$(FORTRAN_FREE_STD_FLAG);ORDERINGSF=$(ORDERINGSF);ORDERINGSC=$(ORDERINGSC);LORDERINGS=$(LORDERINGS);LIBBLAS=$(LIBBLAS);LAPACK=$(LAPACK);LIBOTHERS=$(LIBOTHERS);PLAT=$(PLAT);LIBEXT=$(LIBEXT);LIBEXT_SHARED=$(LIBEXT_SHARED)
ifneq ($(strip $(HASH_TOOL)),)
CACHE_KEY := $(shell printf '%s' '$(CACHE_SIGNATURE)' | $(HASH_TOOL) | awk '{print $$1}')
else
CACHE_KEY := nohash
endif
CACHE_ENTRY := $(BUILD_CACHE_DIR)/$(CACHE_KEY)

define run_cached_build
	@if [ "$(BUILD_CACHE)" = "1" ]; then $(MAKE) --no-print-directory cache-restore; fi
	+cd src; $(MAKE) $(1)
	+cd examples; $(MAKE) $(2)
	@if [ "$(BUILD_CACHE)" = "1" ]; then $(MAKE) --no-print-directory cache-save; fi
endef

MUMPS_VERSION := $(shell awk 'NR==1 {print $$2}' $(topdir)/VERSION)
PREFIX ?= /usr/local
EXEC_PREFIX ?= $(PREFIX)
INSTALL_LIBDIR ?= $(EXEC_PREFIX)/lib
INSTALL_INCLUDEDIR ?= $(PREFIX)/include
PKGCONFIG_DIR ?= $(INSTALL_LIBDIR)/pkgconfig
INSTALL ?= install
INSTALL_DATA ?= $(INSTALL) -m 644

PKGCONFIG_FILES = \
	$(libdir)/mumps-s.pc \
	$(libdir)/mumps-d.pc \
	$(libdir)/mumps-c.pc \
	$(libdir)/mumps-z.pc

AVAILABLE_LIBS = $(wildcard \
	$(libdir)/libmumps_common$(PLAT)$(LIBEXT) \
	$(libdir)/libmumps_common$(PLAT)$(LIBEXT_SHARED) \
	$(libdir)/libsmumps$(PLAT)$(LIBEXT) \
	$(libdir)/libsmumps$(PLAT)$(LIBEXT_SHARED) \
	$(libdir)/libdmumps$(PLAT)$(LIBEXT) \
	$(libdir)/libdmumps$(PLAT)$(LIBEXT_SHARED) \
	$(libdir)/libcmumps$(PLAT)$(LIBEXT) \
	$(libdir)/libcmumps$(PLAT)$(LIBEXT_SHARED) \
	$(libdir)/libzmumps$(PLAT)$(LIBEXT) \
	$(libdir)/libzmumps$(PLAT)$(LIBEXT_SHARED) \
	$(libdir)/libpord$(PLAT)$(LIBEXT) \
	$(libdir)/libpord$(PLAT)$(LIBEXT_SHARED) \
	$(libdir)/libmpiseq$(PLAT)$(LIBEXT) \
	$(libdir)/libmpiseq$(PLAT)$(LIBEXT_SHARED))

PUBLIC_HEADERS = $(wildcard include/*.h)

prerequisites: Makefile.inc $(LIBSEQNEEDED) $(libdir)/libpord$(PLAT)$(LIBEXT)

prerequisitesshared: Makefile.inc $(LIBSEQNEEDED)sharedlibseq $(libdir)/libpord$(PLAT)$(LIBEXT_SHARED)

# Rules for fake MPI library used to avoid using MPI:
#
# If $(LIBSEQNEEDED) is empty, prerequisitesshared includes a dependenecy on
# the sharedlibseq suffix dependency which we always satisfy

sharedlibseq:

libseqneeded:
	(cd libseq; $(MAKE))
	(cp libseq/libmpiseq$(PLAT)$(LIBEXT) $(libdir))

libseqneededsharedlibseq:
	(cd libseq; $(MAKE) sharedlibmpiseq)
	if [ -f libseq/libmpiseq$(PLAT)$(LIBEXT) ]; then cp libseq/libmpiseq$(PLAT)$(LIBEXT) $(libdir); fi
	(cp libseq/libmpiseq$(PLAT)$(LIBEXT_SHARED) $(libdir))

# Build the libpord.a library and copy it into $(topdir)/lib
$(libdir)/libpord$(PLAT)$(LIBEXT):
	if [ "$(LPORDDIR)" != "" ] ; then \
	  cd $(LPORDDIR); \
	  $(MAKE) CC="$(CC)" PLAT="$(PLAT)" CFLAGS="$(OPTC)" AR="$(AR)" RANLIB="$(RANLIB)" OUTC="$(OUTC)" LIBEXT="$(LIBEXT)" LIBEXT_SHARED="$(LIBEXT_SHARED)" libpord$(PLAT)$(LIBEXT); \
	fi;
	if [ "$(LPORDDIR)" != "" ] ; then \
	  cp $(LPORDDIR)/libpord$(PLAT)$(LIBEXT) $@; \
	fi;

$(libdir)/libpord$(PLAT)$(LIBEXT_SHARED):
	if [ "$(LPORDDIR)" != "" ] ; then \
	  cd $(LPORDDIR); \
	  $(MAKE) PLAT="$(PLAT)" FPIC="$(FPIC_OPT)" CC="$(CC)" CFLAGS="$(OPTC)" AR="$(AR)" RANLIB="$(RANLIB)" OUTC="$(OUTC)" LIBEXT="$(LIBEXT)" LIBEXT_SHARED="$(LIBEXT_SHARED)" libpord$(PLAT)$(LIBEXT_SHARED); \
	fi;
	if [ "$(LPORDDIR)" != "" ] ; then \
	  cp $(LPORDDIR)/libpord$(PLAT)$(LIBEXT_SHARED) $@; \
	fi;




clean:
	(cd src; $(MAKE) clean)
	(cd examples; $(MAKE) clean)
	(cd $(libdir); $(RM) lib*$(PLAT)$(LIBEXT) lib*$(PLAT)$(LIBEXT_SHARED))
	($(RM) $(PKGCONFIG_FILES))
	(cd libseq; $(MAKE) clean)
	if [ "$(LPORDDIR)" != "" ] ; then \
	  cd $(LPORDDIR); $(MAKE) CC="$(CC)" CFLAGS="$(OPTC)" AR="$(AR)" RANLIB="$(RANLIB)" OUTC="$(OUTC)" LIBEXT="$(LIBEXT)" LIBEXT_SHARED="$(LIBEXT_SHARED)" PLAT="$(PLAT)" realclean; \
        fi;

clean-build:
	@echo "Cleaning build artifacts (preserving generated sources)"
	(cd src; $(MAKE) clean)
	(cd examples; $(MAKE) clean)
	(cd $(libdir); $(RM) lib*$(PLAT)$(LIBEXT) lib*$(PLAT)$(LIBEXT_SHARED))
	(cd libseq; $(MAKE) clean)

distclean: clean cache-clean

pkgconfig: $(PKGCONFIG_FILES)

FORCE:

$(libdir)/mumps-s.pc: FORCE
	@mkdir -p $(libdir)
	@printf '%s\n' \
		'prefix=$(PREFIX)' \
		'exec_prefix=$${prefix}' \
		'libdir=$(INSTALL_LIBDIR)' \
		'includedir=$(INSTALL_INCLUDEDIR)' \
		'' \
		'Name: MUMPS (single real)' \
		'Description: MUltifrontal Massively Parallel sparse direct Solver (single real)' \
		'Version: $(MUMPS_VERSION)' \
		'Libs: -L$${libdir} -lsmumps$(PLAT) -lmumps_common$(PLAT)' \
		'Libs.private: $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)' \
		'Cflags: -I$${includedir}' > $@

$(libdir)/mumps-d.pc: FORCE
	@mkdir -p $(libdir)
	@printf '%s\n' \
		'prefix=$(PREFIX)' \
		'exec_prefix=$${prefix}' \
		'libdir=$(INSTALL_LIBDIR)' \
		'includedir=$(INSTALL_INCLUDEDIR)' \
		'' \
		'Name: MUMPS (double real)' \
		'Description: MUltifrontal Massively Parallel sparse direct Solver (double real)' \
		'Version: $(MUMPS_VERSION)' \
		'Libs: -L$${libdir} -ldmumps$(PLAT) -lsmumps$(PLAT) -lmumps_common$(PLAT)' \
		'Libs.private: $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)' \
		'Cflags: -I$${includedir}' > $@

$(libdir)/mumps-c.pc: FORCE
	@mkdir -p $(libdir)
	@printf '%s\n' \
		'prefix=$(PREFIX)' \
		'exec_prefix=$${prefix}' \
		'libdir=$(INSTALL_LIBDIR)' \
		'includedir=$(INSTALL_INCLUDEDIR)' \
		'' \
		'Name: MUMPS (single complex)' \
		'Description: MUltifrontal Massively Parallel sparse direct Solver (single complex)' \
		'Version: $(MUMPS_VERSION)' \
		'Libs: -L$${libdir} -lcmumps$(PLAT) -lmumps_common$(PLAT)' \
		'Libs.private: $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)' \
		'Cflags: -I$${includedir}' > $@

$(libdir)/mumps-z.pc: FORCE
	@mkdir -p $(libdir)
	@printf '%s\n' \
		'prefix=$(PREFIX)' \
		'exec_prefix=$${prefix}' \
		'libdir=$(INSTALL_LIBDIR)' \
		'includedir=$(INSTALL_INCLUDEDIR)' \
		'' \
		'Name: MUMPS (double complex)' \
		'Description: MUltifrontal Massively Parallel sparse direct Solver (double complex)' \
		'Version: $(MUMPS_VERSION)' \
		'Libs: -L$${libdir} -lzmumps$(PLAT) -lcmumps$(PLAT) -lmumps_common$(PLAT)' \
		'Libs.private: $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)' \
		'Cflags: -I$${includedir}' > $@

install: install-libs install-headers install-pkgconfig

install-libs:
	@if [ -z "$(AVAILABLE_LIBS)" ]; then \
		echo "No built libraries found under $(libdir). Build first (for example: make d or make all)."; \
		exit 1; \
	fi
	$(INSTALL) -d $(DESTDIR)$(INSTALL_LIBDIR)
	$(INSTALL_DATA) $(AVAILABLE_LIBS) $(DESTDIR)$(INSTALL_LIBDIR)/

install-headers:
	$(INSTALL) -d $(DESTDIR)$(INSTALL_INCLUDEDIR)
	$(INSTALL_DATA) $(PUBLIC_HEADERS) $(DESTDIR)$(INSTALL_INCLUDEDIR)/

install-pkgconfig: pkgconfig
	$(INSTALL) -d $(DESTDIR)$(PKGCONFIG_DIR)
	$(INSTALL_DATA) $(PKGCONFIG_FILES) $(DESTDIR)$(PKGCONFIG_DIR)/

showconfig:
	@echo "BUILD=$(BUILD)"
	@echo "BLAS_VENDOR=$(BLAS_VENDOR)"
	@echo "LAPACK=$(LAPACK)"
	@echo "LIBBLAS=$(LIBBLAS)"
	@echo "LIBOTHERS=$(LIBOTHERS)"
	@echo "LIBSEQ=$(LIBSEQ)"
	@echo "LORDERINGS=$(LORDERINGS)"
	@echo "C_STD_MODE=$(C_STD_MODE)"
	@echo "C_STD_FLAG=$(C_STD_FLAG)"
	@echo "FORTRAN_FIXED_STD_MODE=$(FORTRAN_FIXED_STD_MODE)"
	@echo "FORTRAN_FIXED_STD_FLAG=$(FORTRAN_FIXED_STD_FLAG)"
	@echo "FORTRAN_FREE_STD_MODE=$(FORTRAN_FREE_STD_MODE)"
	@echo "FORTRAN_FREE_STD_FLAG=$(FORTRAN_FREE_STD_FLAG)"
	@echo "USE_CCACHE=$(USE_CCACHE)"
	@echo "CCACHE_BIN=$(CCACHE_BIN)"
	@echo "BUILD_CACHE=$(BUILD_CACHE)"
	@echo "BUILD_CACHE_DIR=$(BUILD_CACHE_DIR)"
	@echo "CACHE_KEY=$(CACHE_KEY)"

cache-key:
	@echo "$(CACHE_KEY)"

cache-restore:
	@if [ "$(BUILD_CACHE)" != "1" ]; then \
	  exit 0; \
	elif [ ! -d "$(CACHE_ENTRY)" ]; then \
	  echo "No build cache for key $(CACHE_KEY)"; \
	  exit 0; \
	else \
	  echo "Restoring build cache $(CACHE_KEY)"; \
	  for d in src examples lib include; do \
	    if [ -d "$(CACHE_ENTRY)/$$d" ]; then \
	      cp -a "$(CACHE_ENTRY)/$$d/." "$$d/"; \
	    fi; \
	  done; \
	fi

cache-save:
	@if [ "$(BUILD_CACHE)" != "1" ]; then \
	  exit 0; \
	else \
	  echo "Saving build cache $(CACHE_KEY)"; \
	  mkdir -p "$(CACHE_ENTRY)/src" "$(CACHE_ENTRY)/examples" "$(CACHE_ENTRY)/lib" "$(CACHE_ENTRY)/include"; \
	  for f in src/*.o src/*.mod src/*.d src/.build_config.stamp; do \
	    if [ -e "$$f" ]; then cp -a "$$f" "$(CACHE_ENTRY)/src/"; fi; \
	  done; \
	  for f in examples/*.o examples/*.d examples/.build_config.stamp; do \
	    if [ -e "$$f" ]; then cp -a "$$f" "$(CACHE_ENTRY)/examples/"; fi; \
	  done; \
	  for f in examples/*; do \
	    if [ -f "$$f" ] && [ -x "$$f" ]; then cp -a "$$f" "$(CACHE_ENTRY)/examples/"; fi; \
	  done; \
	  for f in lib/lib*$(PLAT)$(LIBEXT) lib/lib*$(PLAT)$(LIBEXT_SHARED); do \
	    if [ -e "$$f" ]; then cp -a "$$f" "$(CACHE_ENTRY)/lib/"; fi; \
	  done; \
	  if [ -f include/mumps_int_def.h ]; then cp -a include/mumps_int_def.h "$(CACHE_ENTRY)/include/"; fi; \
	  printf '%s\n' "$(CACHE_SIGNATURE)" > "$(CACHE_ENTRY)/signature.txt"; \
	fi

cache-clean:
	@rm -rf "$(BUILD_CACHE_DIR)"

bench-blas:
	./scripts/benchmark_blas.sh

bench-blas-dense:
	./scripts/benchmark_blas_dense.sh

bench-correctness:
	./scripts/check_blas_correctness.sh

bench-db:
	python3 ./scripts/ingest_benchmark_results_sqlite.py

bench-all-db:
	./scripts/run_all_benchmarks_to_sqlite.sh

bench-mkl-amd-vitis-db:
	BENCH_VENDORS="mkl amd-vitis" ./scripts/run_all_benchmarks_to_sqlite.sh

bench-web:
	uvicorn webapp.main:app --reload

# Webapp targets
webapp:
	@echo "Starting MUMPS Benchmark Results webapp on http://localhost:9001"
	@python3 -m webapp.main --port 9001

webapp-stop:
	@echo "Stopping webapp..."
	@pkill -f "python3 -m webapp.main" || pkill -f "uvicorn webapp.main:app" || echo "No webapp process found"

webapp-open:
	@echo "Opening webapp in browser..."
	@command -v xdg-open >/dev/null 2>&1 && xdg-open http://localhost:9001 || \
	 command -v open >/dev/null 2>&1 && open http://localhost:9001 || \
	 echo "Please open http://localhost:9001 in your browser"

webapp-health:
	@echo "Checking webapp health..."
	@curl -s http://localhost:9001/health | python3 -m json.tool || echo "Webapp not responding"
