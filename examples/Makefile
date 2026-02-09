#
#  This file is part of MUMPS 5.8.2, released
#  on Mon Jan 12 15:17:08 UTC 2026
#
topdir = ..
libdir = $(topdir)/lib

default: d

.PHONY: default all s d c z multi bench densebench clean
.SECONDEXPANSION:

all:	c z s d multi bench densebench

c:	csimpletest csimpletest_save_restore
z:	zsimpletest zsimpletest_save_restore
s:	ssimpletest ssimpletest_save_restore
d:	dsimpletest dsimpletest_save_restore c_example_save_restore c_example
multi:	multiple_arithmetics_example
bench:  smumps_bench dmumps_bench cmumps_bench zmumps_bench
densebench: sgemm_bench dgemm_bench cgemm_bench zgemm_bench


include $(topdir)/Makefile.inc



LIBSMUMPS = -L$(libdir) -lsmumps$(PLAT) -lmumps_common$(PLAT)
LIBCMUMPS = -L$(libdir) -lcmumps$(PLAT) -lmumps_common$(PLAT)
LIBSDMUMPS = -L$(libdir) -ldmumps$(PLAT) -lsmumps$(PLAT) -lmumps_common$(PLAT)
LIBCZMUMPS = -L$(libdir) -lzmumps$(PLAT) -lcmumps$(PLAT) -lmumps_common$(PLAT)

ssimpletest:   $$@.o
	$(FL) -o $@ $(OPTL) ssimpletest.o  $(LIBSMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

dsimpletest:  $$@.o 
	$(FL) -o $@ $(OPTL) dsimpletest.o  $(LIBSDMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

csimpletest:  $$@.o
	$(FL) -o $@ $(OPTL) csimpletest.o  $(LIBCMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

zsimpletest:  $$@.o
	$(FL) -o $@ $(OPTL) zsimpletest.o  $(LIBCZMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

c_example: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBSDMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)


multiple_arithmetics_example: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBSDMUMPS) $(LIBCZMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

ssimpletest_save_restore:   $$@.o
	$(FL) -o $@ $(OPTL) ssimpletest_save_restore.o  $(LIBSMUMPS) $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)

dsimpletest_save_restore:  $$@.o 
	$(FL) -o $@ $(OPTL) dsimpletest_save_restore.o  $(LIBSDMUMPS) $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)

csimpletest_save_restore:  $$@.o
	$(FL) -o $@ $(OPTL) csimpletest_save_restore.o  $(LIBCMUMPS) $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)

zsimpletest_save_restore:  $$@.o
	$(FL) -o $@ $(OPTL) zsimpletest_save_restore.o  $(LIBCZMUMPS) $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)

c_example_save_restore: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBSDMUMPS) $(LORDERINGS) $(LIBS) $(LIBBLAS) $(LIBOTHERS)

# Sparse benchmarks (all precisions)
smumps_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBSMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

dmumps_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBSDMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

cmumps_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBCMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

zmumps_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBCZMUMPS) $(LORDERINGS) $(LIBS) $(RPATH_OPT) $(LIBBLAS) $(LIBOTHERS)

# Dense benchmarks (all precisions)
sgemm_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBBLAS) $(LIBOTHERS)

dgemm_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBBLAS) $(LIBOTHERS)

cgemm_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBBLAS) $(LIBOTHERS)

zgemm_bench: $$@.o
	$(FL) -o $@ $(OPTL) $@.o $(LIBBLAS) $(LIBOTHERS)

# Compilation rules for Fortran benchmarks
sgemm_bench.o: sgemm_bench.F90
	$(FC) $(OPTF) -I. -I$(topdir)/include -I$(topdir)/src $(INCS) -c sgemm_bench.F90 $(OUTF)$@

dgemm_bench.o: dgemm_bench.F90
	$(FC) $(OPTF) -I. -I$(topdir)/include -I$(topdir)/src $(INCS) -c dgemm_bench.F90 $(OUTF)$@

cgemm_bench.o: cgemm_bench.F90
	$(FC) $(OPTF) -I. -I$(topdir)/include -I$(topdir)/src $(INCS) -c cgemm_bench.F90 $(OUTF)$@

zgemm_bench.o: zgemm_bench.F90
	$(FC) $(OPTF) -I. -I$(topdir)/include -I$(topdir)/src $(INCS) -c zgemm_bench.F90 $(OUTF)$@

.SUFFIXES: .c .F .o
.F.o:
	$(FC) $(OPTF) -I. -I$(topdir)/include -I$(topdir)/src $(INCS) -c $*.F $(OUTF)$*.o
.c.o:
	$(CC) $(OPTC) $(CDEFS) -I. -I$(topdir)/include -I$(topdir)/src $(INCS) -c $*.c $(OUTC)$*.o

clean:
	$(RM) *.o [sdcz]simpletest c_example multiple_arithmetics_example \
		smumps_bench dmumps_bench cmumps_bench zmumps_bench \
		sgemm_bench dgemm_bench cgemm_bench zgemm_bench \
		ssimpletest_save_restore dsimpletest_save_restore csimpletest_save_restore zsimpletest_save_restore c_example_save_restore
