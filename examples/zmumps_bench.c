/*
 *
 *  This file is part of MUMPS 5.8.2, released
 *  on Mon Jan 12 15:17:08 UTC 2026
 *
 */
/*
 * Benchmark driver for the C interface to ZMUMPS (complex double precision).
 * It builds a 2D 5-point Laplacian and times JOB=6 (analysis+factor+solve).
 */
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>

#include "mpi.h"
#include "zmumps_c.h"

#define JOB_INIT -1
#define JOB_END -2
#define USE_COMM_WORLD -987654

#define ICNTL(I) icntl[(I)-1]

static void usage(const char *prog) {
  fprintf(stderr,
          "Usage: %s [--ngrid N] [--nrhs R]\n"
          "  --ngrid N  Grid size per dimension (default: 220)\n"
          "  --nrhs R   Number of right-hand sides (default: 4)\n",
          prog);
}

static int parse_positive_int(const char *s, int *out) {
  long long v;
  char *endptr = NULL;
  v = strtoll(s, &endptr, 10);
  if (endptr == s || *endptr != '\0' || v <= 0 || v > INT_MAX) return 0;
  *out = (int)v;
  return 1;
}

int main(int argc, char **argv) {
  int ierr, myid;
  int ngrid = 220;
  int nrhs = 4;
  int i, j;
  int error = 0;
  int64_t n64, nnz_max64, rhs_size64;
  MUMPS_INT n;
  MUMPS_INT8 nnz_max, nnz_final;
  MUMPS_INT *irn = NULL, *jcn = NULL;
  ZMUMPS_COMPLEX *a = NULL, *rhs = NULL;
  int64_t idx = 0;
  ZMUMPS_STRUC_C id;
  double t0, t1;

  for (i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--ngrid") == 0) {
      if (i + 1 >= argc || !parse_positive_int(argv[i + 1], &ngrid)) {
        usage(argv[0]);
        return 2;
      }
      ++i;
    } else if (strcmp(argv[i], "--nrhs") == 0) {
      if (i + 1 >= argc || !parse_positive_int(argv[i + 1], &nrhs)) {
        usage(argv[0]);
        return 2;
      }
      ++i;
    } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
      usage(argv[0]);
      return 0;
    } else {
      fprintf(stderr, "Unknown argument: %s\n", argv[i]);
      usage(argv[0]);
      return 2;
    }
  }

  ierr = MPI_Init(&argc, &argv);
  ierr = MPI_Comm_rank(MPI_COMM_WORLD, &myid);
  if (ierr != 0) {
    fprintf(stderr, "MPI_Init or MPI_Comm_rank failed.\n");
    return 1;
  }

  n64 = (int64_t)ngrid * (int64_t)ngrid;
  nnz_max64 = 5LL * n64;
  rhs_size64 = n64 * (int64_t)nrhs;
  if (((MUMPS_INT)n64) != n64 || ((MUMPS_INT8)nnz_max64) != nnz_max64) {
    if (myid == 0) {
      fprintf(stderr, "Problem too large for MUMPS integer type. ngrid=%d\n", ngrid);
    }
    MPI_Finalize();
    return 1;
  }
  if (rhs_size64 <= 0 || (uint64_t)rhs_size64 > (SIZE_MAX / sizeof(ZMUMPS_COMPLEX))) {
    if (myid == 0) {
      fprintf(stderr, "RHS size overflow. ngrid=%d nrhs=%d\n", ngrid, nrhs);
    }
    MPI_Finalize();
    return 1;
  }

  n = (MUMPS_INT)n64;
  nnz_max = (MUMPS_INT8)nnz_max64;

  irn = (MUMPS_INT *)malloc((size_t)nnz_max * sizeof(MUMPS_INT));
  jcn = (MUMPS_INT *)malloc((size_t)nnz_max * sizeof(MUMPS_INT));
  a = (ZMUMPS_COMPLEX *)malloc((size_t)nnz_max * sizeof(ZMUMPS_COMPLEX));
  rhs = (ZMUMPS_COMPLEX *)malloc((size_t)rhs_size64 * sizeof(ZMUMPS_COMPLEX));
  if (!irn || !jcn || !a || !rhs) {
    if (myid == 0) fprintf(stderr, "Allocation failed.\n");
    free(irn);
    free(jcn);
    free(a);
    free(rhs);
    MPI_Finalize();
    return 1;
  }

  for (int64_t rhs_id = 0; rhs_id < rhs_size64; ++rhs_id) {
    rhs[rhs_id].r = 1.0;
    rhs[rhs_id].i = 0.0;
  }

  for (i = 0; i < ngrid; ++i) {
    for (j = 0; j < ngrid; ++j) {
      MUMPS_INT row = (MUMPS_INT)(i * ngrid + j + 1);
      irn[idx] = row; jcn[idx] = row; a[idx].r = 4.0; a[idx].i = 0.0; idx++;
      if (i > 0)         { irn[idx] = row; jcn[idx] = row - ngrid; a[idx].r = -1.0; a[idx].i = 0.0; idx++; }
      if (i < ngrid - 1) { irn[idx] = row; jcn[idx] = row + ngrid; a[idx].r = -1.0; a[idx].i = 0.0; idx++; }
      if (j > 0)         { irn[idx] = row; jcn[idx] = row - 1;     a[idx].r = -1.0; a[idx].i = 0.0; idx++; }
      if (j < ngrid - 1) { irn[idx] = row; jcn[idx] = row + 1;     a[idx].r = -1.0; a[idx].i = 0.0; idx++; }
    }
  }
  nnz_final = (MUMPS_INT8)idx;

  id.comm_fortran = USE_COMM_WORLD;
  id.par = 1;
  id.sym = 0;
  id.job = JOB_INIT;
  zmumps_c(&id);

  if (myid == 0) {
    id.n = n;
    id.nnz = nnz_final;
    id.irn = irn;
    id.jcn = jcn;
    id.a = a;
    id.rhs = rhs;
    id.nrhs = (MUMPS_INT)nrhs;
    id.lrhs = n;
  }

  id.ICNTL(1) = -1;
  id.ICNTL(2) = -1;
  id.ICNTL(3) = -1;
  id.ICNTL(4) = 0;

  t0 = MPI_Wtime();
  id.job = 6;
  zmumps_c(&id);
  t1 = MPI_Wtime();

  if (id.infog[0] < 0) {
    if (myid == 0) {
      fprintf(stderr, "ZMUMPS ERROR: INFOG(1)=%d INFOG(2)=%d\n", id.infog[0], id.infog[1]);
    }
    error = 1;
  }

  id.job = JOB_END;
  zmumps_c(&id);

  if (myid == 0 && !error) {
    printf("ZMUMPS_BENCH_N=%d\n", (int)n);
    printf("ZMUMPS_BENCH_NNZ=%lld\n", (long long)nnz_final);
    printf("ZMUMPS_BENCH_NRHS=%d\n", nrhs);
    printf("ZMUMPS_BENCH_JOB6_SECONDS=%.6f\n", t1 - t0);
  }

  free(irn);
  free(jcn);
  free(a);
  free(rhs);
  MPI_Finalize();
  return error ? 1 : 0;
}
