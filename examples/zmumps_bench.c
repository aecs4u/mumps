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
#include <math.h>
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
          "Usage: %s [--ngrid N] [--nrhs R] [--ordering O]\n"
          "  --ngrid N    Grid size per dimension (default: 220)\n"
          "  --nrhs R     Number of right-hand sides (default: 4)\n"
          "  --ordering O Ordering method (0=AMD, 2=AMF, 3=SCOTCH, 4=PORD, 5=METIS, 6=QAMD, 7=Auto, default: 7)\n",
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

static double abs_complex(double real, double imag) {
  return hypot(real, imag);
}

static double compute_relative_residual_inf(MUMPS_INT n, MUMPS_INT8 nnz, const MUMPS_INT *irn,
                                            const MUMPS_INT *jcn, const ZMUMPS_COMPLEX *a,
                                            const ZMUMPS_COMPLEX *x, const ZMUMPS_COMPLEX *b,
                                            MUMPS_INT nrhs) {
  size_t nsz = (size_t)n;
  size_t rhs_count = (size_t)nrhs;
  double *row_abs_sum = NULL;
  double *ax_r = NULL;
  double *ax_i = NULL;
  double a_inf = 0.0;
  double worst_rel = 0.0;
  int64_t e;

  if (n <= 0 || nrhs <= 0) return -1.0;
  if ((uint64_t)n > SIZE_MAX / sizeof(double)) return -1.0;
  if ((uint64_t)nrhs > SIZE_MAX / sizeof(double) || rhs_count > SIZE_MAX / nsz) return -1.0;

  row_abs_sum = (double *)calloc(nsz, sizeof(double));
  ax_r = (double *)malloc(nsz * sizeof(double));
  ax_i = (double *)malloc(nsz * sizeof(double));
  if (!row_abs_sum || !ax_r || !ax_i) goto fail;

  for (e = 0; e < (int64_t)nnz; ++e) {
    size_t row = (size_t)(irn[e] - 1);
    if (row < nsz) row_abs_sum[row] += abs_complex(a[e].r, a[e].i);
  }
  for (size_t i = 0; i < nsz; ++i) {
    if (row_abs_sum[i] > a_inf) a_inf = row_abs_sum[i];
  }
  if (a_inf == 0.0) a_inf = 1.0;

  for (size_t rhs_id = 0; rhs_id < rhs_count; ++rhs_id) {
    const ZMUMPS_COMPLEX *x_col = x + rhs_id * nsz;
    const ZMUMPS_COMPLEX *b_col = b + rhs_id * nsz;
    double x_inf = 0.0;
    double b_inf = 0.0;
    double residual_inf = 0.0;
    double denom;
    double rel;

    memset(ax_r, 0, nsz * sizeof(double));
    memset(ax_i, 0, nsz * sizeof(double));

    for (size_t i = 0; i < nsz; ++i) {
      double xabs = abs_complex(x_col[i].r, x_col[i].i);
      double babs = abs_complex(b_col[i].r, b_col[i].i);
      if (xabs > x_inf) x_inf = xabs;
      if (babs > b_inf) b_inf = babs;
    }

    for (e = 0; e < (int64_t)nnz; ++e) {
      size_t row = (size_t)(irn[e] - 1);
      size_t col = (size_t)(jcn[e] - 1);
      double ar, ai, xr, xi;
      if (row >= nsz || col >= nsz) continue;
      ar = a[e].r;
      ai = a[e].i;
      xr = x_col[col].r;
      xi = x_col[col].i;
      ax_r[row] += ar * xr - ai * xi;
      ax_i[row] += ar * xi + ai * xr;
    }

    for (size_t i = 0; i < nsz; ++i) {
      double rr = ax_r[i] - b_col[i].r;
      double ri = ax_i[i] - b_col[i].i;
      double rabs = hypot(rr, ri);
      if (rabs > residual_inf) residual_inf = rabs;
    }
    denom = a_inf * x_inf + b_inf;
    if (denom <= 1e-30) denom = 1.0;
    rel = residual_inf / denom;
    if (rel > worst_rel) worst_rel = rel;
  }

  free(row_abs_sum);
  free(ax_r);
  free(ax_i);
  return worst_rel;

fail:
  free(row_abs_sum);
  free(ax_r);
  free(ax_i);
  return -1.0;
}

int main(int argc, char **argv) {
  int ierr, myid;
  int ngrid = 220;
  int nrhs = 4;
  int ordering = 7;
  int i, j;
  int error = 0;
  int infog1 = 0, infog2 = 0;
  int64_t n64, nnz_max64, rhs_size64;
  MUMPS_INT n;
  MUMPS_INT8 nnz_max, nnz_final;
  MUMPS_INT *irn = NULL, *jcn = NULL;
  ZMUMPS_COMPLEX *a = NULL, *rhs = NULL, *rhs_ref = NULL;
  int64_t idx = 0;
  ZMUMPS_STRUC_C id;
  double t0, t1;
  double rel_residual_inf = -1.0;

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
    } else if (strcmp(argv[i], "--ordering") == 0) {
      if (i + 1 >= argc || !parse_positive_int(argv[i + 1], &ordering)) {
        usage(argv[0]);
        return 2;
      }
      if (ordering < 0 || ordering > 7 || ordering == 1) {
        fprintf(stderr, "Ordering must be 0, 2-7 (1 is reserved by MUMPS)\n");
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
  rhs_ref = (ZMUMPS_COMPLEX *)malloc((size_t)rhs_size64 * sizeof(ZMUMPS_COMPLEX));
  if (!irn || !jcn || !a || !rhs || !rhs_ref) {
    if (myid == 0) fprintf(stderr, "Allocation failed.\n");
    free(irn);
    free(jcn);
    free(a);
    free(rhs);
    free(rhs_ref);
    MPI_Finalize();
    return 1;
  }

  for (int64_t rhs_id = 0; rhs_id < rhs_size64; ++rhs_id) {
    rhs[rhs_id].r = 1.0;
    rhs[rhs_id].i = 0.0;
    rhs_ref[rhs_id].r = 1.0;
    rhs_ref[rhs_id].i = 0.0;
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
  id.ICNTL(7) = ordering;

  t0 = MPI_Wtime();
  id.job = 6;
  zmumps_c(&id);
  t1 = MPI_Wtime();
  infog1 = id.infog[0];
  infog2 = id.infog[1];

  if (infog1 < 0) {
    if (myid == 0) {
      fprintf(stderr, "ZMUMPS ERROR: INFOG(1)=%d INFOG(2)=%d\n", infog1, infog2);
    }
    error = 1;
  }

  if (myid == 0 && !error) {
    rel_residual_inf = compute_relative_residual_inf(n, nnz_final, irn, jcn, a, rhs, rhs_ref, (MUMPS_INT)nrhs);
    if (rel_residual_inf < 0.0) {
      fprintf(stderr, "Failed to compute residual.\n");
      error = 1;
    }
  }

  id.job = JOB_END;
  zmumps_c(&id);

  if (myid == 0 && !error) {
    printf("ZMUMPS_BENCH_N=%d\n", (int)n);
    printf("ZMUMPS_BENCH_NNZ=%lld\n", (long long)nnz_final);
    printf("ZMUMPS_BENCH_NRHS=%d\n", nrhs);
    printf("ZMUMPS_BENCH_ORDERING=%d\n", ordering);
    printf("ZMUMPS_BENCH_JOB6_SECONDS=%.6f\n", t1 - t0);
    printf("ZMUMPS_BENCH_INFOG1=%d\n", infog1);
    printf("ZMUMPS_BENCH_INFOG2=%d\n", infog2);
    printf("ZMUMPS_BENCH_REL_RESIDUAL_INF=%.12e\n", rel_residual_inf);
  }

  free(irn);
  free(jcn);
  free(a);
  free(rhs);
  free(rhs_ref);
  MPI_Finalize();
  return error ? 1 : 0;
}
