/*
 *
 *  This file is part of MUMPS 5.8.2, released
 *  on Mon Jan 12 15:17:08 UTC 2026
 *
 */
/*
 * Benchmark driver for the C interface to SMUMPS (single precision).
 * It builds a 2D 5-point Laplacian and times JOB=6 (analysis+factor+solve).
 */
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpi.h"
#include "smumps_c.h"

#define JOB_INIT -1
#define JOB_END -2
#define USE_COMM_WORLD -987654

#define ICNTL(I) icntl[(I)-1]

static void usage(const char *prog) {
  fprintf(stderr,
          "Usage: %s [--ngrid N] [--nrhs R] [--ordering O] [--matrix-file F] [--sym S]\n"
          "  --ngrid N      Grid size per dimension (default: 220, ignored with --matrix-file)\n"
          "  --nrhs R       Number of right-hand sides (default: 4)\n"
          "  --ordering O   Ordering method (0=AMD, 2=AMF, 3=SCOTCH, 4=PORD, 5=METIS, 6=QAMD, 7=Auto, default: 7)\n"
          "  --matrix-file F  Load matrix from COO file instead of generating Laplacian\n"
          "  --sym S        Symmetry flag (0=unsymmetric, 1=SPD, 2=general symmetric, default: 0)\n",
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

static int load_coo_file(const char *path, MUMPS_INT *out_n, MUMPS_INT8 *out_nnz,
                        MUMPS_INT **out_irn, MUMPS_INT **out_jcn, float **out_a) {
  FILE *fp;
  char line[1024];
  int n_int, nnz_int;
  MUMPS_INT n;
  MUMPS_INT8 nnz;
  MUMPS_INT *irn = NULL, *jcn = NULL;
  float *a = NULL;
  int64_t count = 0;
  int ri, ci;
  double val;

  fp = fopen(path, "r");
  if (!fp) {
    fprintf(stderr, "Cannot open matrix file: %s\n", path);
    return 0;
  }

  while (fgets(line, sizeof(line), fp)) {
    if (line[0] == '#' || line[0] == '%' || line[0] == '\n') continue;
    if (sscanf(line, "%d %d", &n_int, &nnz_int) == 2) break;
    fprintf(stderr, "Invalid COO header in %s\n", path);
    fclose(fp);
    return 0;
  }

  n = (MUMPS_INT)n_int;
  nnz = (MUMPS_INT8)nnz_int;
  if (n <= 0 || nnz <= 0) {
    fprintf(stderr, "Invalid dimensions in %s: n=%d nnz=%d\n", path, n_int, nnz_int);
    fclose(fp);
    return 0;
  }

  irn = (MUMPS_INT *)malloc((size_t)nnz * sizeof(MUMPS_INT));
  jcn = (MUMPS_INT *)malloc((size_t)nnz * sizeof(MUMPS_INT));
  a = (float *)malloc((size_t)nnz * sizeof(float));
  if (!irn || !jcn || !a) {
    fprintf(stderr, "Allocation failed for matrix from %s\n", path);
    free(irn); free(jcn); free(a);
    fclose(fp);
    return 0;
  }

  while (fgets(line, sizeof(line), fp) && count < (int64_t)nnz) {
    if (line[0] == '#' || line[0] == '%' || line[0] == '\n') continue;
    if (sscanf(line, "%d %d %lf", &ri, &ci, &val) != 3) {
      fprintf(stderr, "Invalid COO entry at line %lld in %s\n", (long long)(count + 1), path);
      free(irn); free(jcn); free(a);
      fclose(fp);
      return 0;
    }
    irn[count] = (MUMPS_INT)ri;
    jcn[count] = (MUMPS_INT)ci;
    a[count] = (float)val;
    count++;
  }
  fclose(fp);

  if (count != (int64_t)nnz) {
    fprintf(stderr, "Expected %lld entries but read %lld in %s\n",
            (long long)nnz, (long long)count, path);
    free(irn); free(jcn); free(a);
    return 0;
  }

  *out_n = n;
  *out_nnz = nnz;
  *out_irn = irn;
  *out_jcn = jcn;
  *out_a = a;
  return 1;
}

static double compute_relative_residual_inf(MUMPS_INT n, MUMPS_INT8 nnz, const MUMPS_INT *irn,
                                            const MUMPS_INT *jcn, const float *a, const float *x,
                                            const float *b, MUMPS_INT nrhs) {
  size_t nsz = (size_t)n;
  size_t rhs_count = (size_t)nrhs;
  double *row_abs_sum = NULL;
  double *ax = NULL;
  double a_inf = 0.0;
  double worst_rel = 0.0;
  int64_t e;

  if (n <= 0 || nrhs <= 0) return -1.0;
  if ((uint64_t)n > SIZE_MAX / sizeof(double)) return -1.0;
  if ((uint64_t)nrhs > SIZE_MAX / sizeof(double) || rhs_count > SIZE_MAX / nsz) return -1.0;

  row_abs_sum = (double *)calloc(nsz, sizeof(double));
  ax = (double *)malloc(nsz * sizeof(double));
  if (!row_abs_sum || !ax) goto fail;

  for (e = 0; e < (int64_t)nnz; ++e) {
    size_t row = (size_t)(irn[e] - 1);
    if (row < nsz) row_abs_sum[row] += fabs((double)a[e]);
  }
  for (size_t i = 0; i < nsz; ++i) {
    if (row_abs_sum[i] > a_inf) a_inf = row_abs_sum[i];
  }
  if (a_inf == 0.0) a_inf = 1.0;

  for (size_t rhs_id = 0; rhs_id < rhs_count; ++rhs_id) {
    const float *x_col = x + rhs_id * nsz;
    const float *b_col = b + rhs_id * nsz;
    double x_inf = 0.0;
    double b_inf = 0.0;
    double residual_inf = 0.0;
    double denom;
    double rel;

    memset(ax, 0, nsz * sizeof(double));
    for (size_t i = 0; i < nsz; ++i) {
      double xabs = fabs((double)x_col[i]);
      double babs = fabs((double)b_col[i]);
      if (xabs > x_inf) x_inf = xabs;
      if (babs > b_inf) b_inf = babs;
    }

    for (e = 0; e < (int64_t)nnz; ++e) {
      size_t row = (size_t)(irn[e] - 1);
      size_t col = (size_t)(jcn[e] - 1);
      if (row < nsz && col < nsz) ax[row] += (double)a[e] * (double)x_col[col];
    }

    for (size_t i = 0; i < nsz; ++i) {
      double rabs = fabs(ax[i] - (double)b_col[i]);
      if (rabs > residual_inf) residual_inf = rabs;
    }
    denom = a_inf * x_inf + b_inf;
    if (denom <= 1e-30) denom = 1.0;
    rel = residual_inf / denom;
    if (rel > worst_rel) worst_rel = rel;
  }

  free(row_abs_sum);
  free(ax);
  return worst_rel;

fail:
  free(row_abs_sum);
  free(ax);
  return -1.0;
}

int main(int argc, char **argv) {
  int ierr, myid;
  int ngrid = 220;
  int nrhs = 4;
  int ordering = 7;
  int sym = 0;
  const char *matrix_file = NULL;
  int i, j;
  int error = 0;
  int infog1 = 0, infog2 = 0;
  int64_t n64, nnz_max64, rhs_size64;
  MUMPS_INT n;
  MUMPS_INT8 nnz_max, nnz_final;
  MUMPS_INT *irn = NULL, *jcn = NULL;
  float *a = NULL, *rhs = NULL, *rhs_ref = NULL;
  int64_t idx = 0;
  SMUMPS_STRUC_C id;
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
    } else if (strcmp(argv[i], "--matrix-file") == 0 || strcmp(argv[i], "-f") == 0) {
      if (i + 1 >= argc) {
        usage(argv[0]);
        return 2;
      }
      matrix_file = argv[++i];
    } else if (strcmp(argv[i], "--sym") == 0) {
      if (i + 1 >= argc) {
        usage(argv[0]);
        return 2;
      }
      sym = atoi(argv[++i]);
      if (sym < 0 || sym > 2) {
        fprintf(stderr, "Symmetry must be 0 (unsymmetric), 1 (SPD), or 2 (general symmetric)\n");
        usage(argv[0]);
        return 2;
      }
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

  if (matrix_file) {
    if (!load_coo_file(matrix_file, &n, &nnz_final, &irn, &jcn, &a)) {
      if (myid == 0) fprintf(stderr, "Failed to load matrix from %s\n", matrix_file);
      MPI_Finalize();
      return 1;
    }
    rhs_size64 = (int64_t)n * (int64_t)nrhs;
    if (rhs_size64 <= 0 || (uint64_t)rhs_size64 > (SIZE_MAX / sizeof(float))) {
      if (myid == 0) fprintf(stderr, "RHS size overflow. n=%d nrhs=%d\n", (int)n, nrhs);
      free(irn); free(jcn); free(a);
      MPI_Finalize();
      return 1;
    }
    rhs = (float *)malloc((size_t)rhs_size64 * sizeof(float));
    rhs_ref = (float *)malloc((size_t)rhs_size64 * sizeof(float));
    if (!rhs || !rhs_ref) {
      if (myid == 0) fprintf(stderr, "Allocation failed.\n");
      free(irn); free(jcn); free(a); free(rhs); free(rhs_ref);
      MPI_Finalize();
      return 1;
    }
    for (int64_t rhs_id = 0; rhs_id < rhs_size64; ++rhs_id) {
      rhs[rhs_id] = 1.0f;
      rhs_ref[rhs_id] = 1.0f;
    }
    if (myid == 0) {
      printf("SMUMPS_BENCH_MATRIX_FILE=%s\n", matrix_file);
    }
  } else {
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
    if (rhs_size64 <= 0 || (uint64_t)rhs_size64 > (SIZE_MAX / sizeof(float))) {
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
    a = (float *)malloc((size_t)nnz_max * sizeof(float));
    rhs = (float *)malloc((size_t)rhs_size64 * sizeof(float));
    rhs_ref = (float *)malloc((size_t)rhs_size64 * sizeof(float));
    if (!irn || !jcn || !a || !rhs || !rhs_ref) {
      if (myid == 0) fprintf(stderr, "Allocation failed.\n");
      free(irn); free(jcn); free(a); free(rhs); free(rhs_ref);
      MPI_Finalize();
      return 1;
    }

    for (int64_t rhs_id = 0; rhs_id < rhs_size64; ++rhs_id) {
      rhs[rhs_id] = 1.0f;
      rhs_ref[rhs_id] = 1.0f;
    }

    for (i = 0; i < ngrid; ++i) {
      for (j = 0; j < ngrid; ++j) {
        MUMPS_INT row = (MUMPS_INT)(i * ngrid + j + 1);
        irn[idx] = row; jcn[idx] = row; a[idx] = 4.0f; idx++;
        if (i > 0)         { irn[idx] = row; jcn[idx] = row - ngrid; a[idx] = -1.0f; idx++; }
        if (i < ngrid - 1) { irn[idx] = row; jcn[idx] = row + ngrid; a[idx] = -1.0f; idx++; }
        if (j > 0)         { irn[idx] = row; jcn[idx] = row - 1;     a[idx] = -1.0f; idx++; }
        if (j < ngrid - 1) { irn[idx] = row; jcn[idx] = row + 1;     a[idx] = -1.0f; idx++; }
      }
    }
    nnz_final = (MUMPS_INT8)idx;
  }

  id.comm_fortran = USE_COMM_WORLD;
  id.par = 1;
  id.sym = sym;
  id.job = JOB_INIT;
  smumps_c(&id);

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
  smumps_c(&id);
  t1 = MPI_Wtime();
  infog1 = id.infog[0];
  infog2 = id.infog[1];

  if (infog1 < 0) {
    if (myid == 0) {
      fprintf(stderr, "SMUMPS ERROR: INFOG(1)=%d INFOG(2)=%d\n", infog1, infog2);
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
  smumps_c(&id);

  if (myid == 0 && !error) {
    printf("SMUMPS_BENCH_N=%d\n", (int)n);
    printf("SMUMPS_BENCH_NNZ=%lld\n", (long long)nnz_final);
    printf("SMUMPS_BENCH_NRHS=%d\n", nrhs);
    printf("SMUMPS_BENCH_ORDERING=%d\n", ordering);
    printf("SMUMPS_BENCH_JOB6_SECONDS=%.6f\n", t1 - t0);
    printf("SMUMPS_BENCH_INFOG1=%d\n", infog1);
    printf("SMUMPS_BENCH_INFOG2=%d\n", infog2);
    printf("SMUMPS_BENCH_REL_RESIDUAL_INF=%.12e\n", rel_residual_inf);
  }

  free(irn);
  free(jcn);
  free(a);
  free(rhs);
  free(rhs_ref);
  MPI_Finalize();
  return error ? 1 : 0;
}
