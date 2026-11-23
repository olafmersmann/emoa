/**
 * hypervolume.c - Interface to hypervolume calculation code.
 *
 * Authors:
 *  Olaf Mersmann    <olafm@statistik.tu-dortmund.de>
 *
 */
#include "extern.h"

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>
#include <R_ext/Utils.h>

#include "sexp_macros.h"

#include "hv.h"

void calc_hv_contrib_2d(const double *data, const double *ref, double *res,
                        const int n) {
  double *x = (double *)R_alloc(n, sizeof(double));
  int *index = (int *)R_alloc(n, sizeof(int));
  int count = 0;
  for (int i = 0; i < n; ++i) {
    const double *point = data + 2 * (size_t)i;
    res[i] = 0.0;
    if (point[0] < ref[0] && point[1] < ref[1]) {
      x[count] = point[0];
      index[count] = i;
      ++count;
    }
  }

  if (count > 1)
    R_qsort_I(x, index, 1, count);

  double lowest_y = ref[1];
  double second_y = ref[1];
  int lowest_index = 0;
  for (int i = 0; i < count; ++i) {
    const double y = data[2 * (size_t)index[i] + 1];
    if (y < lowest_y) {
      second_y = lowest_y;
      lowest_y = y;
      lowest_index = index[i];
    } else if (y < second_y) {
      second_y = y;
    }

    /* Only the lowest rectangle covers the strip below the second lowest. */
    const double next_x = i + 1 < count ? x[i + 1] : ref[0];
    if (x[i] < next_x && lowest_y < second_y)
      res[lowest_index] += (next_x - x[i]) * (second_y - lowest_y);
  }
}

void calc_hv_contrib_anyd(double *data, const double *ref, double *res,
                          const size_t n, size_t k) {
  const double tothv = fpli_hv(data, k, n, ref);
  for (int i = 0; i < n; ++i) {
    const double ihv = fpli_hv(data + k, k, n - 1, ref);
    res[i] = tothv - ihv;
    /* Swap ith and last row in data matrix: */
    if (i != (n - 1)) {
      for (int j = 0; j < k; ++j) {
        double tmp = data[k * (i + 1) + j];
        data[k * (i + 1) + j] = data[j];
        data[j] = tmp;
      }
    }
  }
}

SEXP do_dominated_hypervolume(SEXP s_data, SEXP s_ref) {
  SEXP s_res;

  /* Unpack arguments:
   *
   * Since we need s_data in row major format, it is passed in
   * transposed in column major format (R's internal format for
   * matricies). That means the number of rows and columns are
   * switched.
   */
  UNPACK_REAL_MATRIX(s_data, data, k_data, n_data);
  UNPACK_REAL_VECTOR(s_ref, ref, n_ref);

  if (n_ref != k_data)
    error("ref and data must have the same dimension.");

  /* Allocate result */
  PROTECT(s_res = allocVector(REALSXP, 1));
  double *res = REAL(s_res);

  res[0] = fpli_hv(data, k_data, n_data, ref);

  UNPROTECT(1); /* s_res */
  return s_res;
}

SEXP do_hv_contrib(SEXP s_data, SEXP s_ref) {
  SEXP s_res;
  double *ddata;

  UNPACK_REAL_MATRIX(s_data, data, k_data, n_data);
  UNPACK_REAL_VECTOR(s_ref, ref, n_ref);

  if (n_ref != k_data)
    error("ref and data must have the same dimension.");

  /* Allocate result */
  PROTECT(s_res = allocVector(REALSXP, n_data));
  double *res = REAL(s_res);

  switch (k_data) {
  case 2:
    calc_hv_contrib_2d(data, ref, res, n_data);
    break;
  default:
    /* calc_hv_contrib modifies 'data', so we need to make sure we
     * duplicate it before use.
     */
    ddata = (double *)R_alloc(k_data * n_data, sizeof(double));
    memcpy(ddata, data, sizeof(double) * k_data * n_data);
    calc_hv_contrib_anyd(ddata, ref, res, n_data, k_data);
    break;
  }
  UNPROTECT(1); /* s_res */
  return s_res;
}
