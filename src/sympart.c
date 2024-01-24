/*
 * sympart.c - SYM-PART test function as used in the CEC 2007 EOMA competition
 *
 * Based on source code by Huang Ling <huangling@pmail.ntu.edu.sg>
 *
 * Author:
 *   Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 */
#include "extern.h"

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include <math.h>
#include <string.h> /* for memcpy */

#include "sexp_macros.h"

static const double a = 1.0;
static const double b = 10.0;
/* static const double c = 8.0; */
static const double c2 = 10.0; /* c + 2*a */

#ifndef PI
#define PI M_PI
#endif

#define MAX(a, b) ((a > b) ? a : b)

/*
 * find_tile - find in which tile of (-1, 0, 1) x lies.
 */
static R_INLINE int find_tile(const double x, const double c) {
  const double xx = fabs(x);
  const double c2 = c / 2.0;

  int tmp = (int)ceil((xx - c2) / c);
  if (tmp > 1)
    tmp = 1;
  if (x < 0)
    tmp = -tmp;

  return tmp;
}

SEXP do_sympart(SEXP s_x) {
  R_len_t k;
  SEXP s_res;

  const double omega = PI / 4.0;
  const double si = sin(omega);
  const double co = cos(omega);

  UNPACK_REAL_VECTOR(s_x, x, n);

  /* Allocate return vector */
  PROTECT(s_res = allocVector(REALSXP, 2));
  double *res = REAL(s_res);
  res[0] = res[1] = 0.0;

  /* Copy input and rotate: */
  double *xx = (double *)R_alloc(n, sizeof(double));
  for (k = 0; k + 1 < n; k += 2) {
    const double xk = x[k];
    const double xkp = x[k + 1];
    xx[k] = co * xk - si * xkp;
    xx[k + 1] = si * xk + co * xkp;
  }

  /* Find tile (i, j) */
  int i = find_tile(xx[0], c2);
  int j = find_tile(xx[1], b);

  /* Calculate function values */
  for (k = 0; k < n; ++k) {
    double xxk = xx[k];
    if ((k % 2) == 0) {
      res[0] += pow(xxk + a - i * c2, 2);
      res[1] += pow(xxk - a - i * c2, 2);
    } else {
      res[0] += pow(xxk - j * b, 2);
      res[1] += pow(xxk - j * b, 2);
    }
  }
  res[0] /= n;
  res[1] /= n;
  UNPROTECT(1); /* s_res */
  return s_res;
}
