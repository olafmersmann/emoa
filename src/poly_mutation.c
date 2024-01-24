#include "extern.h"

#include <R.h>
#include <Rinternals.h>

#include "sexp_macros.h"
#include "utilities.h"

/*
 * Perform polynomial mutation of x with probability p.
 */
SEXP do_pm(SEXP s_x, SEXP s_lb, SEXP s_ub, SEXP s_eta, SEXP s_p) {
  SEXP s_res;
  double deltaq, rnd;

#if FALSE
  /* Only mutate in half of all calls: */
  if (flip_coin())
    return s_x;
#endif

  /* Unpack arguments: */
  UNPACK_REAL_VECTOR(s_x, x, d);
  UNPACK_REAL_VECTOR(s_lb, lb, dlb);
  UNPACK_REAL_VECTOR(s_ub, ub, dub);
  UNPACK_REAL(s_eta, eta);
  UNPACK_REAL(s_p, p);
  const double mpow = 1.0 / (eta + 1.0);

  /* Sanity checks: */
  if (dlb != d || dub != d)
    error("do_pm: Dimension of individual and bounds differ.");

  /* Allocate results: */
  PROTECT(s_res = allocVector(REALSXP, d));
  double *res = REAL(s_res);

  GetRNGstate();
  for (int i = 0; i < d; ++i) {
    /* Mutate with probability p: */
    if (unif_rand() < p) {
      const double delta = ub[i] - lb[i];
      rnd = unif_rand();
      if (rnd <= 0.5) {
        const double xy = 1.0 - (x[i] - lb[i]) / delta;
        deltaq =
            pow(2.0 * rnd + (1.0 - 2.0 * rnd) * pow(xy, eta + 1.0), mpow) - 1.0;
      } else {
        const double xy = 1.0 - (ub[i] - x[i]) / delta;
        deltaq = 1.0 -
                 pow(2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * pow(xy, eta + 1.0),
                     mpow);
      }
      res[i] = clip_to_limits(x[i] + deltaq * delta, lb[i], ub[i]);
    } else {
      res[i] = x[i];
    }
  }
  PutRNGstate();
  UNPROTECT(1); /* s_res */
  return (s_res);
}
