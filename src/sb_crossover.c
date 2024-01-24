#include "extern.h"

#include <R.h>
#include <Rinternals.h>

#include "sexp_macros.h"
#include "utilities.h"

#define MIN(A, B) ((A < B) ? (A) : (B))
#define MAX(A, B) ((A > B) ? (A) : (B))

static const double min_diff = 1.0e-14;

/*
 * calc_betaq - calculate betaq parameter based on beta and eta.
 */
static R_INLINE double calc_betaq(const double beta, const double eta) {
  const double rand = unif_rand();
  const double alpha = 2.0 - pow(beta, -(eta + 1.0));
  if (rand <= (1.0 / alpha)) {
    return (pow(rand * alpha, 1.0 / (eta + 1.0)));
  } else {
    return (pow(1.0 / (2.0 - rand * alpha), 1.0 / (eta + 1.0)));
  }
}

/*
 * Perform a simulated binary crossover between two individuals and
 * return two new individuals.
 */
SEXP do_sbx(SEXP s_parents, SEXP s_lb, SEXP s_ub, SEXP s_eta, SEXP s_p) {
  SEXP s_res;
  double betaq;

  /* Unpack arguments: */
  UNPACK_REAL_MATRIX(s_parents, parents, d, n);
  const double *parent1 = parents;
  const double *parent2 = parents + d;
  UNPACK_REAL_VECTOR(s_lb, lb, dlb);
  UNPACK_REAL_VECTOR(s_ub, ub, dub);
  UNPACK_REAL(s_eta, eta);
  UNPACK_REAL(s_p, p);

  /* Sanity checks; */
  if (n < 2)
    error("do_sbx called with more less than two parents.");
  if (n > 2)
    warning("do_sbx called with more than two parents. Only the first two are "
            "used.");

  if (dlb != d || dub != d)
    error("do_sbx: Dimension of parents and bounds not equal.");

  /* Allocate result matrix: */
  PROTECT(s_res = allocMatrix(REALSXP, d, 2));
  double *child1 = REAL(s_res);
  double *child2 = REAL(s_res) + d;

  GetRNGstate();
  for (int i = 0; i < d; ++i) {
    /* Perform crossover for i-th dimension with probability p, if
     * the two parents differ by at least min_diff.
     */
    if (unif_rand() <= p && fabs(parent1[i] - parent2[i]) > min_diff) {
      const double y1 = MIN(parent1[i], parent2[i]);
      const double y2 = MAX(parent1[i], parent2[i]);
      const double yl = lb[i];
      const double yu = ub[i];

      /* Calculate offsprint: */
      betaq = calc_betaq(1.0 + (2.0 * (y1 - yl) / (y2 - y1)), eta);
      const double c1 =
          clip_to_limits(0.5 * ((y1 + y2) - betaq * (y2 - y1)), yl, yu);

      betaq = calc_betaq(1.0 + (2.0 * (yu - y2) / (y2 - y1)), eta);
      const double c2 =
          clip_to_limits(0.5 * ((y1 + y2) + betaq * (y2 - y1)), yl, yu);

      /* Flip coin to see which offspring gets which new
       * value:
       */
      if (flip_coin()) {
        child1[i] = c2;
        child2[i] = c1;
      } else {
        child1[i] = c1;
        child2[i] = c2;
      }
    } else {
      child1[i] = parent1[i];
      child2[i] = parent2[i];
    }
  }
  PutRNGstate();
  UNPROTECT(1); /* s_res */
  return (s_res);
}
