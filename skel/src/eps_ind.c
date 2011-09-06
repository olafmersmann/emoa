/*===========================================================================*
 * eps_ind.c: implements the unary epsilon indicator as proposed in
 *            Zitzler, E., Thiele, L., Laumanns, M., Fonseca, C., and
 *            Grunert da Fonseca, V (2003): Performance Assessment of
 *            Multiobjective Optimizers: An Analysis and Review. IEEE
 *            Transactions on Evolutionary Computation, 7(2), 117-132.
 *
 * IMPORTANT:
 *   To make the epsilon indicator work for mixed optimization problems
 *   where some objectives are to be maximized while others are to be
 *   minimized, in the case of minimization the value -epsilon (for the
 *   additive case) resp. 1/epsilon (for the multiplicative version) is
 *   considered and returned. Example: suppose f1 is to be minimized and
 *   f2 to be maximized, and the multiplicative epsilon value computed by
 *   this program is 3.0; this means that the considered nondominated front
 *   needs to be multiplied by 1/3 for all f1 values and by 3 for all
 *   f2 values. Thus, independently of which type of problem one considers
 *   (minimization, maximization, mixed minimization/maximization), a lower
 *   indicator value corresponds to a better approximation set.
 *
 * Author:
 *   Eckart Zitzler, February 3, 2005 / last update August 9, 2005
 *
 * Adapted for use in R by:
 *   Olaf Mersmann <olafm@statistik.tu-dortmund.de>
 */

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

typedef enum  {additive, multiplicative} method_t;

#define MAX(A, B) ((A > B) ? A : B)
#define MIN(A, B) ((A < B) ? A : B)

/*
 * calc_eps_ind:
 *  a - reference front
 *  b - current front
 */
static double calc_eps_ind(double  *a, const size_t a_points,
			   double  *b, const size_t b_points,
			   const size_t number_of_objectives, 
                           method_t method) {
    size_t i, j, current_objective;
    double  maximal_eps, minimal_pointwise_eps, pointwise_eps, eps;
    
    maximal_eps = (additive == method) ? -DBL_MAX : 0.0;
    for (i = 0; i < a_points; i++) {
        minimal_pointwise_eps = DBL_MAX;
	const double *ai = a + i * number_of_objectives;
        for (j = 0; j < b_points; j++) {
            pointwise_eps = -DBL_MAX;
	    const double *bj = b + j * number_of_objectives;
	    for (current_objective = 0; current_objective < number_of_objectives; ++current_objective) {
		switch (method) {
		case additive:
		    eps = bj[current_objective] - ai[current_objective];
		    break;
		case multiplicative:
		    eps = bj[current_objective] / ai[current_objective];
		    break;
		}
                pointwise_eps = MAX(pointwise_eps, eps);
	    }
            minimal_pointwise_eps = MIN(minimal_pointwise_eps, pointwise_eps);
	}
        maximal_eps = MAX(maximal_eps, minimal_pointwise_eps);
    }
    return maximal_eps;
}


#define UNPACK_REAL_VECTOR(S, D, N)             \
    double *D = REAL(S);			\
    const R_len_t N = length(S);

#define UNPACK_REAL_MATRIX(S, D, N, K)          \
    double *D = REAL(S);			\
    const R_len_t N = nrows(S);			\
    const R_len_t K = ncols(S);


SEXP do_eps_ind(SEXP s_data, SEXP s_ref) {
    /* Unpack arguments */
    UNPACK_REAL_MATRIX(s_data, data, number_of_objectives, number_of_points);
    UNPACK_REAL_MATRIX(s_ref, ref, number_of_ref_objectives, number_of_ref_points);
    
    if (number_of_ref_objectives != number_of_objectives)
        error("Reference and current front must have the same dimension.");
    
    /* Calculate criterion */
    double res = calc_eps_ind(ref, number_of_ref_points, 
                              data, number_of_points, number_of_objectives, 
                              additive);
    return ScalarReal(res);
}
