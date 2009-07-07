/*
 * pareto_utilities.c - utility functions relating to pareto dominance
 *
 * Most of these functions could just as well have been implemented in
 * R but were deemed performance critical enough to warrant rewriting
 * them in C.
 *
 * Author:
 *  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 *
 */

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "sexp_macros.h"

/*
 * dominates 
 * 
 * Expects points to be a nxd matrix in _column_ major format. This is
 * the default format used by R.
 *
 * Returns:
 *    -1  iff points[i,] dominates points[j,]
 *     0  iff points[i,] and points[j,] are incomparable
 *     1  iff points[j,] dominates points[i,]
 */
static R_INLINE int dominates(double *p, R_len_t i, R_len_t j, R_len_t nrow, R_len_t d) {
    int i_flagged = 0;
    int j_flagged = 0;
    R_len_t k;
    for (k = 0; k < d; ++k) {
	const double p_ik = p[i + nrow*k];
	const double p_jk = p[j + nrow*k];
	if (p_ik < p_jk) {
	    j_flagged = 1;
	} else if (p_jk < p_ik) {
	    i_flagged = 1;
	}
    }
    return j_flagged - i_flagged;
}

/*
 * nondominated_points
 *
 * Returns a logical vector whose lenght corresponds to the number of
 * rows in the matrix contained in 's_points'. An entry is true if the
 * point defined by the corresponding row in 's_points' is not dominated
 * by any point other point in 's_points'.
 */
SEXP nondominated_points(SEXP s_points) {
    SEXP s_res;
    R_len_t i, j, k;
    
    /* Check argument: */
    if (!isReal(s_points) || !isMatrix(s_points)) 
	error("Argument 's_points' is not a real matrix.");
    UNPACK_REAL_MATRIX(s_points, points, n, d);
    
    /* Allocate result vector: 
     *
     * res[i] == TRUE  <=> i-th point is not dominated
     * res[i] == FALSE <=> i-th point is dominated
     */
    PROTECT(s_res = allocVector(LGLSXP, n));
    int *res = LOGICAL(s_res);
    
    /* Initialy all points are not dominated: */
    for (i = 0; i < n; ++i) 
	res[i] = TRUE;
    
    for (i = 0; i < n; ++i) {
        /* Point is not dominated: */
	if (res[i]) { 
	    for (j = (i+1); j < n; ++j) {
                /* Point j is also not dominated: */
		if (res[j]) { 
		    int dom = dominates(points, i, j, n, d);
		    if (dom > 0) { /* i dominates j */
			res[j] = FALSE;
		    } else if (dom < 0) { /* j dominates i */
			res[i] = FALSE;
		    }
		}
	    }
	}
    }
    UNPROTECT(1); /* s_res */
    return s_res;
}


/*
 * nondominated_order
 *
 * Implements a variant of non dominated sorting, but only returns the
 * front membership instead of actually sorting the matrix s_points.
 */
SEXP nondominated_order(SEXP s_points, SEXP s_tosort) {
    R_len_t i, j;
    SEXP s_rank;
    UNPACK_REAL_MATRIX(s_points, points, n, d);
    
    R_len_t nsorted = 0;
    R_len_t ntosort = INTEGER(s_tosort)[0];

    unsigned char *S = (unsigned char *)Calloc(n*n, unsigned char);
    unsigned int *N = (unsigned int *)Calloc(n, unsigned int);

    /* Allocate result vector: */
    PROTECT(s_rank = allocVector(INTSXP, n));
    int *rank = INTEGER(s_rank);

    /* Check to make sure we exit while() loop further down even if
     * ntosort is missspecified. */
    if (ntosort > n)
	ntosort = n;

    /* Initialize array counting the number of individuals that
     * dominate the i-th individual. */
    for (i = 0; i < n; ++i)
	N[i] = 0;

    for (i = 0; i < n; ++i) {
	for (j = i+1; j < n; ++j) {
	    int dom = dominates(points, i, j, n, d);
	    if (dom < 0) { /* j dominates i */
		S[n * i + j] = 0;
		S[n * j + i] = 1;
		++N[i];
	    } else if (dom > 0) { /* i dominates j */
		S[n * i + j] = 1;
		S[n * j + i] = 0;
		++N[j];
	    } else { /* neither dominates the other */
		S[n * i + j] = 0;
		S[n * j + i] = 0;
	    }
	}
    }

    /* Assign initial ranks: */
    for (i = 0; i < n; ++i) {
	if (0 == N[i]) { /* Member of first front */
	    rank[i] = 1;
	    ++nsorted;
	} else { /* Not yet decide what front i belongs to */
	    rank[i] = -1;
	}
    }

    /* Assign remaining ranks: */
    int r = 1;
    while (nsorted < ntosort) {
	for (i = 0; i < n; ++i) {
	    if (r != rank[i])  /* Skip all not in current rank */
		continue;      
	    for (j = 0; j < n; ++j) {
		if (1 == S[n * i + j]) { /* j in S_i */
		    --N[j];
		    if (0 == N[j]) { /* N_j == 0 -> assign rank */
			rank[j] = r + 1;
			++nsorted;
		    }
		}
	    }
	}
	++r;
    }
    Free(S);
    Free(N);
    UNPROTECT(1); /* s_rank */
    return(s_rank);
}
