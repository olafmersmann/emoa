/*
 * pareto_utilities.c - utility functions relating to pareto dominance
 *
 * Most of these functions could just as well have been implemented in
 * R but were deemed performance critical enough to warrant rewriting
 * them in C.
 *
 * NOTICE: These functions expect the points to be passed in in column
 * major order. That means, we expect each column of the matrix to be
 * one point and each row to be one coordinate. This may seem counter
 * intuitive from an R standpoint but makes all C code more cache
 * efficient.
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

#include "bitstring.h"

/*
 * dominates 
 * 
 * Expects points to be a nxd matrix in _column_ major format. This is
 * the default format used by R.
 *
 * Returns:
 *    -1  iff points[,i] dominates points[,i]
 *     0  iff points[,i] and points[,j] are incomparable
 *     1  iff points[,j] dominates points[,i]
 */
static R_INLINE int dominates(double *p, R_len_t i, R_len_t j, R_len_t nobj) {
    int i_flagged = 0;
    int j_flagged = 0;
    R_len_t k;
    double *pi = p + i*nobj;
    double *pj = p + j*nobj;
    for (k = 0; k < nobj; ++k) {
	const double p_ik = pi[k];
	const double p_jk = pj[k];
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
    UNPACK_REAL_MATRIX(s_points, points, d, n); /* Note column layout */
    
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
		    int dom = dominates(points, i, j, d);
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
    UNPACK_REAL_MATRIX(s_points, points, d, n); /* Note column layout */
    
    R_len_t nsorted = 0;
    R_len_t ntosort = INTEGER(s_tosort)[0];

    /* Use compact bitstring for speed and ease of managment instead
     * of a dynamicly sized array of arrays or linkes lists.
     */
    bitstring_t *S = (bitstring_t *)Calloc(n, bitstring_t);
    unsigned int *N = (unsigned int *)Calloc(n, unsigned int);

    /* Allocate result vector: */
    PROTECT(s_rank = allocVector(INTSXP, n));
    int *rank = INTEGER(s_rank);

    /* Check to make sure we exit while() loop further down even if
     * ntosort is missspecified. 
     */
    if (ntosort > n)
	ntosort = n;

    /* Initialize bitstrings and array counting the number of
     * individuals that dominate the i-th individual.
     */
    for (i = 0; i < n; ++i) {
	bitstring_initialize(&S[i], n);
	N[i] = 0;
    }
    for (i = 0; i < n; ++i) {
	for (j = i+1; j < n; ++j) {
	    int dom = dominates(points, i, j, d);
	    if (dom < 0) { /* j dominates i */
		bitstring_set(S[i], j);
		bitstring_clear(S[j], i);
		++N[i];
	    } else if (dom > 0) { /* i dominates j */
		bitstring_clear(S[i], j);
		bitstring_set(S[j], i);
		++N[j];
	    } else { /* neither dominates the other */
		bitstring_set(S[i], j);
		bitstring_set(S[j], i);
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
	R_CheckUserInterrupt();
	for (i = 0; i < n; ++i) {
	    if (r != rank[i])  /* Skip all not in current rank */
		continue;
	    for (j = 0; j < n; ++j) {
		if (bitstring_is_clear(S[i], j)) { /* j in S_i */
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
    
    /* Free bitstrings and arrays */
    for (i = 0; i < n; ++i)
	bitstring_delete(S[i]);
    Free(S);
    Free(N);
    UNPROTECT(1); /* s_rank */
    return(s_rank);
}
