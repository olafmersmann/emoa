/*
 * r_ind.c - R[123] indicator helpers.
 *
 * Author:
 *   Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 *
 * Based on source code by Joshua Knowles (c) 2008.
 */

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "sexp_macros.h"

#define NORMALIZED(x, min, max) ((x - min)/(max - min))

/* 
 * Helper functions:
 */
static R_INLINE int ipow(const int x, const int exp) {
    int val = 1;
    for (int i = 0; i < exp; ++i)
        val *= x;
    return(val);
}

/*
 * Utility functions:
 */
static double weighted_sum_utility(const double *wv, const double *x, 
			    const double *ideal, const double *nadir,
			    const size_t nobjs) {
    int i;
    double total = 0.0;
    
    for (i=0; i<nobjs; i++) 
        total += wv[i] * NORMALIZED(x[i], ideal[i], nadir[i]);
    
    return (1.0 - total);
}

static double tchebycheff_utility(const double *wv, const double *x, 
			   const double *ideal, const double *nadir,
			   const size_t nobjs) {
    int i;
    double val, mymax = 0.0;
    
    for (i=0; i<nobjs; i++) {
        val = wv[i] * NORMALIZED(x[i], ideal[i], nadir[i]);
        if (val > mymax)
            mymax = val;
    }
    return (1.0 - mymax);
}

static double augmented_tchebycheff_utility(const double *wv, const double *x, 
				     const double *ideal, const double *nadir, 
				     const size_t nobjs,
				     const double rho) {
    double tu, wu;
    tu = tchebycheff_utility(wv, x, ideal, nadir, nobjs);
    wu = weighted_sum_utility(wv, x, ideal, nadir, nobjs);
    return(tu + rho*wu);
}


static void int2kary(int x, const int basek, const int digits, int *kary) {
    int i;
    int val;
    if (x >= ipow(basek,digits)) {
        printf("Number in int2kary() too large. Exiting.\n");
        exit(-1);
    }
    val = digits-1;

    for (i=0; i<digits; i++)
        kary[i]=0;

    i=0;
    while (x) {
        if (x >= ipow(basek,val)) {
            kary[i] += 1;
            x -= ipow(basek,val);
        } else {
            val -= 1;
            i++;
        }
    }
}

/* 
 * create_weight_vectors - sample from all possible weight vectors
 */
static double *create_weight_vectors(const int s, const int k, int *pnwv) {
    int c=0;
    size_t nwv = (int) choose(s+k-1, k-1);
    double *wv = (double *)R_alloc(nwv*k, sizeof(double));
    int *count = (int *)R_alloc(k, sizeof(int));

    int i=0;
    while (i < ipow(s+1,k)) {
        int sum=0;
        int2kary(i,s+1,k,count);	
        for (int j = 0; j < k; ++j) 
            sum += count[j];
        if (sum == s) {
            for (int j = 0; j < k; ++j)
                wv[c*k + j] = (double)count[j] / (double)s;
            ++c;
	}
        i++;
    }
    *pnwv = nwv;
    return(wv);
}

/*
 * do_r_ind - R interface routine
 */
SEXP do_r_ind(SEXP s_data, 
	      SEXP s_ideal, SEXP s_nadir, 
	      SEXP s_lambda, SEXP s_method) {
    SEXP s_res;
    
    /* Unpack arguments */
    UNPACK_REAL_MATRIX(s_data, data, k_data, n_data); /* Matrix is in column major order! */
    UNPACK_REAL_VECTOR(s_ideal, ideal, n_ideal);
    UNPACK_REAL_VECTOR(s_nadir, nadir, n_nadir);
    UNPACK_INT(s_lambda, lambda);
    UNPACK_INT(s_method, method);

    const int nobjs = k_data;

    if (k_data != n_ideal)
	error("Ideal and current front must have the same dimension.");

    if (k_data != n_nadir)
	error("Nadir and current front must have the same dimension.");

    /* Generate weight vectors */
    int nwv;
    double *wv = create_weight_vectors(lambda, k_data, &nwv);

    /* Allocate result */
    PROTECT(s_res = allocVector(REALSXP, nwv));
    double *res = REAL(s_res);

    /* Calculate r criterion */
    for(int i = 0; i < nwv; ++i) { 
	double maxval = -DBL_MAX;
	for (int j = 0; j < n_data; ++j) {
	    const double *cwv = wv + nobjs * i;
	    const double *cdata = data + nobjs * j;
	    double val;
	    switch (method) {
	    case 1:
                val = weighted_sum_utility(cwv, cdata, ideal, nadir, nobjs);
		break;
	    case 2:
                val = tchebycheff_utility(cwv, cdata, ideal, nadir, nobjs);
		break;
	    default:
                val = augmented_tchebycheff_utility(cwv, data, ideal, nadir, nobjs, 0.01);
		break;
	    }
	    if (val > maxval) 
		res[i]  = maxval = val;
	}
    }
    UNPROTECT(1); /* s_res */
    return s_res;
}
