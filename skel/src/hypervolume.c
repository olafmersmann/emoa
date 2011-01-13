/**
 * hypervolume.c - Interface to hypervolume calculation code.
 *
 * Authors:
 *  Olaf Mersmann    <olafm@statistik.tu-dortmund.de>
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <float.h>

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "sexp_macros.h"

#include "hv.h"

void calc_hv_contrib_2d(const double *data, const double *ref, double *res, 
			const size_t n, const size_t k) {
    size_t i,j,l;
    double mindistplus;
    
    for (i = 0; i < n; ++i) {
        double dvol = 1.0;
        for (j = 0; j < k; ++j) {
            const double fij = data[k*i + j];
            mindistplus = DBL_MAX; 
            for (l = 0; l < n; ++l) {
                if (l != i) {
		    const double flj = data[k*l + j];
                    const double delta = flj - fij;
                    if (delta >= 0.0 && delta < mindistplus)
                        mindistplus = delta;
                }
            }
            dvol *= mindistplus;
        }
	res[i] = dvol;
    }
}

void calc_hv_contrib_anyd(double *data, const double *ref, double *res, const size_t n, size_t k) {
    const double tothv = fpli_hv(data, k, n, ref);
    Rprintf("total hv: %f\n", tothv);
    for (int i = 0; i < n; ++i) {
	const double ihv = fpli_hv(data + k, k, n-1, ref);
        Rprintf("ihv: %f\n", ihv);
	res[i] = tothv - ihv;
	/* Swap ith and last row in data matrix: */
	if (i != (n - 1)) {
	    for (int j = 0; j < k; ++j) {
		double tmp = data[k * (i+1) + j];
		data[k * (i+1) + j] = data[j];
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
  
    UNPACK_REAL_MATRIX(s_data, data, k_data, n_data);
    UNPACK_REAL_VECTOR(s_ref, ref, n_ref);

    if (n_ref != k_data)
	error("ref and data must have the same dimension.");
    
    /* Allocate result */
    PROTECT(s_res = allocVector(REALSXP, n_data));
    double *res = REAL(s_res);

    switch (k_data) {
    case 2:
	calc_hv_contrib_2d(data, ref, res, n_data, k_data);
	break;
    default:
	calc_hv_contrib_anyd(data, ref, res, n_data, k_data);
	break;
    }
    UNPROTECT(1); /* s_res */
    return s_res;
}
