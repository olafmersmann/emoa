/*
 * r_ind.c - R[123] indicator helpers.
 *
 * This file includes both helper functions to calculate R indicator
 * values from R as well as a utility to precompute common weight
 * vector sets to speed up the calculation. To generate the weight vectors, compile this file using
 *
 *  cc -std=c99 -DGENERATE_WV_HEADER -o gen_header r_ind.c
 *
 * and run 
 *
 *  ./gen_header > weight_vectors.h
 *
 * afterwards.
 *
 * Author:
 *   Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 *
 * Based on source code by Joshua Knowles (c) 2008.
 */

#ifdef GENERATE_WV_HEADER
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#define error(...) exit(-1)
#else
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "sexp_macros.h"
#include "weight_vectors.h"
#endif

#define NORMALIZED(x, min, max) ((x - min)/(max - min))

/* 
 * Helper functions:
 */
static int ipow(const int x, const int exp) {
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
    
    for (i = 0; i < nobjs; ++i)
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
        error("Number in int2kary() too large. Exiting.");
        return;
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

#ifdef GENERATE_WV_HEADER
size_t choose(int r, int k) {
    double result = 1;
    int lower = r, upper = r - k + 1;
    if (lower > upper) { int tmp = lower; lower = upper; upper = tmp; }
    for (int i = lower; i <= upper; ++i)
        result *= i;
    size_t val = (result / tgamma(k+1));
    return val;
}
#endif

/* 
 * create_weight_vectors - sample from all possible weight vectors
 */
static double *create_weight_vectors(const int s, const int k, unsigned int *pnwv) {
    int c = 0, i = 0;
    size_t nwv = (int) choose(s + k - 1, k - 1);
    double *wv = (double *)malloc(nwv*k * sizeof(double));
    int *count = (int *)malloc(k * sizeof(int));
    
    while (i < ipow(s + 1, k)) {
        int sum=0;
        int2kary(i, s + 1, k, count);
        for (int j = 0; j < k; ++j)
            sum += count[j];
        if (sum == s) {
            for (int j = 0; j < k; ++j)
                wv[c*k + j] = (double)count[j] / (double)s;
            ++c;
	}
        ++i;
    }
    *pnwv = nwv;
    free(count);
    return(wv);
}

#ifdef GENERATE_WV_HEADER

void dump_weights(int lambda, int objectives) {
    size_t current;
    int nwv;    
    double *weight_vectors = create_weight_vectors(lambda, objectives, &nwv);
    
    printf("unsigned int number_of_weights_%i_%i = %i;\n", lambda, objectives, nwv);
    printf("double weights_%i_%i[] = {\n", lambda, objectives);
    for (current = 0; current < nwv * objectives; ++current) {
        printf("%f, ", weight_vectors[current]);
    }
    printf("};\n\n");
    free(weight_vectors);
}

int main(int argc, char **argv) {
    printf("#ifndef WEIGHT_VECTORS_H\n\n");
#define DO_PRECOMPUTED_WEIGHT_VECTOR(L, D) dump_weights(L, D);
#include "precomputed_weight_vectors.h"
    printf("\n#endif /* WEIGHT_VECTORS_H */\n");
    return 0;
}

#else

/*
 * do_r_ind - R interface routine
 */
SEXP do_r_ind(SEXP s_data, 
	      SEXP s_ideal, SEXP s_nadir, 
	      SEXP s_lambda, SEXP s_method) {
    int must_free_weight_vectors = FALSE;
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
    unsigned int nwv;
    double *wv = NULL;

    /* Compute weights: */
#define DO_PRECOMPUTED_WEIGHT_VECTOR(L, D)                            \
    if (lambda == L && k_data == D) {                                 \
        nwv = number_of_weights_ ## L ## _ ## D;                      \
        wv = weights_ ## L ## _ ## D;                                 \
    } else 

    #include "precomputed_weight_vectors.h" 
    {
        must_free_weight_vectors = TRUE;
        wv = create_weight_vectors(lambda, k_data, &nwv);
    }

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
                val = augmented_tchebycheff_utility(cwv, cdata, ideal, nadir, nobjs,
                                                    0.01);
		break;
	    }
	    if (val > maxval) 
		res[i]  = maxval = val;
	}
    }

    if (must_free_weight_vectors)
        free(wv);

    UNPROTECT(1); /* s_res */
    return s_res;
}

#endif
