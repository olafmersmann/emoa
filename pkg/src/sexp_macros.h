/*
 * sexp_macros.h - helper macros for SEXPs
 *
 * Collection of useful macros to handle S expressions. Most of these
 * are used to unpack arguments passed in via the .Call() or
 * .External() interface.
 *
 * Author:
 *   Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
 */

#if !defined(__SEXP_MACROS_H__)
#define __SEXP_MACROS_H__

#include <R.h>
#include <Rinternals.h>

/*
 * Unpack a real matrix stored in SEXP S. 
 */
#define UNPACK_REAL_MATRIX(S, D, N, K)          \
  double *D = REAL(S);                          \
  const R_len_t N = nrows(S);                   \
  const R_len_t K = ncols(S);

/*
 * Unpack a real vector stored in SEXP S.
 */
#define UNPACK_REAL_VECTOR(S, D, N)             \
  double *D = REAL(S);                          \
  const R_len_t N = length(S);                   

#endif
