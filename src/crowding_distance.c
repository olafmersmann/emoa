#include "extern.h"

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include "sexp_macros.h"

static R_INLINE void do_merge(double *data, int *index, int *work, R_len_t low,
                              R_len_t mid, R_len_t high, R_len_t d,
                              R_len_t offs) {
  int i, j, k;
  i = low;
  j = mid + 1;
  k = low;
  while (i <= mid && j <= high) {
    if (data[d * index[i] + offs] < data[d * index[j] + offs]) {
      work[k++] = index[i++];
    } else {
      work[k++] = index[j++];
    }
  }
  while (i <= mid)
    work[k++] = index[i++];

  while (j <= high)
    work[k++] = index[j++];

  for (i = low; i <= high; ++i)
    index[i] = work[i];
}

static void do_index_mergesort(double *data, int *index, int *work, R_len_t low,
                               R_len_t high, R_len_t d, R_len_t i) {
  if (low < high) {
    R_len_t mid = (low + high) / 2;
    do_index_mergesort(data, index, work, low, mid, d, i);
    do_index_mergesort(data, index, work, mid + 1, high, d, i);
    do_merge(data, index, work, low, mid, high, d, i);
  }
}

static R_INLINE void index_mergesort(double *data, int *index, int *work,
                                     R_len_t n, R_len_t d, R_len_t i) {
  R_len_t k;
  for (k = 0; k < n; ++k)
    index[k] = k;

  do_index_mergesort(data, index, work, 0, n - 1, d, i);
}

SEXP do_crowding_distance(SEXP s_front) {
  SEXP s_res;
  int i, j;
  UNPACK_REAL_MATRIX(s_front, front, d, n);

  /* Work arrays: */
  int *index = Calloc(n, int);
  int *work = Calloc(n, int);

  /* Result: */
  PROTECT(s_res = allocVector(REALSXP, n));
  double *res = REAL(s_res);
  for (i = 0; i < n; ++i)
    res[i] = 0.0;

  /* Main loop. Calculate distance for each dimension: */
  for (i = 0; i < d; ++i) {
    index_mergesort(front, index, work, n, d, i);
    res[index[0]] = R_PosInf;
    res[index[n - 1]] = R_PosInf;
    for (j = 1; j < (n - 1); ++j) {
      res[index[j]] +=
          front[index[j + 1] * d + i] - front[index[j - 1] * d + i];
    }
  }

  Free(work);
  Free(index);
  UNPROTECT(1); /* s_res */
  return (s_res);
}
