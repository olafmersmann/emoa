#include <R.h>
#include "utilities.h"

double clip_to_limits(const double x, const double l, const double u) {
    if (x < l) {
	return(l);
    } else if (x > u) {
	return(u);
    } else {
	return(x);
    }
}

int flip_coin() {
    return(.5 < unif_rand() ? HEADS : TAILS);
}

