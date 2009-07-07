##
## hypervolume.r - Functions for calculating the dominated hypervolume
##
## Authors:
##   Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
##

##
## These functions generate pareto fronts for testing:
##
convexFront <- function(n, d) {
}

##
## dominatedHypervolume - wrapper around the various C routines
##
## Currently only the code by Fonseca et.al. is included.
##
dominatedHypervolume <- function(x, ref, algorithm) {
  ## Possibly infer reference point:
  if (missing(ref))
    ref <- apply(x, 2, max)
  ## Pick an algorithm:
  if (missing(algorithm))
    algorithm <- "fonseca"

  ## Sanity checks:
  if (!is.matrix(x))
    stop("Pareto front must be a matrix")
  if (ncol(x) != length(ref))
    stop("Reference point and front must have the same dimension.")

  if (algorithm == "fonseca") {
    ## Note the transopse. do_fonseca_hv() needs the front in row major format.
    .Call("do_fonseca_hv", t(x), ref)
  } else {
    stop("Unsupported algorithm '", algorithm, "'.")
  }
}
