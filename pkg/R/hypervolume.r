##
## hypervolume.r - Functions for calculating the dominated hypervolume
##
## Authors:
##   Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
##

##
## dominated_hypervolume - wrapper around the various C routines
##
## Currently only the code by Fonseca et.al. is included.
##
dominated_hypervolume <- function(x, ref, algorithm) {
  ## Possibly infer reference point:
  if (missing(ref))
    ref <- apply(x, 1, max)
  ## Pick an algorithm:
  if (missing(algorithm))
    algorithm <- "fonseca"

  ## Sanity checks:
  if (!is.matrix(x))
    stop("Pareto front must be a matrix")
  if (nrow(x) != length(ref))
    stop("Reference point and front must have the same dimension.")

  if (algorithm == "fonseca") {
    .Call("do_fonseca_hv", x, ref, PACKAGE="emoa")
  } else {
    stop("Unsupported algorithm '", algorithm, "'.")
  }
}

##
## hypervolume_contribution - calculate hv contribution for a front
##
hypervolume_contribution <- function(x, ref) {
  ## Possibly infer reference point:
  if (missing(ref))
    ref <- apply(x, 1, max) + 1

  ## Sanity checks:
  if (!is.matrix(x))
    stop("Pareto front must be a matrix")
  if (nrow(x) != length(ref))
    stop("Reference point and front must have the same dimension.")

  ## Call C code:
  .Call("do_hv_contrib", x, ref, PACKAGE="emoa")
}
