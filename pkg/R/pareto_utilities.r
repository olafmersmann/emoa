##
## pareto_utilities.r - Operators relating to pareto optimality
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

normalizePoints <- function(front, minval, maxval) {
  if (missing(minval))
    minval <- apply(front, 2, min)
  if (missing(maxval))
    maxval <- apply(front, 2, max)
  ## FIXME: This is ugly!
  t((t(front) - minval)/(maxval - minval))
}

nonDominatedPoints <- function(par) {
  stopifnot(is.matrix(par))
  
  .Call("nondominated_points", par)
}

nonDominatedOrdering <- function(par, partial) {
  stopifnot(is.matrix(par))
  stopifnot(is.numeric(par))
  
  if (missing(partial))
    partial <- nrow(par)
  else if (is.numeric(partial))
    partial <- as.integer(partial)
  else
    stopifnot(is.integer(partial))
  
  .Call("nondominated_order", par, partial)
}

epsilonIndicator <- function(x, o) {
  stopifnot(is.matrix(x))
  stopifnot(is.matrix(o))

  stopifnot(is.numeric(x))
  stopifnot(is.numeric(o))
  
  if (any(x < 0) || any(o < 0))
    stop("Epsilon Indicator only works for fronts which are strictly positive.")
  
  .Call("do_eps_ind", x, o)
}
