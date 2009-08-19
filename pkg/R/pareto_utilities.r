##
## pareto_utilities.r - Operators relating to pareto optimality
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

normalize_points <- function(front, minval, maxval) {
  if (missing(minval))
    minval <- apply(front, 1, min)
  if (missing(maxval))
    maxval <- apply(front, 1, max)
  ## FIXME: This is ugly!
  (front - minval)/(maxval - minval)
}

nondominated_points <- function(par) {
  stopifnot(is.matrix(par))
  stopifnot(is.numeric(par))
  
  .Call("nondominated_points", par)
}

nondominated_ordering <- function(par, partial) {
  stopifnot(is.matrix(par))
  stopifnot(is.numeric(par))
  
  if (missing(partial))
    partial <- ncol(par)
  else if (is.numeric(partial))
    partial <- as.integer(partial)
  else
    stopifnot(is.integer(partial))
  
  .Call("nondominated_order", par, partial)
}

epsilon_indicator <- function(x, o) {
  stopifnot(is.matrix(x))
  stopifnot(is.matrix(o))

  stopifnot(is.numeric(x))
  stopifnot(is.numeric(o))
  
  if (any(x < 0) || any(o < 0))
    stop("Epsilon Indicator only works for fronts which are strictly positive.")
  
  .Call("do_eps_ind", x, o)
}
