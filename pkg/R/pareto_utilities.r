##
## pareto_utilities.r - Operators relating to pareto optimality
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

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
