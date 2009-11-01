##
## domination.R - Anything to do with Pareto dominance
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

`%dominates%` <- function(x, y) {
  #stopifnot(is.vector(x))
  #stopifnot(is.vector(y))
  
  n1 <- sum(x < y)
  n2 <- sum(y > x)
  n1 > 0 & n2 == 0
}

is_dominated <- function(points) {
  #stopifnot(is.matrix(points))
  #stopifnot(is.numeric(points))
  .Call("is_dominated", points, PACKAGE="emoa")
}

nondominated_points <- function(points)
  points[,!is_dominated(points)]

nds_rank <- function(points, partial) {
  #stopifnot(is.matrix(points))
  #stopifnot(is.numeric(points))
  
  if (missing(partial))
    partial <- ncol(points)
  else if (is.numeric(partial))
    partial <- as.integer(partial)
  else
    stopifnot(is.integer(partial))
  
  .Call("nondominated_order", points, partial, PACKAGE="emoa")
}

nondominated_ordering <- function(points, partial) {
  .Deprecated("nds_rank")
  nds_rank(par, partial)
}
