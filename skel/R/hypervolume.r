##
## hypervolume.r - Functions for calculating the dominated hypervolume
##
## Authors:
##   Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
##

##' Dominated Hypervolume calculation
##' 
##' \code{dominated_hypervolume} calculates the dominated hypervolume of
##' the points in \code{points}. 
##' 
##' \code{hypervolume_contribution} calculates the hypervolume
##' contribution of each point.
##' 
##' If no reference point \code{ref} is given, one is automatically
##' calculated by determening the maximum in each coordinate.
##'  
##' Currently only one general algorithm is implemented due to Fonseca
##' et.al. but work is underway to include others such as the Beume &
##' Rudolph approach as well as the approach by Bradstreet et.al.
##'  
##' The 1D and 2D cases are handle seperately by efficient algorithms.
##' Calculates the exact dominated hypervolume of the points given in
##' \code{x} subject to the reference point \code{ref}.
##' 
##' @param points Matrix containing the points one per column.
##' @param ref Optional reference point. If not provided the maximum
##'   in each dimension is used.
##' @param algorithm Optional argument to choose the algorithm used for
##'   the calculation. Currently only the code by Fonseca et.al. is
##'   supported.
##' @return For \code{dominated_hypervolume} the dominated hypervolume
##'   by the points in \code{points} with respect to the reference point
##'   \code{ref}. For \code{hypervolume_contribution} a vector giving
##'   the hypervolume soley dominated by that point.
##' 
##' @seealso \code{\link{nondominated_points}} to extract the pareto
##'   optimal points for a given set of points and
##'   \code{\link{nds_hv_selection}} for a selection strategy based
##'   on the hypervolume contribution of each point.
##' 
##' @author Olaf Mersmann \email{olafm@@statistik.tu-dortmund.de}
##' @export
##' @keywords optimize
dominated_hypervolume <- function(points, ref, algorithm) {
  ## Possibly infer reference point:
  if (missing(ref))
    ref <- apply(points, 1, max)
  ## Pick an algorithm:
  if (missing(algorithm))
    algorithm <- "fonseca"

  ## Sanity checks:
  if (!is.matrix(points))
    stop("Pareto front must be a matrix")
  if (nrow(points) != length(ref))
    stop("Reference point and front must have the same dimension.")

  if (algorithm == "fonseca") {
    .Call("do_fonseca_hv", points, ref, PACKAGE="emoa")
  } else {
    stop("Unsupported algorithm '", algorithm, "'.")
  }
}

##' @export
##' @rdname dominated_hypervolume
hypervolume_contribution <- function(points, ref) {
  ## Possibly infer reference point:
  if (missing(ref))
    ref <- apply(points, 1, max) + 1

  ## Sanity checks:
  if (!is.matrix(points))
    stop("Pareto front must be a matrix")
  if (nrow(points) != length(ref))
    stop("Reference point and front must have the same dimension.")

  ## Call C code:
  .Call("do_hv_contrib", points, ref, PACKAGE="emoa")
}
