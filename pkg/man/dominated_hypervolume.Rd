\name{dominated_hypervolume}
\Rdversion{1.1}
\alias{dominated_hypervolume}
\alias{hypervolume_contribution}
\title{
  Dominated Hypervolume calculation
}
\description{
  Calculates the exact dominated hypervolume of the points given in
  \code{x} subject to the reference point \code{ref}.
}
\usage{
  dominated_hypervolume(x, ref, algorithm)
  hypervolume_contribution(x, ref)
}
\arguments{
  \item{x}{Matrix of points one per column}
  \item{ref}{Reference point for hypervolume calculation}
  \item{algorithm}{Algorithm to use}
}
\details{
  \code{dominated_hypervolume} calculates the dominated hypervolume of
  the points in \code{x}. 

  \code{hypervolume_contribution} calculates the hypervolume
  contribution of each point.

  If no reference point \code{ref} is given, one is automatically
  calculated by determening the maximum in each coordinate.
  
  Currently only one general algorithm is implemented due to Fonseca
  et.al. but work is underway to include others such as the Beume &
  Rudolph approach as well as the approach by Bradstreet et.al.
  
  The 1D and 2D cases are handle seperately by efficient algorithms.
}
\value{
  The dominated hypervolume by the set of points given in \code{x} with
  respect to the reference point given in \code{ref}
}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\seealso{
  \code{\link{nondominated_points}} to extract the pareto optimal points
  for a given set of points.
}
\keyword{optimize}
