\name{dominatedHypervolume}
\Rdversion{1.1}
\alias{dominatedHypervolume}
\title{
  Dominated Hypervolume calculation
}
\description{
  Calculates the exact dominated hypervolume of the points given in
  \code{x} subject to the reference point \code{ref}.
}
\usage{
dominatedHypervolume(x, ref, algorithm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Points}
  \item{ref}{Reference point for hypervolume calculation}
  \item{algorithm}{Algorithm to use}
}
\details{
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
  \code{\link{nonDominatedPoints}} to extract the pareto optimal points
  for a given set of points.
}
\examples{
}
\keyword{optimize}
