\name{nondominated_points}
\Rdversion{1.1}
\alias{nondominated_points}
\alias{nondominated_ordering}
\alias{nds_rank}
\alias{is_dominated}
\alias{\%dominates\%}
\title{
  Utilitiy functions for Pareto dominance
}
\description{
 Functions dealing with pareto dominance.
}
\usage{
  nondominated_points(points)
  is_dominated(points)
  nds_rank(points, partial)
  nondominated_ordering(points, partial)
}
\arguments{
  \item{points}{Matrix containing one point per column}
  \item{partial}{Integer specifying the number of elements to be in
    order. This allows partial ordering to save time.}
}
\details{
  \code{nondominated_ordering} has been deprecated in favor of
  \code{nds_rank}.
}
\value{
  For \code{nondominated_points} and \code{is_dominated} a logical
  vector containing true if the point is not dominated or dominated respectivly.

  For \code{nondominated_ordering} and \code{nds_rank} a vector giving
  the rank of the front to which the respective individual belongs.
}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\keyword{optimize}
