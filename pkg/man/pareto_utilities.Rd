\name{nondominated_points}
\Rdversion{1.1}
\alias{nondominated_points}
\alias{nondominated_ordering}
\title{
  Utilitiy functions for Pareto dominance
}
\description{
 Functions dealing with pareto dominance.
}
\usage{
  nondominated_points(par)
  nondominated_ordering(par, partial)
}
\arguments{
  \item{par}{Matrix containing one point per row}
  \item{partial}{Integer specifying the number of elements to be in
    order. This allows partial ordering to save time.}
}
\value{
  For \code{nondominated_points} a matrix containing those points which
  are non dominated (pareto optimal in the set \code{par}).

  For \code{nondominated_ordering} a vector giving the rank of the front
  to which the respective individual belongs.
}
