\name{nonDominatedPoints}
\Rdversion{1.1}
\alias{nonDominatedPoints}
\alias{nonDominatedOrdering}
\title{
  Utilitiy functions for Pareto dominance
}
\description{
 Functions dealing with pareto dominance.
}
\usage{
  nonDominatedPoints(par)
  nonDominatedOrdering(par, partial)
}
\arguments{
  \item{par}{Matrix containing one point per row}
  \item{partial}{Integer specifying the number of elements to be in
    order. This allows partial ordering to save time.}
}
\value{
  For \code{nonDominatedPoints} a matrix containing those points which
  are non dominated (pareto optimal in the set \code{par}).

  For \code{nonDominatedOrdering} a vector giving the rank of the front
  to which the respective individual belongs.
}