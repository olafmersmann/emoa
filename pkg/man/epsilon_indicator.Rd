\name{epsilon_indicator}
\Rdversion{1.1}
\alias{hypervolume_indicator}
\alias{epsilon_indicator}
\alias{r1_indicator}
\alias{r2_indicator}
\alias{r3_indicator}
\title{
  Quality indicator functions for Pareto front approximations.
}
\description{
  Calculates the hypervolume, epsilon, R1, R2 or R3 quality indicator value.
}
\usage{
  hypervolume_indicator(x, o, ref)
  epsilon_indicator(x, o)
  r1_indicator(x, o, ideal, nadir, lambda, utility = "Tchebycheff")
  r2_indicator(x, o, ideal, nadir, lambda, utility = "Tchebycheff")
  r3_indicator(x, o, ideal, nadir, lambda, utility = "Tchebycheff")
}
\arguments{
  \item{x}{Matrix of points for which to calculate the indicator value}
  \item{o}{Matrix of points of the reference set}
  \item{ref}{Reference point, if omitted, the nadir of the point sets is used.}
  \item{ideal}{Ideal point of true Pareto front. If omited the ideal of both point sets is used.}
  \item{nadir}{Nadir of the true Pareto front. If ommited the nadir of both point sets is used.}
  \item{lambda}{Number of weight vectors to use.}
  \item{utility}{Name of utility function.}
}
\details{
  Calculates the quality indicator value of the set of points given in
  \code{x} with respect to the set given in \code{o}. As with all
  functions in \code{emoa} that deal with sets of objective values
  these are stored by column.
}
\value{
  Value of the quality indicator.
}
\references{
  Zitzler, E., Thiele, L., Laumanns, M., Fonseca, C., and
  Grunert da Fonseca, V (2003): Performance Assessment of
  Multiobjective Optimizers: An Analysis and Review. IEEE
  Transactions on Evolutionary Computation, 7(2), 117-132.
}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\keyword{optimize}
