\name{epsilonIndicator}
\Rdversion{1.1}
\alias{epsilonIndicator}
\title{
  Epsilon Indicator calculation
}
\description{
  Calculates the epsilon indicator.
}
\usage{
  epsilonIndicator(x, o)
}
\arguments{
  \item{x}{Set of points for which to calculate the criterion}
  \item{o}{Other set of points}
}
\details{
  Calculates the epsilon indicator value of the set of points given in
  \code{x} with respect to the set given in \code{o}.
}
\value{
  Value of the epsilon indicator.
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
