\name{cec2009}
\Rdversion{1.1}
\alias{cec2009}
\alias{UF1}
\alias{UF2}
\alias{UF3}
\alias{UF4}
\alias{UF5}
\alias{UF6}
\alias{UF7}
\alias{UF8}
\alias{UF9}
\alias{UF10}
\title{
  Unconstrained CEC 2009 test functions
}
\description{
  Unconstrained test functions from the CEC 2009 competition of
  evolutionary multiobjective optimization algorithms.
}
\usage{
UF1(x)
UF2(x)
UF3(x)
UF4(x)
UF5(x)
UF6(x)
UF7(x)
UF8(x)
UF9(x)
UF10(x)
}
\arguments{
  \item{x}{Parameter vector}
}
\details{
  These functions are not vectorized. For a simple way to evaluate
  several parameter combinations stored in a matrix see the example
  section below.
}
\value{
Value of the test function in \code{x}. For \code{UF1} to \code{UF7}
this is a two dimensional vector, for \code{UF8}, \code{UF9} and
\code{UF10} it is a three dimensional vector.
}
\references{
}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\examples{
  ## Number of samples:
  ## n <- 10000
  ## Sample parameter space:
  ## par <- matrix(runif(3*n, c(0, -1, -1), c(1, 1, 1)), ncol=3, byrow=TRUE)
  ## Evaluate function:
  ## value <- t(apply(par, 1, UF1))
  ## find non dominated solutions:
  ## d <- nonDominatedPoints(value)

  ## Plot objective space:
  ## opar <- par(mfrow=c(1,2))
  ## plot(value,
  ##     col=ifelse(d, "red", "blue"),
  ##     pch=ifelse(d, 19, 20),
  ##     main="Objective Space")
  ## plot(value[d,], main="Pareto Front")
  ## par(opar)
}
\keyword{optimize}
