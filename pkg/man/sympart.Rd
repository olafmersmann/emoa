\name{sympart}
\Rdversion{1.1}
\alias{sympart}
\title{
  SYM-PART test function
}
\description{
  The SYM-PART test function from the CEC 2007 EMOA competition.
}
\usage{
sympart(x)
}
\arguments{
  \item{x}{Parameter vector}
}
\details{
  The \code{sympart} function is only defined for even sized dimensions
  of \code{x}. For this particular instance of the test function all
  components of \code{x} should be between -20 and 20.
}
\value{
  A 2D vector containing the value of the SYM-PART function.
}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\note{
  This function is not vectorized. For a simple way to apply it to a
  matrix see \code{\link{apply}}.
}
\seealso{
  \link{cec2009} for the unconstrained test functions from the CEC 2009 competition.
}
\keyword{optimize}
