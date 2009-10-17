\name{pm_operator}
\Rdversion{1.1}
\alias{pm_operator}
\title{
  Polynomial mutation operator
}
\description{
  Returns a polynomial mutation operator with the given parameters.
}
\usage{
pm_operator(n, p, lower, upper)
}
\arguments{
  \item{n}{Distance parameter mutation distribution (eta)}
  \item{p}{Probability of mutation}
  \item{lower}{Lower bounds of parameter space}
  \item{upper}{Upper bounds of parameter space}
}
\value{
  Function with one parameter \code{x} which takes a vector and returns the mutated version of \code{x}.
}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\seealso{\code{\link{sbx_operator}}}
\keyword{optimize}
