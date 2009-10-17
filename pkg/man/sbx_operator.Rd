\name{sbx_operator}
\Rdversion{1.1}
\alias{sbx_operator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Simulated Binary Crossover operator
}
\description{
  Returns a simulated binary crossover operator with the given parameters.
}
\usage{
  sbx_operator(n, p, lower, upper)
}
\arguments{
  \item{n}{Distance parameter crossover distribution (eta)}
  \item{p}{Probability of mutation}
  \item{lower}{Lower bounds of parameter space}
  \item{upper}{Upper bounds of parameter space}
}
\value{
  Function with one parameter \code{x} which takes a matrix containing
  two sets of parameters and returns a matrix of two sets of
  parameters which resulted from the crossover operation. As with all
  \code{emoa} functions, the parameter sets are stored in the columns
  of \code{x}. \code{x} should therefore always have two columns and a
  warning will be given if it has more than two columns.
}
\seealso{\code{\link{pm_operator}}}
\author{
  Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}
}
\keyword{optimize}
