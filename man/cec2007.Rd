\name{cec2007}
\Rdversion{1.1}
\alias{cec2007}
\docType{data}
\title{CEC 2007 multiobjective optimization competition results}
\description{  
  This data set contains the hypervolume and R2 indicator results of the
  8 different algorithms that took part in the CEC 2007 multiobjective
  optimization benchmark.
}
\usage{data(cec2007)}
\format{
  A data frame with 456 observations of the following 9 variables.
  \describe{
    \item{\code{algo}}{Abbreviated name of algorithm}
    \item{\code{fun}}{Name of benchmark function}
    \item{\code{d}}{Dimension of objective space}
    \item{\code{n}}{Number of function evaluations}
    \item{\code{metric}}{Name of quality metric}
    \item{\code{pdef}}{Unique id for each combination of \code{fun},
      \code{d}, \code{n} and \code{metric}}
    \item{\code{best}}{Largest value of metric}
    \item{\code{median}}{Median value of metric}
    \item{\code{worst}}{Smallest value of metric}
    \item{\code{mean}}{Average value of metric}
    \item{\code{std}}{Standard deviation of metric}
  }
}
\source{
	Formerly available at http://web.mysites.ntu.edu.sg/epnsugan/PublicSite/Shared\%20Documents/CEC2007-final-pdfs.zip
}
\examples{
  \dontrun{
  data(cec2007)
  require(lattice)
  print(dotplot(algo ~ median | fun + metric, cec2007, groups=cec2007$n))
  }
}
\keyword{datasets}
