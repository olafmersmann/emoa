\name{cec2007}
\Rdversion{1.1}
\alias{cec2007}
\docType{data}
\title{CEC 2007 multiobjective optimization competition results}
\description{  
  This data set contains the hypervolume indicator results of 8
  different algorithms on the CEC 2007 multiobjective optimization
  benchmark problemset.  
}
\usage{data(cec2007)}
\format{
  A data frame with 456 observations on the following 9 variables.
  \describe{
    \item{\code{algo}}{abbreviated name of algorithm}
    \item{\code{fun}}{name of benchmark function}
    \item{\code{d}}{dimension of objective space}
    \item{\code{n}}{number of function evaluations}
    \item{\code{metric}}{name of quality metric}
    \item{\code{best}}{largest value of metric}
    \item{\code{median}}{median value of metric}
    \item{\code{worst}}{smallest value of metric}
    \item{\code{mean}}{average value of metric}
    \item{\code{std}}{standard deviation of metric}
  }
}
\source{
  \url{
    http://web.mysites.ntu.edu.sg/epnsugan/PublicSite/Shared\%20Documents/CEC2007-final-pdfs.zip
  }
}
\examples{
  \dontrun{
  data(cec2007)
  require(lattice)
  print(dotplot(algo ~ median | fun + metric, cec2007, groups=cec2007$n))
  }
}
\keyword{datasets}
