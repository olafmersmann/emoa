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
    \item{\code{algo}}{Abbreviated name of algorithm}
    \item{\code{fun}}{Name of benchmark function}
    \item{\code{d}}{Dimension of objective space}
    \item{\code{n}}{Number of function evaluations}
    \item{\code{best}}{Largest dominated hypervolume}
    \item{\code{median}}{Median dominated hypervolume}
    \item{\code{worst}}{lSmallest dominated hypervolume}
    \item{\code{mean}}{average dominated hypervolume}
    \item{\code{std}}{standard deviation of dominated hypervolume indicator}
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
  print(dotplot(algo ~ median | fun, cec2007, groups=cec2007$n))
  }
}
\keyword{datasets}
