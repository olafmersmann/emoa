##
## runit-ei.r - Epsilon Indicator tests
##

x <- matrix(c(1.0, 0.5, 0.0,
              0.0, 0.5, 1.0),            
            ncol=3, byrow=TRUE)

runit.epsilonIndicator <- function() {
  k <- nrow(x)
  n <- ncol(x)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    m <- x[p,o]
    for (delta in seq(0, 1, by=0.2)) {
      checkEqualsNumeric(epsilonIndicator(m, m + delta), -delta)
      checkEqualsNumeric(epsilonIndicator(m + delta, m), delta)
    }
  }
  ## Check different sized matrices:
  checkEqualsNumeric(epsilonIndicator(x, x[,-2] + 0.2), -0.2)
  ## Negative values:
  checkException(epsilonIndicator(x, x-10))
  checkException(epsilonIndicator(x-10, x))
}
