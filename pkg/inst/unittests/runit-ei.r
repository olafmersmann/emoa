##
## runit-ei.r - Epsilon Indicator tests
##

x <- matrix(c(1.0, 0.0,
              0.5, 0.5,
              0.0, 1.0),
            nrow=3, byrow=TRUE)

runit.epsilonIndicator <- function() {
  for (delta in seq(0, 1, by=0.2)) {
    checkEqualsNumeric(epsilonIndicator(x, x + delta), -delta)
    checkEqualsNumeric(epsilonIndicator(x + delta, x), delta)
  }
  ## Negative values:
  checkException(epsilonIndicator(x, x-10))
  checkException(epsilonIndicator(x-10, x))
}
