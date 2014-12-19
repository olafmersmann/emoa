##
## test-ei.r - Epsilon Indicator tests
##

context("epsilon_indicator")

x <- matrix(c(1.0, 0.5, 0.0,
              0.0, 0.5, 1.0),
            ncol=3, byrow=TRUE)

test_that("Permutations", {
  ## Check for different permutations of the rows and columns of
  ## points.
  k <- nrow(x)
  n <- ncol(x)
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    m <- x[p, o]
    for (delta in seq(0, 1, by=0.2)) {
      expect_equal(epsilon_indicator(m, m + delta), -delta)
      expect_equal(epsilon_indicator(m + delta, m), delta)
    }
  }
})

## Check different sized matrices:
test_that("different sized matrices", {
  expect_equal(epsilon_indicator(x, x[,-2] + 0.2), -0.2)
})

## Negative values:
test_that("negative values", {
  expect_error(epsilon_indicator(x, x-10))
  expect_error(epsilon_indicator(x-10, x))
})
