##
## test-poly.r - Polynomial mutation
##
## These checks may fail sometimes! They are simply empirical checks of the
## probability of crossover.
##
context("poly")

N <- 10000L
f <- pm_operator(5, 0.8, -10, 10)
x <- replicate(N, f(5))

test_that("polyP1", {
  p <- mean(x != 5)
  expect_true(p > 0.78 && p < 0.82)
})

test_that("polyP2", {
  p <- mean(x < 5)
  expect_true(p > 0.38 && p < 0.42)
})

test_that("polyInBounds", {
  expect_true(all(x >= -10))
  expect_true(all(x <= 10))
})
