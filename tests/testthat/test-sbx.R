##
## test-sbx.r - SBX crossover
##
## These checks may fail sometimes! They are simply empirical checks of the
## probability of crossover.
##
context("sbx")

N <- 10000L
f <- sbx_operator(2, 0.8, -2, 2)
parents <- matrix(c(0, 1), ncol=2)
x <- replicate(N, c(f(parents)))[1,]

test_that("sbxP1", {
  p <- mean(x != 0)
  expect_true(p > 0.78 && p < 0.82)
})

test_that("sbxInBounds", {
  expect_true(all(x >= -2))
  expect_true(all(x <= 2))
})
