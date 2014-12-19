##
## test-id.r - Pareto utility test
##
context("id")

points <- matrix(c(1.0, 0.0, 0.0,
                   0.0, 1.0, 0.0,
                   0.0, 0.0, 1.0,
                   0.5, 0.5, 0.5,
                   0.5, 0.6, 0.6,
                   0.6, 0.5, 0.6,
                   0.6, 0.6, 0.5,
                   0.8, 0.8, 0.8),
                 ncol=8, byrow=FALSE)

nd <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
no <- c(1, 1, 1, 1, 2, 2, 2, 3)

test_that("hv_indicators", {
  p1 <- points[, no==1]
  p2 <- points[, no==2]

  I12 <- hypervolume_indicator(p1, p2, ref=c(1, 1, 1))
  I21 <- hypervolume_indicator(p2, p1, ref=c(1, 1, 1))
  expect_equal(I21, 0.013)
  expect_equal(I12, -I21)

  I21p <- hypervolume_indicator(p2, p1, ref=c(10, 10, 10))
  expect_true(I21 < I21p)
})

test_that("r_indicators", {
  p1 <- points[, no==1]
  p2 <- points[, no==2]

  ## Basic sanity:
  expect_equal(r1_indicator(p1, p1), 0.5)
  expect_equal(r2_indicator(p1, p1), 0)
  expect_equal(r3_indicator(p1, p1), 0)
  expect_equal(r1_indicator(p2, p2), 0.5)
  expect_equal(r2_indicator(p2, p2), 0)
  expect_equal(r3_indicator(p2, p2), 0)

  ## Precalculate values:
  r112 <- r1_indicator(p1, p2)
  r121 <- r1_indicator(p2, p1)

  r212 <- r2_indicator(p1, p2)
  r221 <- r2_indicator(p2, p1)

  r312 <- r3_indicator(p1, p2)
  r321 <- r3_indicator(p2, p1)

  ## Symmetry properties:
  expect_equal(r112 + r121, 1)
  expect_equal(r212, -r221)

  ## Known 'better':
  expect_true(r112 > r121)
  expect_true(r212 < r221)
  expect_true(r312 < r321)
})

test_that("eps_indicator", {
  p1 <- points[, no==1]
  p2 <- points[, no==2]

  expect_equal(epsilon_indicator(p1, p2), 0)
  expect_equal(epsilon_indicator(p2, p1), 0.6)
})
