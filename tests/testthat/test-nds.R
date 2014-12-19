##
## test-nds.r - Pareto dominance stuff
##

context("nds")

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

test_that("is_dominated", {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    m <- points[p,o]
    res <- is_dominated(m)
    expect_equal(res, !nd[o])
  }
})

test_that("is_maximally_dominated", {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    m <- points[p,o]
    res <- is_maximally_dominated(m)
    expect_equal(res, max(no[o]) == no[o])
  }
})

test_that("nds_rank", {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    expect_equal(nds_rank(points[p,o]), no[o])
  }
})

test_that("nds_rank.args", {
  expect_error(nds_rank("a"))
  expect_error(nds_rank(1))
  expect_error(nds_rank(list(1, 2, 3)))
  expect_error(nds_rank(data.frame(x=1:10)))
  expect_error(nds_rank(points, partial="a"))
})

## Bug fixed i
test_that("single_nds", {
  expect_equal(dim(nondominated_points(points[,-(1:3)])), c(3, 1))
})

##test_dominates_op <- function() {
##  x <- c(1, 2, 1)
##  y <- c(2, 1, 2)
##  z <- c(0, 1, 0)
##  expect_equal(x %dominates% y, FALSE)
##  expect_equal(y %dominates% x, FALSE)
##  expect_equal(z %dominates% x, FALSE)
##  expect_equal(z %dominates% y, FALSE)
##}
