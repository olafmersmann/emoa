##
## test-front_edge.R - Edge point tests
##

context("front_edge")

## Predeclare some simple test fronts:
front_2d <- matrix(c(0.0, 1.0, 0.5,
                     1.0, 0.0, 0.5),
                   nrow=2, byrow=TRUE)

front_3d <- matrix(c(0.0, 1.0, 0.0, 0.5, 0.7, 1.0, 1.0, 0.5,
                     0.0, 0.0, 1.0, 0.5, 0.5, 1.0, 0.5, 1.0,
                     1.0, 0.0, 0.0, 0.5, 0.3, 0.5, 1.0, 1.0),
                   nrow=3, byrow=TRUE)
which_points_on_edge(front_3d)

test_that("which_points_on_edge_2d", {
  expect_equal(which_points_on_edge(front_2d), c(1, 2))
})

test_that("which_points_on_edge_3d", {
  expect_equal(which_points_on_edge(front_3d), c(1, 2, 3, 6, 7, 8))
})
