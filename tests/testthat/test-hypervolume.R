##
## test-hypervolume.r - Hypervolume tests
##

context("hypervolume")

## Predeclare some simple test fronts:
simpleFront <- matrix(c(0.0, 1.0, 0.5,
                        1.0, 0.0, 0.5),
                      ncol=3, byrow=TRUE)

infFront <- simpleFront
infFront[,2] <- Inf

naFront <- simpleFront
naFront[,2] <- NA

nanFront <- simpleFront
nanFront[,2] <- NaN

test_that("dominated_hypervolume", {
  expect_equal(dominated_hypervolume(simpleFront), 0.5^2)
  expect_equal(dominated_hypervolume(simpleFront, c(1.0, 1.0)), 0.5^2)
  ## OME: FIXME should ignore outer points?
  ## expect_equal(dominated_hypervolume(simpleFront, c(0.8, 0.8)), 0.3^2)
  expect_equal(dominated_hypervolume(simpleFront, c(2.0, 2.0)), 3.25)
  expect_equal(dominated_hypervolume(simpleFront, c(0.0, 0.0)), 0)
  expect_equal(dominated_hypervolume(simpleFront, c(0.5, 0.5)), 0)

  suppressWarnings(expect_equal(dominated_hypervolume(simpleFront, c(NaN, NaN)),
                                NaN))
  expect_warning(dominated_hypervolume(simpleFront, c(NaN, NaN)))
})

test_that("bad_front", {
  ## OME: Should probably also return NaN instead of NA
  suppressWarnings(expect_equal(is.na(dominated_hypervolume(naFront)), TRUE))
  suppressWarnings(expect_equal(dominated_hypervolume(nanFront), NaN))

  expect_warning(is.na(dominated_hypervolume(naFront)))
  expect_warning(dominated_hypervolume(nanFront))
})

test_that("inf_front", {
  ## OME: These should probably be reworked to return 'saner' values.
  suppressWarnings(expect_equal(dominated_hypervolume(infFront), NaN))
  expect_warning(dominated_hypervolume(infFront))

  suppressWarnings(expect_equal(dominated_hypervolume(infFront, ref=c(1, 1)), NaN))
  expect_warning(dominated_hypervolume(infFront, ref=c(1, 1)))

  suppressWarnings(expect_equal(dominated_hypervolume(simpleFront, ref=c(Inf, Inf)), NaN))
  expect_warning(dominated_hypervolume(simpleFront, ref=c(Inf, Inf)))
})

test.badInput <- function() {
  expect_error(dominated_hypervolume(1:10))
  expect_error(dominated_hypervolume("abc"))
  expect_error(dominated_hypervolume(simpleFront, 1:3))
  expect_error(dominated_hypervolume(simpleFront, 1))
}

test.dhv3d <- function() {
  ## Built with Lego. Hurra!
  m3 <- structure(c(5, 2, 0, 4, 3, 0, 4, 2, 1, 4, 1, 2, 3, 2, 2, 2, 3,
                    2, 4, 0, 3, 3, 1, 3, 2, 2, 3, 3, 0, 4, 2, 1, 4, 1,
                    2, 4, 0, 3, 4, 0, 2, 5), .Dim = c(3L, 14L))
  m3_dhv <- c(1, 1,
              1,
              2, 1, 1,
              2, 1, 1,
              2, 2, 1, 1,
              1)
  expect_equal(hypervolume_contribution(m3, c(6, 4, 6)), m3_dhv)
}

## Regression test for the permutation bug (missing duplicate()).
test.regress_perm <- function() {
  ## Built with Lego. Hurra!
  m3 <- structure(c(5, 2, 0, 4, 3, 0, 4, 2, 1, 4, 1, 2, 3, 2, 2, 2, 3,
                    2, 4, 0, 3, 3, 1, 3, 2, 2, 3, 3, 0, 4, 2, 1, 4, 1,
                    2, 4, 0, 3, 4, 0, 2, 5), .Dim = c(3L, 14L))
  o3 <- structure(c(5, 2, 0, 4, 3, 0, 4, 2, 1, 4, 1, 2, 3, 2, 2, 2, 3,
                    2, 4, 0, 3, 3, 1, 3, 2, 2, 3, 3, 0, 4, 2, 1, 4, 1,
                      2, 4, 0, 3, 4, 0, 2, 5), .Dim = c(3L, 14L))
  hypervolume_contribution(m3)
  expect_equal(m3, o3)
}
