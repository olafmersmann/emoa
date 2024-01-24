##
## cec2009 unit test
##
context("cec2009_functions")

## Source to generate test point checks:
##
## f <- function(n, x=c(0.2, 0.2, 0.2, 0.2)) {
##   fn <- get(n)
##   r <- fn(x)
##   message("expect_equal(", n, "(",
##           "c(", paste(x, collapse=", "), "), ",
##           "c(", paste(r, collapse=", "), ")))")
## }
##
## for (n in paste("UF", 1:10, sep=""))
##   f(n)

test_that("UF1", {
  expect_equal(UF1(c(0.2, 0.2, 0.2, 0.2)), c(0.454091055737032, 1.72127910133303))
  expect_equal(UF1(c(1.0, 0.0, 0.0)), c(1.0, 1.5))
  expect_equal(UF1(c(1.0, 2.0, 2.0)), c(NaN, NaN))
  expect_equal(UF1(c(-1.0, 0.0, 0.0)), c(NaN, NaN))
#  expect_equal(UF1(c(0.2, 0.2, "a")), c(NaN, NaN))
  expect_error(UF1(c(1.0, 2.0)))
})

test_that("UF2", {
  expect_equal(UF2(c(0.2, 0.2, 0.2, 0.2)), c(0.210335975924134, 0.654710703252405))
  expect_equal(UF2(c(1.0, 1.0, 1.0)), c(8.2199999999999989, 0.7449042731880116))
#  expect_equal(UF2(c(1.0, 2.0, 2.0)), c(NaN, NaN))
#  expect_equal(UF2(c(-1.0, 0.0, 0.0)), c(NaN, NaN))
#  expect_equal(UF2(c(0.2, 0.2, "a")), c(NaN, NaN))
  expect_error(UF2(c(1.0, 2.0)))
})

test_that("UF3", {
  expect_equal(UF3(c(0.2, 0.2, 0.2, 0.2)), c(7.1937361619286, 2.90716200450771))
  expect_equal(UF3(c(1.0, 2.0, 2.0)), c(NaN, NaN))
  expect_equal(UF3(c(-1.0, 0.0, 0.0)), c(NaN, NaN))
#  expect_equal(UF3(c(0.2, 0.2, "a")), c(NaN, NaN))
  expect_error(UF3(c(1.0, 2.0)))
})

test_that("UF4", {
  expect_equal(UF4(c(0.2, 0.2, 0.2, 0.2)), c(0.434509085633295, 1.20063945204195))
  expect_equal(UF4(c(1.0, 2.0, 2.0)), c(NaN, NaN))
  expect_equal(UF4(c(-1.0, 0.0, 0.0)), c(NaN, NaN))
  #expect_equal(UF4(c(0.2, 0.2, "a")), c(NaN, NaN))
  expect_error(UF4(c(1.0, 2.0)))
})

test_that("UF5", {
  expect_equal(UF5(c(0.2, 0.2, 0.2, 0.2)), c(3.17056357285719, 3.98342430089429))
})

test_that("UF6", {
  expect_equal(UF6(c(0.2, 0.2, 0.2, 0.2)), c(1.88938265125039, 6.65748253841339))
})

test_that("UF7", {
  expect_equal(UF7(c(0.2, 0.2, 0.2, 0.2)), c(0.978870719414727, 1.44371303315529))
})

test_that("UF8", {
  expect_equal(UF8(c(2.0, 0.2, 0.2, 0.2, 0.2)), c(NaN, NaN, NaN))
  expect_equal(UF8(c(0.2, 2.0, 0.2, 0.2, 0.2)), c(NaN, NaN, NaN))
  expect_equal(UF8(c(0.2, 0.2, 3.0, 0.2, 0.2)), c(NaN, NaN, NaN))
  #expect_equal(UF8(c(0.2, 0.2, 0.2, 0.2, "a")), c(NaN, NaN, NaN))
  for (i in 1:4)
    expect_error(UF8(rep(0.2, i)))
})

test_that("UF9", {
  expect_equal(UF9(c(2.0, 0.2, 0.2, 0.2, 0.2)), c(NaN, NaN, NaN))
  expect_equal(UF9(c(0.2, 2.0, 0.2, 0.2, 0.2)), c(NaN, NaN, NaN))
  expect_equal(UF9(c(0.2, 0.2, 3.0, 0.2, 0.2)), c(NaN, NaN, NaN))
  #expect_equal(UF9(c(0.2, 0.2, 0.2, 0.2, "a")), c(NaN, NaN, NaN))
  for (i in 1:4)
    expect_error(UF9(rep(0.2, i)))
})

test_that("UF10", {
  expect_equal(UF10(c(2.0, 0.2, 0.2, 0.2, 0.2)), c(NaN, NaN, NaN))
  expect_equal(UF10(c(0.2, 2.0, 0.2, 0.2, 0.2)), c(NaN, NaN, NaN))
  expect_equal(UF10(c(0.2, 0.2, 3.0, 0.2, 0.2)), c(NaN, NaN, NaN))
  #expect_equal(UF10(c(0.2, 0.2, 0.2, 0.2, "a")), c(NaN, NaN, NaN))
  for (i in 1:4)
    expect_error(UF10(rep(0.2, i)))
})
