##
## runit-hypervolume.r - Hypervolume tests
##

## Predeclare some simple test fronts:
simpleFront <- matrix(c(0.0, 1.0, 0.5, 0.5, 1.0, 0.0), ncol=2, byrow=TRUE)
infFront <- matrix(c(0.0, Inf, 0.5, 0.5, Inf, 0.0), ncol=2, byrow=TRUE)
naFront <- matrix(c(0.0, 1.0, NA, NA, 1.0, 0.0), ncol=2, byrow=TRUE)
nanFront <- matrix(c(0.0, 1.0, NaN, NaN, 1.0, 0.0), ncol=2, byrow=TRUE)

test.dominatedHypervolume <- function() {
  checkEqualsNumeric(dominatedHypervolume(simpleFront), 0.5^2)
  checkEqualsNumeric(dominatedHypervolume(simpleFront, c(1.0, 1.0)), 0.5^2)
  ## OME: FIXME should ignore outer points?
  ## checkEqualsNumeric(dominatedHypervolume(simpleFront, c(0.8, 0.8)), 0.3^2)
  checkEqualsNumeric(dominatedHypervolume(simpleFront, c(2.0, 2.0)), 3.25)
  checkEqualsNumeric(dominatedHypervolume(simpleFront, c(0.0, 0.0)), -0.75)
  checkEqualsNumeric(dominatedHypervolume(simpleFront, c(0.5, 0.5)), -0.50)
  checkEqualsNumeric(dominatedHypervolume(simpleFront, c(NaN, NaN)), NaN)
}

test.badFront <- function() {
  ## OME: Should probably also return NaN instead of NA
  checkEquals(is.na(dominatedHypervolume(naFront)), TRUE)
  checkEquals(dominatedHypervolume(nanFront), NaN)
}

test.infFront <- function() {
  ## OME: These should probably be reworked to return 'saner' values.
  checkEquals(dominatedHypervolume(infFront), NaN)
  checkEquals(dominatedHypervolume(infFront, ref=c(1, 1)), NaN)
  checkEquals(dominatedHypervolume(simpleFront, ref=c(Inf, Inf)), Inf)
}

test.badInput <- function() {
  checkException(dominatedHypervolume(1:10))
  checkException(dominatedHypervolume("abc"))
  checkException(dominatedHypervolume(simpleFront, 1:3))
  checkException(dominatedHypervolume(simpleFront, 1))
}
