##
## runit-pu.r - Pareto utility test
##

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

test.nonDominatedPoints <- function() {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    m <- points[p,o]
    res <- nonDominatedPoints(m)
    checkEquals(res, nd[o])
  }  
}

test.nonDominatedPoints.args <- function() {
  checkException(nonDominatedPoints("a"))
  checkException(nonDominatedPoints(list(1, 2, 3)))
  checkException(nonDominatedPoints(data.frame(x=1:10)))
}

test.nonDominatedOrdering <- function() {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    checkEquals(nonDominatedOrdering(points[p,o]), no[o])
  }  
}

test.nonDominatedOrdering.args <- function() {
  checkException(nonDominatedOrdering("a"))
  checkException(nonDominatedOrdering(1))
  checkException(nonDominatedOrdering(list(1, 2, 3)))
  checkException(nonDominatedOrdering(data.frame(x=1:10)))
  checkException(nonDominatedOrdering(points, partial="a"))
}
