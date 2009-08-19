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

test.nondominated_points <- function() {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    m <- points[p,o]
    res <- nondominated_points(m)
    checkEquals(res, nd[o])
  }  
}

test.nondominated_points.args <- function() {
  checkException(nondominated_points("a"))
  checkException(nondominated_points(list(1, 2, 3)))
  checkException(nondominated_points(data.frame(x=1:10)))
}

test.nondominated_ordering <- function() {
  k <- nrow(points)
  n <- ncol(points)
  ## Check for different permutations of the rows and columns of
  ## points.
  for (i in 1:10) {
    o <- sample(1:n)
    p <- sample(1:k)
    checkEquals(nondominated_ordering(points[p,o]), no[o])
  }  
}

test.nondominated_ordering.args <- function() {
  checkException(nondominated_ordering("a"))
  checkException(nondominated_ordering(1))
  checkException(nondominated_ordering(list(1, 2, 3)))
  checkException(nondominated_ordering(data.frame(x=1:10)))
  checkException(nondominated_ordering(points, partial="a"))
}
