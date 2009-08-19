##
## utilities.r - Internal utility functions
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

## rsbxbeta - Draw n samples from the SBX 'beta' distribution with
##   parameter nc
rsbxbeta <- function(n, nc) {
    ## U ~ [0, 1] , twou := 2 * U ~ [0, 2]
    twou <- runif(n, 0, 2.0)

    e <- 1/(nc + 1)
    beta <- ifelse(twou < 1, twou, 1/(2 - twou))^e
    return(beta)
}

## inbounds - ensure x is in the bounds given by l and u
inbounds <- function(x, l, u) {
  ifelse(x < l, l, ifelse(x > u, u, x))
}

## coalesce - return first non null argument.
coalesce <- function(...) {
  l <- list(...)
  isnull <- sapply(l, is.null)
  l[[which.min(isnull)]]
}
