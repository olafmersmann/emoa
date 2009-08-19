##
## poly_mutation.r - Polynomial mutation operator
##
## Author
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

poly_mutation <- function(n, p, lower, upper) {
  mutation <- function(x) {
    if (runif(1) <= pm) {
      d1 <- (x - l)/delta
      d2 <- (u - x)/delta
      r <- runif(length(x))
      dq <- ifelse(r < .5,
                   (2*r + (1-2*r)*(1-d1)^(nm+1))^e - 1,
                   1 - (2*(1-r) + 2*(r-.5)*(1-d2)^(nm+1))^e)
      z <- x + (u-l)*dq
      return(inbounds(z, l, u))
    } else {
      return(x)
    }
  }
  ## Force arguments:
  nm <- n
  pm <- p
  l <- lower
  u <- upper
  delta <- (u - l)
  e <- 1/(nm+1)
  rm(n, p, lower, upper)
  ## Return mutation operator:
  return(mutation)
}
