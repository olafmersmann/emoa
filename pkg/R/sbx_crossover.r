##
## sbx_crossover - Simulated Binary Crossover
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

sbx_crossover <- function(n, p, lower, upper) {
  crossover <- function(x1, x2) {
    if (runif(1) < pc) {
      n <- length(x1)
      beta <- rsbxbeta(n, nc)
      z1 <- 0.5*((1+beta)*x1 + (1-beta)*x2)
      z2 <- 0.5*((1+beta)*x1 + (1+beta)*x2)
      return(list(inbounds(z1, lower, upper),
                  inbounds(z2, lower, upper)))
    } else {
      ## Assume x1/x2 are in bounds for efficiency reasons
      return(list(x1, x2))
    }
  }
  ## Force arguments:
  nc <- n
  pc <- p
  force(lower); force(upper)
  rm(n, p)
  ## Return crossover operator:
  return(crossover)
}
