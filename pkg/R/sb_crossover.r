##
## sb_crossover - Simulated Binary Crossover
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

##
## sbx_operator - return a simulated binary crossover operator with
##   the given parameters.
##
sbx_operator <- function(n, p, lower, upper) {
  ## Force arguments:
  force(n); force(p); force(lower); force(upper);

  crossover <- function(x)
    .Call("do_sbx", x, lower, upper, n, p)
  return(crossover)
}
