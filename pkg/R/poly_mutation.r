##
## poly_mutation.r - Polynomial mutation operator
##
## Author
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

##
## pm_operator - return a polynomial mutation operator with the given
##   parameters.
##
pm_operator <- function(n, p, lower, upper) {
  ## Force arguments:
  force(n); force(p); force(lower); force(upper);

  mutation <- function(x)
    .Call("do_pm", x, lower, upper, n, p, PACKAGE="emoa")
  return(mutation)
}
