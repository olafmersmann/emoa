##
## sympart.r - SYM-PART test function from the CEC 2007 competition
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

sympart <- function(x) {
  stopifnot(length(x) >= 2)
  stopifnot(length(x) %% 2 == 0)
  .Call("do_sympart", as.numeric(x))
}
