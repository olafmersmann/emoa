##
## cec2009.r - Test functions from the CEC2009 competition
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

## Unconstrained test functions:
##
## These actually do have constraints, but they are box constraints.
## The dimension is deduced from the length of 'x'.
UF1 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF1", as.numeric(x))
}

UF2 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF2", as.numeric(x))
}

UF3 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF3", as.numeric(x))
}

UF4 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF4", as.numeric(x))
}

UF5 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF5", as.numeric(x))
}

UF6 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF6", as.numeric(x))
}

UF7 <- function(x) {
  stopifnot(length(x) >= 3)
  .Call("do_UF7", as.numeric(x))
}

UF8 <- function(x) {
  stopifnot(length(x) >= 5)
  .Call("do_UF8", as.numeric(x))
}

UF9 <- function(x) {
  stopifnot(length(x) >= 5)
  .Call("do_UF9", as.numeric(x))
}

UF10 <- function(x) {
  stopifnot(length(x) >= 5)
  .Call("do_UF10", as.numeric(x))
}
