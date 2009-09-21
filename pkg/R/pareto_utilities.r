##
## pareto_utilities.r - Operators relating to pareto optimality
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

normalize_points <- function(front, minval, maxval) {
  if (missing(minval))
    minval <- apply(front, 1, min)
  if (missing(maxval))
    maxval <- apply(front, 1, max)
  ## FIXME: This is ugly!
  (front - minval)/(maxval - minval)
}

nondominated_points <- function(par) {
  stopifnot(is.matrix(par))
  stopifnot(is.numeric(par))
  
  .Call("nondominated_points", par)
}

nondominated_ordering <- function(par, partial) {
  stopifnot(is.matrix(par))
  stopifnot(is.numeric(par))
  
  if (missing(partial))
    partial <- ncol(par)
  else if (is.numeric(partial))
    partial <- as.integer(partial)
  else
    stopifnot(is.integer(partial))
  
  .Call("nondominated_order", par, partial)
}

##
## Epsilon Indicator:
##
epsilon_indicator <- function(x, o) {
  stopifnot(is.matrix(x))
  stopifnot(is.matrix(o))

  stopifnot(is.numeric(x))
  stopifnot(is.numeric(o))
  
  if (any(x < 0) || any(o < 0))
    stop("The epsilon indicator is only defined for strictly positive objective values.")
  
  .Call("do_eps_ind", x, o)
}

##
## R Indicators:
###
r_indicator <- function(x, ref, ideal, nadir, lambda, utility, summary) {
  ## (OME): Order of utility functions is important. It translates
  ## into the method number in the C code!
  utility.functions <- c("weighted sum", "Tchebycheff", "Augmented Tchebycheff")
  utility <- match.arg(utility, utility.functions)
  method <- which(utility == utility.functions)
  
  if (missing(ideal)) 
    ideal <- pmin(apply(x, 1, min), apply(ref, 1, min))
  if (missing(nadir))
    nadir <- pmax(apply(x, 1, max), apply(ref, 1, max))

  dim <- nrow(x)
  if (missing(lambda)) {
    lambda <- if (dim == 2) { 500 }
         else if (dim == 3) { 30  }
         else if (dim == 4) { 12  }
         else if (dim == 5) { 8   }
         else               { 3   }
  }
  
  ix <- .Call("do_r_ind", x, ideal, nadir, as.integer(lambda), as.integer(method))
  ir <- .Call("do_r_ind", ref, ideal, nadir, as.integer(lambda), as.integer(method))
  
  return(summary(ix, ir))
}

r1_indicator <- function(x, ref, ideal, nadir, lambda, utility="Tchebycheff")
  r_indicator(x, ref, ideal, nadir, lambda, utility, function(ua, ur) mean(ua > ur) + mean(ua == ur)/2)

r2_indicator <- function(x, ref, ideal, nadir, lambda, utility="Tchebycheff") 
    r_indicator(x, ref, ideal, nadir, lambda, utility, function(ua, ur) mean(ur - ua))

r3_indicator <- function(x, ref, ideal, nadir, lambda, utility="Tchebycheff") 
    r_indicator(x, ref, ideal, nadir, lambda, utility, function(ua, ur) mean((ur - ua)/ur))
