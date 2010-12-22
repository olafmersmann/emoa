library("emoa")
source("sms_ga.R")

##' Leading ones / trailing zeros test function
##'
##' @param x Binary vector.
##'
##' @return The number of leading ones and trailing zeros in the input
##'   vector \code{x}.
##' @export
lotz <- function(x) {
  x <- round(x)
  n <- length(x)  
  if (any(is.na(x))) {
    fz <- 0
    lo <- 0
  } else {
    fz <- Position(function(i) i == 0, x, nomatch=n+1)
    lo <- Position(function(i) i == 1, x, right=TRUE, nomatch=0)
  }
  -c(fz - 1, n - lo)
}

res <- sms_ga(lotz, 50L, control=list(maxeval=80000L,
                           mu=51L * 2L,
                           crossover=single_point_binary_crossover,
                           mutate=ubm_operator(1/10),
                           logger=emoa_console_logger(every=500)))
plot(t(res$value))
m <- res$par
print(unique(sort(colSums(m))))
