##
## control.R - Preliminary control parameter framework
##
## Author: Olaf Mersmann <olafm@statistik.tu-dortmund.de>
##

##' Basic EMOA control parameters.
##' 
##' Recognized control parameters:
##' \describe{
##'   \item{logger}{\code{emoa_logger} object used to log events.}
##'   \item{n}{Number of parameters, defaults to the length of the longer
##'     of \code{upper} or \code{lower}.}
##'   \item{d}{Number of dimensions.}
##' }
##' 
##' @export
emoa_control <- function(f, upper, lower, ..., control, default) {
  control$logger <- coalesce(control[["logger"]], default[["logger"]], emoa_console_logger())
  control$n <- as.integer(coalesce(control[["n"]],
                                   default[["n"]],
                                   max(length(lower), length(upper))))
  control$d <- as.integer(coalesce(control[["d"]],
                                   default[["d"]],
                                   length(f(rep(NA, control$n), ...))))
  control
}

##' Steady state EMOA control parameters.
##' 
##' Recognized control parameters:
##' \describe{
##'   \item{mu}{Size of population.}
##'   \item{maxeval}{Maximal number of function evaluations.}
##' }
##'
##' @export
steady_state_emoa_control <- function(f, upper, lower, ..., control, default=list()) {
  control <- emoa_control(f, upper, lower, ..., control=control, default=default)
  control$mu <- as.integer(coalesce(control[["mu"]],
                                    default[["mu"]],
                                    100L))
  control$maxeval <- as.integer(coalesce(control[["maxeval"]],
                                         default[["maxeval"]],
                                         control$mu * 300L))
  control
}

##' @export
sbx_control <- function(f, upper, lower, ..., control, default=list()) {
  if (!"crossover" %in% names(control)) {
    if (!"crossover" %in% names(default)) {
      control$sbx.n <- coalesce(control[["sbx.n"]], default[["sbx.n"]], 5)
      control$sbx.p <- coalesce(control[["sbx.p"]], default[["sbx.p"]], 1.0)
      control$crossover <- sbx_operator(control$sbx.n, control$sbx.p, lower, upper)
    } else {
      control$crossover <- default[["crossover"]]
    }
  }
  control
}

##' @export
pm_control <- function(f, upper, lower, ..., control, default=list()) {
  if (!"mutate" %in% names(control)) {
    if (!"mutate" %in% names(default)) {
      control$pm.n <- coalesce(control[["pm.n"]], default[["pm.n"]], 10)
      control$pm.p <- coalesce(control[["pm.p"]], default[["pm.p"]], .2)
      control$mutate <- pm_operator(control$pm.n, control$pm.p, lower, upper)
    } else {
      control$mutate <- default[["mutate"]]
    }
  }
  control
}
