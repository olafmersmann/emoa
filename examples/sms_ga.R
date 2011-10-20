##
## sms_ga.r - Discrete SMS-EMOA variant
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##
library("emoa")

##' Single point binary crossover
##'
##' @param x Two column matrix. Each column is one individual.
##'
##' @return A two column matrix. Each column is a new individual
##'   obtained using the crossover.
##' @export
single_point_binary_crossover <- function(x) {
  n <- nrow(x)
  p <- sample(1:n, 1)
  which <- 1:p
  tmp <- x[which, 1]
  x[which, 1] <- x[which, 2]
  x[which, 2] <- tmp
  x  
}

##' Uniform binary crossover operator
##'
##' @param p Probability of crossover.
##'
##' @return A function implementing the uniform binary crossover
##'   operator with crossover probability \code{p}.
##' @export
ubx_operator <- function(p) {
  crossover <- function(x) {
    n <- nrow(x)
    which <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(p, 1-p))
    tmp <- x[which, 1]
    x[which, 1] <- x[which, 2]
    x[which, 2] <- tmp
    x
  }
}

##' Uniform binary crossover operator
##'
##' @param p Probability of mutation.
##'
##' @return A function implementing binary mutation.
##' @export
ubm_operator <- function(p) {
  mutate <- function(x) {
    n <- length(x)
    which <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(p, 1-p))
    x[which] <- !x[which]
    x
  }  
}

##' S-Metric selection genetic algorithm.
##'
##' Genetic algorithm for discrete multiobjective optimization
##' problems.
##'
##' @param f Binary function to optimize (f:{0, 1}^n -> R^m).
##' @param nvars Number of binary variables over which f is defined.
##' @param ... Passed on to \code{f}.
##' @param control List of control parameters.
##' @return An \code{emoa_result} object.
##' @export
##' @author Olaf Mersmann \email{olafm@@statistik.tu-dortmund.de}
sms_ga <- function(f, nvars, ...,
                   initial_population,
                   control=list(mu=100L,
                     crossover=bx_operator(0.75),
                     mutate=ubm_operator(0.05)
                     )) {
  ## Extract control parameters:
  default <- formals(sys.function())$control
  control <- steady_state_emoa_control(f, lower=rep(0L, nvars), upper=rep(1L, nvars),
                                       ...,
                                       control=control, default=default)
  control$crossover <- eval(emoa:::coalesce(control[["crossover"]], default$crossover))
  control$mutate <- eval(emoa:::coalesce(control[["mutate"]], default$mutate))
  
  ## Tracking variables:
  X <- matrix(0L, nrow=control$n, ncol=control$maxeval)
  Y <- matrix(0, nrow=control$d, ncol=control$maxeval)
  dob <- rep(-1L, control$maxeval)
  eol <- rep(-1L, control$maxeval)
  
  ## Random inital population:
  X[, 1:control$mu] <- if (!missing(initial_population)) {
    if (nrow(initial_population) != nvars)
      stop("Initial population must consist of individuals with " + nvars + " parameters.")
    if (ncol(initial_population) != control$mu)
      stop("Initial population must contain " + control$mu + " individuals")
    if (!all(initial_population %in% c(0, 1)))
      stop("Initial population can only contain values of 0 or 1.")
    initial_population
  } else {
    replicate(control$mu, sample(0:1, nvars, replace=TRUE))
  }
  Y[, 1:control$mu] <- sapply(1:control$mu, function(i) f(X[,i]))
  control$ref <- apply(Y[, 1:control$mu], 1, max)

  neval <- control$mu       ## Count the number of function evaluations
  active <- 1:control$mu    ## Indices of individuals that are in the current pop.

  ## Save some common control parameters into the current
  ## environment. This saves a few msec of execution time...
  crossover <- control$crossover
  mutate <- control$mutate
  maxeval <- control$maxeval
  logger <- control$logger

  logger$start("sms_ga")
  while(neval < maxeval) {
    ############################################################
    ## Variation:
    parents <- sample(active, 2)
    child <- crossover(X[, parents])[,sample(c(1, 2), 1)]
    x <- mutate(child)

    ## Add new individual:
    neval <- neval + 1
    X[, neval] <- x
    Y[, neval] <- f(x)
    dob[neval] <- neval ## For a steady state emoa this is trivial...
    active <- c(active, neval)

    ############################################################
    ## Selection:
    i <- nds_hv_selection(Y[, active])

    ## Remove the i-th active individual:
    eol[active[i]] <- neval
    active <- active[-i]

    ############################################################
    ## Logging:    
    logger$step()
  }
  logger$stop()
  
  res <- structure(list(X=X, Y=Y,
                        dob=dob,
                        eol=eol,
                        par=X[,active], value=Y[,active]),
                   class="emoa_result")
}
