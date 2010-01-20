##
## sms_emoa.r - Simple straight forward SMS-EMOA implementation
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

require(emoa)

coalesce <- emoa:::coalesce

sms_emoa <- function(f, lower, upper, ...,
                     control=list(
                       n=NULL,
                       d=NULL,
                       mu=100L,
                       maxeval=NULL,
                       nc=15, pc=0.7,
                       nm=25, pm=0.3,
                       ref=c(11, 11),
                       trace=FALSE
                       )) {
  ## f:R^n -> R^d:
  n <- as.integer(coalesce(control[["n"]], max(length(lower), length(upper))))
  d <- as.integer(coalesce(control[["d"]], length(f(rep(NA, n)))))

  ## Population size:
  mu <- as.integer(coalesce(control[["mu"]], 100L))

  ## Stopping Criterion:
  maxeval <- as.integer(coalesce(control[["maxeval"]], mu*300L))
  
  ## Crossover parameters:
  nc <- coalesce(control[["nc"]], 5)
  pc <- coalesce(control[["pc"]], 1.0)
  crossover <- sbx_operator(nc, pc, lower, upper)

  ## Mutation parameters:
  nm <- coalesce(control[["nm"]], 10)
  pm <- coalesce(control[["pm"]], .2)
  mutation <- pm_operator(nm, pm, lower, upper)

  ## Tracing:
  trace <- coalesce(control[["trace"]], 100L)
  trace <- ifelse(trace == TRUE, 100L, as.integer(trace))
  ref <- coalesce(control[["ref"]], rep(11, d))
  
  ## Tracking variables:
  X <- matrix(0, nrow=n, ncol=maxeval)
  Y <- matrix(0, nrow=d, ncol=maxeval)
  eol <- rep(-1L, maxeval)
  
  ## Inital population:  
  X[, 1:mu] <- replicate(mu, runif(n, lower, upper))
  Y[, 1:mu] <- sapply(1:mu, function(i) f(X[,i]))

  neval <- mu       ## Count the number of function evaluations
  active <- 1:mu    ## Indices of individuals that are in the current pop.

  if (trace) message(sprintf("%8s %8s", "NEVAL", "HV"))

  while(neval < maxeval) {
    ############################################################
    ## Variation:
    parents <- sample(active, 2)
    ## child <- .Call("do_sbx", X[, parents], lower, upper, nc, pc)[,sample(c(1,2), 1)]
    child <- crossover(X[, parents])[,sample(c(1, 2), 1)]
    x <- mutation(child)

    ## Add new individual:
    neval <- neval + 1
    X[, neval] <- x
    Y[, neval] <- f(x)
    active <- c(active, neval)

    ############################################################
    ## Selection:
    i <- nds_hv_selection(Y[,active])

    ## Remove the i-th active individual:
    eol[active[i]] <- neval
    active <- active[-i]

    ############################################################
    ## Reporting:
    if (trace && neval %% trace == 0)
      message(sprintf("%8i %8.4f", neval, dominated_hypervolume(Y[,active], ref)))
  }
  return(list(X=X, Y=Y, eol=eol, par=X[,active], value=Y[,active]))
}

fun <- zdt1
control <- list(mu=100L,
                trace=FALSE,
                maxeval=30000L
                )

res <- sms_emoa(fun, rep(0, 30), rep(1, 30), control=control)
pf.approx <- nondominated_points(res$Y)
