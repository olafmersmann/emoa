##
## sms_emoa.r - Straight forward SMS-EMOA implementation
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

require(emoa)

sms_emoa <- function(f, lower, upper, ...,
                     control=list(mu=100L,
                       sbx.n=15, sbx.p=0.7,
                       pm.n=25, pm.p=0.3
                       )) {
  ## Extract control parameters:
  default <- formals(sys.function())$control
  control <- steady_state_emoa_control(f, lower, upper, ..., control=control, default=default)
  control <- sbx_control(f, upper, lower, ..., control=control, default=default)
  control <- pm_control(f, upper, lower, ..., control=control, default=default)  
  control$ref <- emoa:::coalesce(control[["ref"]], rep(11, control$d))  

  ## Tracking variables:
  X <- matrix(0, nrow=control$n, ncol=control$maxeval)
  Y <- matrix(0, nrow=control$d, ncol=control$maxeval)
  dob <- rep(-1L, control$maxeval)
  eol <- rep(-1L, control$maxeval)
  
  ## Random inital population:
  X[, 1:control$mu] <- replicate(control$mu, runif(control$n, lower, upper))
  Y[, 1:control$mu] <- sapply(1:control$mu, function(i) f(X[,i]))

  neval <- control$mu       ## Count the number of function evaluations
  active <- 1:control$mu    ## Indices of individuals that are in the current pop.

  ## Save some common control parameters into the current
  ## environment. This saves a few msec of execution time...
  crossover <- control$crossover
  mutate <- control$mutate
  maxeval <- control$maxeval
  logger <- control$logger
  
  logger$start("sms_emoa")
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
  
  res <- structure(list(X=X, Y=Y, eol=eol,
                        par=X[,active], value=Y[,active]),
                   class="emoa_result")
  return(res)
}

zdt3 <- function (x) {
    dim <- length(x)
    y1 <- x[1]
    g <- 1 + (9 * mean(x[2:dim]))
    y2 <- g * (1 - sqrt(y1/g) - (y1/g) * sin(10 * pi * y1))
    return(c(y1, y2))
}

control <- list(mu=100L, maxeval=20000L,
                logger=emoa_console_logger(100L))

res <- sms_emoa(zdt3, rep(0, 30), rep(1, 30), control=control)
