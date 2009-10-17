##
## sms_emoa.r - Simple straight forward SMS-EMOA implementation
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##

require(mco)
require(emoa)

coalesce <- emoa:::coalesce

sms_emoa <- function(f, lower, upper, ...,
                     control=list(
                       n=NULL,
                       d=NULL,
                       mu=100L,
                       maxeval=NULL,
                       nc=15, pc=0.7,
                       nm=25, pm=0.3
                       )) {
  ## f:R^n -> R^d:
  n <- coalesce(control[["n"]], max(length(lower), length(upper)))
  d <- coalesce(control[["d"]], length(f(rep(NA, n))))
  ## Population size:
  mu <- coalesce(control[["mu"]], 100L)
  ## Stopping Criterion:
  maxeval <- coalesce(control[["maxeval"]], mu*300L)
  neval <- 0

  ## Crossover parameters:
  nc <- coalesce(control[["nc"]], 5)
  pc <- coalesce(control[["pc"]], 1.0)
  crossover <- sbx_operator(nc, pc, lower, upper)

  ## Mutation parameters:
  nm <- coalesce(control[["nm"]], 10)
  pm <- coalesce(control[["pm"]], .2)
  mutation <- pm_operator(nm, pm, lower, upper)

  ## Constants:
  v <- 1:mu

  ## Inital population:
  par <- replicate(mu, runif(n, lower, upper))
  value <- sapply(1:ncol(par), function(i) f(par[,i]))
  neval <- ncol(par)
  ranks <- rep(1, mu)
  sc <- 0
  message(sprintf("%8s %8s %5s %5s %5s %5s %5s %5s",
                  "NEVAL", "HV", "ARATE",
                  "NF1", "NF2", "NF3", "NF4", "NF5"))
  while(neval < maxeval) {
    ## parents <- sample(v, 2, prob=exp(-ranks))
    parents <- sample(v, 2)
    children <- .Call("do_sbx", par[, parents], lower,  upper, nc, pc)
    child <- children[,sample(c(1, 2), 1)]
    x <- mutation(child)
    y <- f(x)
    par <- cbind(par, x)
    
    ## Evaluation:
    value <- cbind(value, y)
    neval <- neval + 1
    
    ## Selection:
    ranks <- nds_rank(value)
    bad <- max(ranks)
    sel  <- which(ranks == bad)
    ## Identify individual which gets replaced:
    i <- if (length(sel) == 1) {
      sel
    } else {
      front <- value[,sel]
      contrib <- hypervolume_contribution(front)
      sel[which.min(contrib)]
    }
    ## Remove the i-th individual:
    sc <- sc + (i <= mu)
    par <- par[,-i]
    value <- value[,-i]
    ranks <- ranks[-i]

    ## Reporting:
    if (neval %% 100L == 0)
      message(sprintf("%8i %8.4f %5.3f %5i %5i %5i %5i %5i",
                      neval,
                      dominated_hypervolume(value, c(11, 11)),
                      sc/(neval-mu),
                      sum(ranks==1),
                      sum(ranks==2),
                      sum(ranks==3),
                      sum(ranks==4),
                      sum(ranks==5)
                      ))
  }
  return(list(par=par, value=value))
}

fun <- zdt1
t <- system.time(x <- sms_emoa(fun, rep(0, 30), rep(1, 30),
                               control=list(mu=100L, maxeval=30000L)))
print(t)
print(dominated_hypervolume(x$value, c(11, 11)))

res <- nsga2(fun, 30, 2,
             lower.bounds=rep(0, 30),
             upper.bounds=rep(1, 30),
             generations=c(100, 300))

hv <- sapply(res, function(i) dominated_hypervolume(t(i$value), c(11, 11)))
print(hv)
