##
## sms_emoa.r - Straight forward SMS-EMOA implementation
##
## Author:
##  Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>
##
require("emoa")
require("mco")
require("ggplot2")

source("sms_emoa.r")

zdt3 <- function (x) {
  dim <- length(x)
  y1 <- x[1]
  g <- 1 + (9 * mean(x[2:dim]))
  y2 <- g * (1 - sqrt(y1/g) - (y1/g) * sin(10 * pi * y1))
  return(c(y1, y2))
}

doit <- function() {
  r1 <- sms_emoa(zdt3, rep(0, 5), rep(1, 5), control=control)
  f1 <- nondominated_points(r1$value)
  
  hv.sms <- dominated_hypervolume(f1, c(11, 11))
  nopt.sms <- ncol(f1)
  
  r2 <- nsga2(zdt3, 5, 2,
              lower.bounds=rep(0, 5), upper.bounds=rep(1, 5),
              popsize=mu,
              generations=gen)
  f2 <- nondominated_points(t(r2$value))
  
  hv.nsga2 <- dominated_hypervolume(f2, c(11, 11))
  nopt.nsga2 <- ncol(f2)

  data.frame(method=c("sms_emoa", "nsga2"),
             hypervolume=c(hv.sms, hv.nsga2),
             nopt=c(nopt.sms, nopt.nsga2))
}

mu <- 100L
gen <- 100L
control <- list(mu=mu, maxeval=mu*gen,
                logger=emoa_console_logger(5000L))

res <- replicate(10L, doit(), simplify=FALSE)
df <- Reduce(rbind, res)

plt <- qplot(hypervolume, data=subset(df, hypervolume > 127), fill=method, geom="density")
