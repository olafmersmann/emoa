require(emoa)
require(lhs)

x <- seq(-1, 1, by=0.02)

n <- 10000
d <- 3

X <- c(0, rep(-1, d-1)) + c(1, rep(2, d-1)) * t(randomLHS(n, d))
z <- apply(X, 2, UF1)

opt <- nonDominatedPoints(z)
o <- order(opt)
X <- X[, o]
z <- z[, o]
opt <- opt[o]

pairs(t(X),
      pch=19,
      col=ifelse(opt, "red", "black"))
