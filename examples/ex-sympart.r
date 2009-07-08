require(emoa)
require(lhs)

x <- seq(-1, 1, by=0.02)

n <- 20000
d <- 4

X <- t(-20 + 40 * randomLHS(n, d))
z <- apply(X, 2, sympart)

opt <- nonDominatedPoints(z)

opar <- par(mfrow=c(1, 2))
plot(t(z), main="Pareto Front", log="xy",
     pch=19, col=ifelse(opt, "red", "black"))

plot(t(X[1:2,]), main="Pareto Set",
     pch=19, col=ifelse(opt, "red", "black"))
points(t(X[1:2, opt]), col="red", pch=19)
par(opar)
