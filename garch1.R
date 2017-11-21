# https://www.quantstart.com/articles/Generalised-Autoregressive-Conditional-Heteroskedasticity-GARCH-p-q-Models-for-Time-Series-Analysis

# garch(1,1)
set.seed(2)
a0 <- 0.2
a1 <- 0.5
b1 <- 0.3
w <- rnorm(1000)
eps <- rep(0, 1000)
sigsq <- rep(0, 1000)

for (i in 2:1000) {
  sigsq[i] <- a0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1]
  eps[i] <- w[i]*sqrt(sigsq[i])
}
plot(eps, type="l")
acf(eps^2)

require(tseries)

eps.garch <- garch(eps)
plot(eps.garch)
confint(eps.garch)
