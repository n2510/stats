# https://www.quantstart.com/articles/Generalised-Autoregressive-Conditional-Heteroskedasticity-GARCH-p-q-Models-for-Time-Series-Analysis

# garch(1,1)
set.seed(2)
a0 <- 0.2
a1 <- 0.5
b1 <- 0.3
w <- rnorm(10000)
eps <- rep(0, 10000)
sigsq <- rep(0, 10000)
for (i in 2:10000) {
   sigsq[i] <- a0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1]
   eps[i] <- w[i]*sqrt(sigsq[i])
}

acf(eps^2)

require(tseries)

eps.garch <- garch(eps, trace=FALSE)
plot(eps.garch)
confint(eps.garch)


# 
# # financial data with garch
# require(quantmod)
# getSymbols("^FTSE")
# 
# ftrt = diff(log(Cl(FTSE)))
# plot(ftrt)
# 
# ft <- as.numeric(ftrt)
# ft <- ft[!is.na(ft)]
# 
# ft.garch <- garch(ft, trace=F)
# ft.res <- ft.garch$res[-1]
# acf(ft.res^2)