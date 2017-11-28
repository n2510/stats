library('stats')

## garch(1,1)
set.seed(100)
SimulatedData <- function(coeffs) {
  
  w0 <- coeffs[1]              # omega_0
  a1 <- coeffs[2]              # alpha_0
  b1 <- coeffs[3]              # beta_0
  n <- rnorm(10000)             # eta_t
  eps <- rep(0,10000)           # epsilon_t
  sigsq <- rep(0, 10000)        # sigma^2
  
  for (i in 2:10000) {
    sigsq[i] <- w0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1]
    eps[i] <- n[i] * sqrt(sigsq[i])
  }
  
  return(list(eps_t = eps, sig_t = sigsq))
}

coeffs <- c(0.2, 0.5, 0.3)

GARCHdata <- SimulatedData(coeffs)

par(mfrow=c(2,1))
plot(GARCHdata$eps_t, type="l")
plot(sqrt(GARCHdata$sig_t), type="l")



## negative of log likelihood

GarchLogLikelihood <- function(coeffs) {
  
  w0 <- coeffs[1]              # omega_0
  a1 <- coeffs[2]              # alpha_0
  b1 <- coeffs[3]              # beta_0
  sigsq <- rep(0, 10000)       # sigma^2
  
  sigsq[1] = w0

    for (i in 2:10000) {
    sigsq[i] <- w0 + a1 * (GARCHdata$eps_t[i-1]^2) + b1 * sigsq[i-1]
  }
  
  return (-sum(dnorm(GARCHdata$eps_t, mean = 0 , sd=sqrt(sigsq), log=TRUE)))
}

GarchLogLikelihood(coeffs)

## Non-Linear Minimization
nlm(GarchLogLikelihood, c(0.1,0.1,0.1)) # start at 0.1 0.1 0.1
