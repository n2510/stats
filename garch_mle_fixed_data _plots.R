library('stats')

## garch(1,1)
set.seed(100)
SimulatedData <- function(coeffs) {
  
  w0 <- coeffs[1]              # omega_0
  a0 <- coeffs[2]              # alpha_0
  b0 <- coeffs[3]              # beta_0
  n <- rnorm(10000)             # eta_t
  eps <- rep(0,10000)           # epsilon_t
  sigsq <- rep(0, 10000)        # sigma^2
  
  for (i in 2:10000) {
    sigsq[i] <- w0 + a0 * (eps[i-1]^2) + b0 * sigsq[i-1]
    eps[i] <- n[i] * sqrt(sigsq[i])
  }
  
  return(list(eps_t = eps, sig_t = sigsq))
}

coeffs <- c(0.2, 0.5, 0.3)

GARCHdata <- SimulatedData(coeffs)

# plot(GARCHdata$eps_t, type="l")
# plot(sqrt(GARCHdata$sig_t), type="l")

library(ggplot2)

# plot eps_t
ggplot(data.frame(x=c(0:9999), y=GARCHdata$eps_t), aes(x, y)) +
  labs(title="GARCH(1,1)") +
  xlab("t") +
  ylab("eps_t") +
  geom_line(color="grey10")

# plot acf of (eps_t)^2 
eps2acf <- acf(GARCHdata$eps_t^2, plot = FALSE)
eps2acf_df <- with(eps2acf, data.frame(lag, acf))

ggplot(data = eps2acf_df, mapping = aes(x = lag, y = acf)) +
  labs(title="ACF((eps_t)^2)") +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), color="midnightblue")



## negative of log likelihood

GarchLogLikelihood <- function(coeffs) {
  
  w0 <- coeffs[1]              # omega_0
  a0 <- coeffs[2]              # alpha_0
  b0 <- coeffs[3]              # beta_0
  sigsq <- rep(0, 10000)       # sigma^2
  
  sigsq[1] = w0

    for (i in 2:10000) {
    sigsq[i] <- w0 + a0 * (GARCHdata$eps_t[i-1]^2) + b0 * sigsq[i-1]
  }
  
  return (-sum(dnorm(GARCHdata$eps_t, mean = 0 , sd=sqrt(sigsq), log=TRUE)))
}

GarchLogLikelihood(coeffs)

## Non-Linear Minimization
mle <- nlm(GarchLogLikelihood, c(0.1,0.1,0.1)) # start at 0.1 0.1 0.1

mle 
