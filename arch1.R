# arch(1)
set.seed(2)
a0 <- 0.2
a1 <- 0.9
w <- rt(1000, 5) # student t
eps <- rep(0, 1000)
sigsq <- rep(0, 1000)

for (i in 2:1000) {
  sigsq[i] <- a0 + a1 * (eps[i-1]^2) 
  eps[i] <- w[i]*sqrt(sigsq[i])
}
acf(eps^2)

