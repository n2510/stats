# dichte
dens = function(y, a) {
  dens = ((a^a)/gamma(a)) * exp(-a * y^2) * abs(y)^(2*a-1)
  return(dens)
}

plot(-3:3, dens(-3:3, 1), type="l")

library(ggplot2)

# vergleichende plots
ggplot(data.frame(x=c(-3:3)), aes(x)) +
  labs(title="Dichte") + 
  stat_function(fun=dens, args=list(1/8), aes(colour="a = 1/8")) +
  stat_function(fun=dens, args=list(1/4), aes(colour="a = 1/4")) +
  stat_function(fun=dens, args=list(1/2), aes(colour="a = 1/2")) +
  stat_function(fun=dens, args=list(1), aes(colour="a = 1")) +
  stat_function(fun=dens, args=list(2), aes(colour="a = 2")) +
  scale_colour_manual("", 
                      breaks = c("a = 1/8", "a = 1/4", "a = 1/2", "a = 1", "a = 2"),
                      values = c("cadetblue", "orange", "lightskyblue", "chocolate", "royalblue")) 

ggplot(data.frame(x=c(-3:3)), aes(x)) +
  labs(title="Dichte") + 
  stat_function(fun=dens, args=list(1/100), aes(colour="a = 1/100")) +
  stat_function(fun=dens, args=list(1/20), aes(colour="a = 1/20")) +
  stat_function(fun=dens, args=list(3/2), aes(colour="a = 3/2")) +
  stat_function(fun=dens, args=list(3), aes(colour="a = 3")) +
  stat_function(fun=dens, args=list(25), aes(colour="a = 25")) +
  scale_colour_manual("", 
                      breaks = c("a = 1/100", "a = 1/20", "a = 3/2", "a = 3", "a = 25"),
                      values = c("cadetblue3", "orange1", "lightskyblue4", "chocolate1", "royalblue1")) 