plot(dpois(0:20,lambda=3), type="l")

library(ggplot2)

# histogramm
data<-data.frame(c(rpois(1000,8))) 
ggplot(data,aes(data)) + 
  xlab("Anzahl Buecher") +
  ylab("Anzahl Studenten") +
  labs(title="Daten") +
  geom_histogram(binwidth=1) 
sum(data)
summary(data)

# line
ggplot(data.frame(x=c(0:20)), aes(x)) +
  xlab("x") +
  ylab("P(X=x)") +
  labs(title="Poisson-Verteilung") + 
  stat_function(n=21, fun=dpois, args=list(6), aes(colour="lambda = 6")) +
  stat_function(n=21, fun=dpois, args=list(8), aes(colour="lambda = 8")) +
  stat_function(n=21, fun=dpois, args=list(10), aes(colour="lambda = 10")) +
  scale_colour_manual("", 
                      breaks = c("lambda = 6", "lambda = 8", "lambda = 10"),
                      values = c("orange", "blue", "purple")) 
  