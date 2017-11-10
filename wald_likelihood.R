plot(function(x) dt(x, df = 3), -2, 2, ylim = c(0, 0.5),
     main = "t-Dichte", yaxs = "i")

library(ggplot2)

ggplot(data.frame(x=c(-2:2)), aes(x)) +
  labs(title="Wald Test - Likelihood") + 
  xlab("alpha") +
  ylab("log L(alpha)") +
  stat_function(fun=dt, args=list(3)) +
  geom_vline(xintercept=0, linetype=5, colour="cadetblue") +
  geom_vline(xintercept=1, linetype=2, colour="orange") +
  geom_segment(aes(x=0,xend=1,y=0.2,yend=0.2), colour="darkgrey") +
  geom_text(aes(x=-0.1, label="alpha hut", y=0.1), colour="cadetblue", angle=90) +
  geom_text(aes(x=0.9, label="alpha null", y=0.1), colour="orange", angle=90) +
  geom_text(aes(x=0.5, label="Wald", y=0.21), colour="darkgrey")
  