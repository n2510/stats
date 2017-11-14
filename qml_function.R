fct = function(a) {
  fct = -(a^2)/2 + log(a) -log(sqrt(2*pi))
  return(fct)
}

plot(0:35, fct(0:35), type="l")

fct2 = function(a) {
  fct2 = -a + log(a) -log(2)
  return(fct2)
}

library(ggplot2)

ggplot(data.frame(x=c(0.5:10)), aes(x)) +
  labs(title="QML Funktion") +
  xlab("Sigma") +
  ylab("E_f g(eta_0, sigma)") +
  stat_function(fun=fct) +
  geom_vline(xintercept=1, linetype=2, colour="cadetblue") +
  geom_text(aes(x=3, label="maximum: sigma = 1", y=0), colour="cadetblue")

ggplot(data.frame(x=c(0.5:10)), aes(x)) +
  labs(title="Laplace QML Funktion") +
  xlab("Sigma") +
  ylab("E_f g(eta_0, sigma)") +
  stat_function(fun=fct2) +
  geom_vline(xintercept=1, linetype=2, colour="orange") +
  geom_text(aes(x=3, label="maximum: sigma = 1", y=-1.5), colour="orange")