datos <- read.csv("Nicho/Datos/HeV-survival.csv")

d.4 <- subset(datos, Temp == 4)
d.22 <- subset(datos, Temp == 22)
d.56 <- subset(datos, Temp == 56)

library(ggplot2)

ggplot(d.4) + geom_point(aes(x = Time, y = ln.S)) +
  geom_smooth(aes(x = Time, y = ln.S), method = "lm")

ggplot(d.22) + geom_point(aes(x = Time, y = ln.S)) +
  geom_smooth(aes(x = Time, y = ln.S), method = "loess")

ggplot(d.56) + geom_point(aes(x = Time, y = ln.S)) +
  geom_smooth(aes(x = Time, y = ln.S), method = "loess")

# 

# Regresiones

m4 <- nls(ln.S ~ -( rho * Time )^ (kappa), 
          data = d.4, 
          start = list(rho = 1, kappa = 0.9),
          lower = c(0.00001, 0.1),
          upper = c(1, 1),
          algorithm =  "port")

m22 <- nls(ln.S ~ -( rho * Time )^ (kappa), 
          data = d.22, 
          start = list(rho = 1, kappa = 0.9),
          lower = c(0.0001, 0.1),
          upper = c(1, 1),
          algorithm =  "port")

m56 <- nls(ln.S ~ -( rho * Time )^ (kappa), 
          data = d.56, 
          start = list(rho = 1, kappa = 0.9),
          lower = c(0.1, 0.1),
          upper = c(105, 1.5),
          algorithm =  "port")

# Identificando efecto de temperatura sobre rho y kappa

par.estim <- data.frame(rbind(coef(m4), coef(m22), coef(m56)))

par.estim$Temp <-c(4, 22, 56)

ggplot(par.estim) + geom_point(aes(x = Temp, y = log(rho)))
ggplot(par.estim) + geom_point(aes(x = Temp, y = log(kappa)))
