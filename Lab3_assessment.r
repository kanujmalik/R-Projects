library(ggplot2)
library(dplyr)

#Task 1-------

#Time Period
weeks <- 104

#Rates
alpha <- 0.05
beta <-0.0001
gamma <-0.02

#Initial values
Ro <- 30
Fo <- 40

#Lotka-VOlterra model
for (t in 1:(weeks-1)) {
  Ro[t+1] <- Ro[t] + alpha*Ro[t] - beta*Ro[t]*Fo[t]
  Fo[t+1] <- Fo[t] + beta*Ro[t]*Fo[t] - gamma*Fo[t]
  print(Ro[t+1])
  print(Fo[t+1])
}



#Task 2------------------

#Time Period
weeks <- 104

#set seed
set.seed(60854)

#Initial values
Ros <- 30
Fos <- 40



#Lotka-VOlterra model
for (t in 1:(weeks-1)) {
  Ros[t+1] <- Ros[t] + rbinom(1,Ros[t],alpha) - rbinom(1,Ros[t]*Fos[t],beta)
  Fos[t+1] <- Fos[t] + rbinom(1,Ros[t]*Fos[t],beta) - rbinom(1,Fos[t],gamma)
  print(Ros[t+1])
  print(Fos[t+1])
}



#Task3---------
LV <- data.frame(
  time = rep(1:weeks, 4),
  group = rep(c("rabbits", "foxes", "sto_rabbits", "sto_foxes"), each = weeks),
  size = c(Ro, Fo, Ros, Fos)
)



ggplot(LV) + 
  geom_line(aes(x=time, y=size, colour=group, linetype=group))
