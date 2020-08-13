x<-seq(from = -10, to = 10, by = 0.1)
alpha<-0.5
beta<-0.4
p<-(exp(alpha+beta*x))/(1+exp(alpha+beta*x))
p
plot(p~x,type='l',ylab = "p(x)", lwd=3)
