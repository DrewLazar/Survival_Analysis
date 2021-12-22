
rm(list=ls())
library(survival)

#Q4 data 
pf<-c(2,2,2,3,3,4,4,5,5,5,5,7.6,7.6,8,8)
gm<-c(1,1,2.5,2.5,2.5,2.5,3,3,3,3.5,3.5,4,4,4,4)
pfc<-c(1,1,1,0,0,1,0,1,1,1,1,0,1,1,0)
gmc<-c(1,0,1,1,1,0,1,1,0,1,0,1,1,1,0)
RX<-c(rep(0,15),rep(1,15))ex
response<-c(pf,gm)
status<-c(pfc,gmc)

blooddata<-data.frame(response,status,RX)

Y<-Surv(blooddata$response,blooddata$status==1)
kmfitq4=survfit(Y~blooddata$RX)
summary(kmfitq4)

eq = function(x){exp(-0.1852591*x)}
plot(eq,0,7,col=('blue'),xlab='time in days',ylab='Survival Probabilities',ylim=c(0,1))
par(new=TRUE)
eq2 = function(x){exp(-.467*x)}
plot(eq2,0,7,col=('red'),xlab='time in days',ylab='Survival Probabilities',ylim=c(0,1))
