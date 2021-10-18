rm(list=ls())
library(survival)
#1
#i
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
Ven.reset <-read.csv("Venreset.csv", header = TRUE)
#ii
Y<-Surv(Ven.reset$eventtime,Ven.reset$status==1)
kmfit3=survfit(Y~Ven.reset$Setting)
summary(kmfit3)
windows(width=10, height=8)
plot(kmfit3,col=c('green','blue','red'),
     xlab="event time in days",ylab="survival probabilities")
legend("topright",lty=c("solid","solid","solid"),c("Setting=0","Setting=1","Setting=3"),
       col=c('green','blue','red'))
survdiff(Y~Ven.reset$Setting)
#iii
Coxph.Ven.m1=coxph(Y~Setting,data=Ven.reset)
summary(Coxph.Ven.m1)
Coxph.Ven.m2=coxph(Y~Setting+LO2,data=Ven.reset)
summary(Coxph.Ven.m2)
Coxph.Ven.m3=coxph(Y~Setting+LO2+Setting*LO2,data=Ven.reset)
summary(Coxph.Ven.m3)
#iv
quantile(Ven.reset$LO2)
q1=1.0075
q2=2.0350
q3=3.0825
b1=1.38621 ;b2=1.14922 ;b3=-0.17039;
vcov(Coxph.Ven.m3)
cb1b3=-0.010133008; vb1=0.037128597;vb3=0.004633287; 
#effect of setting with LO2=q1 and 95% CI for effect  
ellh=b1+b3*q1;exp(ellh)
SE.ellh=sqrt(vb1+2*q1*cb1b3+q1^2*vb3)
exp(ellh-1.96*SE.ellh);exp(ellh+1.96*SE.ellh); 
#effect of setting with LO2=q2 
ellh=b1+b3*q2;exp(ellh)
SE.ellh=sqrt(vb1+2*q2*cb1b3+q2^2*vb3)
exp(ellh-1.96*SE.ellh);exp(ellh+1.96*SE.ellh); 
#effect of setting with LO2=q3
ellh=b1+b3*q3;exp(ellh)
SE.ellh=sqrt(vb1+2*q3*cb1b3+q3^2*vb3)
exp(ellh-1.96*SE.ellh);exp(ellh+1.96*SE.ellh)
#vi 
windows(width=10, height=8)
pattern1=data.frame(Setting=0,LO2=q1)
pattern2=data.frame(Setting=1,LO2=q1)
pattern3=data.frame(Setting=2,LO2=q1)
plot(survfit(Coxph.Ven.m3,newdata=pattern1),col=('red'),conf.int=F,main="Adjusted survival curves of Setting with LO2=q1",
     xlab='Time to Ventilator Reset (days)',ylab='Survival Probabilities')
par(new=TRUE)
plot(survfit(Coxph.Ven.m3,newdata=pattern2),conf.int=F,col=c('blue')) 
par(new=TRUE)
plot(survfit(Coxph.Ven.m3,nwwdata=pattern3),conf.int=F,col=c('green'))
legend("topright",c("Setting=0","Setting=1","Setting=2"),lty=c('solid'),col=c('red','blue','green'))
windows(width=10, height=8)
pattern1=data.frame(Setting=0,LO2=q3)
pattern2=data.frame(Setting=1,LO2=q3)
pattern3=data.frame(Setting=2,LO2=q3)
plot(survfit(Coxph.Ven.m3,newdata=pattern1),conf.int=F,main="Adjusted survival curves of Setting with LO2=q3",
     xlab='Time to Ventilator Reset (days)',ylab='Survival Probabilities')
par(new=TRUE)
plot(survfit(Coxph.Ven.m3,newdata=pattern2),conf.int=F,col=c('blue')) 
par(new=TRUE)
plot(survfit(Coxph.Ven.m3,newdata=pattern3),conf.int=F,col=c('green'))
legend("topright",c("Setting=0","Setting=1","Setting=2"),lty=c('solid'),col=c('black','blue','green'))
#Problem 2
f1<-function(b) 1/(2*exp(b)+2+exp(2*b)+2*exp(1.5*b))
f2<-function(b) (exp(b)+exp(1.5*b))/(exp(b)+exp(2*b)+2*exp(1.5*b))
f3<-function(b) exp(2*b)/(exp(2*b)+exp(1.5*b))
f <- function(b) -f1(b)*f2(b)*f3(b)
x <- seq(-20,15,0.01)
windows(width=10, height=8)
plot(x, f(x))   
optimize(f, lower = -2, upper = 1)
time<-c(2,3,5,6,6,7,8);status<-c(1,0,0,1,1,1,1);Dose<-c(0,1,0,1,1.5,2,1.5)
RXTrial=data.frame(status,time,Dose)
S<-Surv(RXTrial$time,RXTrial$status==1)
coxph(S~Dose,data=RXTrial)

