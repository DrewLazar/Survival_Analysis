library(survival)
rm(list=ls())
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
load("Remission.rda")
#Create Survival Object 
Y<-Surv(Remission$survt,Remission$status==1)
kmfit2=survfit(Y~Remission$RX)
summary(kmfit2)
plot(kmfit2,lty=c('solid','dashed'),col=c('black','grey'),
     xlab="Remission time in weeks",ylab="Remissison probabilities")