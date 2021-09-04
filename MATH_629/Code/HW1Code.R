rm(list=ls())
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data") 
multmyl <-read.delim("multmylenoma.txt", header = TRUE, sep = "\t",
                    quote = "")
library(survival)
attach(multmyl)
Y<-Surv(Survt,Status==1)
#Create a KM model with no partitioning 
kmfit1=survfit(Y~1)
summary(kmfit1)
windows(width=10, height=8)
plot(kmfit1,xlab="survival time in weeks",ylab="survival probabilities")
kmfit2=survfit(Y~Sex)
summary(kmfit2)
windows(width=10, height=8)
plot(kmfit2,lty=c('solid','dashed'),col=c('black','grey'),
     xlab="survival time in weeks",ylab="survival probabilities")
legend("topright",c("Women","Men"),lty=c("solid","dashed"),
       col=c('black','grey'))