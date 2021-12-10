rm(list=ls())
#load the necessary packages 
library(survival)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#Load the Ventilator data set
Ven.reset <-read.csv("VenresetMft.csv", header = TRUE)
Y<-Surv(Ven.reset$eventtime,Ven.reset$status==1)
kmfitST3=survfit(Y~Ven.reset$Setting)
windows(width=10, height=8)
plot(kmfitST3,fun="cloglog",xlab="time in days on log scale",ylab="log-log survival", main="log-log curves by Setting",col=c('red','green','blue'))
legend("topright",c("Setting=0","Setting=1","Setting=2"),lty=c("solid"),col=c("red","green","blue"))

quantile(Ven.reset$LO2)
Ven.reset$LO2.group<-cut(Ven.reset$LO2,c(-2.58,1.0075,2.0350,3.0825,6.2300),labels=c('1','2','3','4'))
length(unique(Ven.reset$LO2.group))
kmfitO24=survfit(Y~Ven.reset$LO2.group)
windows(width=10, height=8)
plot(kmfitO24,fun="cloglog",xlab="time in days on log scale",ylab="log-log survival", main="log-log curves by LO2group",col=c("red","green","blue","black"))
legend("topright",c("LO2=high","LO2=medhigh","LO2=medlow","LO2=low"),lty=c("solid"),col=c("red","green","blue","black"))

kmfitMft=survfit(Y~Ven.reset$Mft)
windows(width=10, height=8)
plot(kmfitMft,fun="cloglog",xlab="time in days on log scale",ylab="log-log survival", main="log-log curves by Setting",col=c('red','green','blue'))
legend("topright",c("Setting=0","Setting=1","Setting=2"),lty=c("solid"),col=c("red","green","blue"))




windows(width=10, height=8)
plot(log(kmfitST3$time),log(kmfitST3$surv/(1-kmfitST3$surv)),xlab="ln(time)",
     ylab="ln[(1-S(t))/S(t))]",main="ln(t) vs Failure Odds by Iron.group")

windows(width=10, height=8)
plot(log(kmfitO24$time),log(kmfitO24$surv/(1-kmfitO24$surv)),xlab="ln(time)",
     ylab="ln[(1-S(t))/S(t))]",main="ln(t) vs Failure Odds by Iron.group")

windows(width=10, height=8)
plot(log(kmfitMft$time),log(kmfitMft$surv/(1-kmfitMft$surv)),xlab="ln(time)",
     ylab="ln[(1-S(t))/S(t))]",main="ln(t) vs Failure Odds by Iron.group")

