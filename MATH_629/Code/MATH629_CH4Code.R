#Chapter 3 
#Remove any variables in your environment 
rm(list=ls())
#load the R Survival package 
library(survival)
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
#Problem 4.1
#Create a Survival Object 
Y<-Surv(Remission$survt,Remission$status==1)
#4.1a 
#Fit KM curves against TR
kmfitTR2<-survfit(Y~TR,data=Remission)
#Plot log(-log) survival curves against survt - note this function
#takes the log of the negative log (doesn't use another negative)
windows(width=10, height=8)
plot(kmfitTR2,fun="cloglog",xlab="time in weeks",ylab="log-log survival", col=c('blue','green'), main="log-log curves by treatment")
legend("topright",c("Treatment","Placebo"),lty=c("solid"),
       col=c('blue','green'))
#Fit KM curves against logWBC.group 
kmfitLWBC3<-survfit(Y~LogWBC.group,data=Remission)
#Plot log(-log) survival curves against survt 
windows(width=10, height=8)
plot(kmfitLWBC3,fun="cloglog",xlab="time in weeks",ylab="log-log survival", col=c('blue','green','red'), main="log-log curves by treatment")
legend("topright",c("Low LogWBC","Medium LogWBC","High LogWBC" ),lty=c("solid"),
       col=c('blue','green','red'))
#Fit KM curves against logWBC.group 
kmfitS2<-survfit(Y~Sex,data=Remission)
#Plot log(-log) survival curves against Sex
summary(kmfitS2)
windows(width=10, height=8)
plot(kmfitS2,fun="cloglog",xlab="time in weeks",ylab="log-log survival", col=c('blue','green','red'), main="log-log curves by treatment")
legend("topright",c("Male (Sex=0)","Female (Sex=1)"),lty=c("solid"),
       col=c('blue','green'))
#Problem 4.2
#Stratify Remmision Data set by TR
Remission0<-Remission[Remission$TR==0, ]
Remission1<-Remission[Remission$TR==1, ]
#Create Survival Objects for both strata 
Y0<-Surv(Remission0$survt,Remission0$status==1)
Y1<-Surv(Remission1$survt,Remission1$status==1)
#Fit Cox PH models to both strata
Coxph.Rem.m0=coxph(Y0~logWBC,data=Remission0)
Coxph.Rem.m1=coxph(Y1~logWBC,data=Remission1)

