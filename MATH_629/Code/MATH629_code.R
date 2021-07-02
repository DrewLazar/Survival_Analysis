#Remove any variables in your environment 
rm(list=ls())
#load the R Survival package 
library(survival)
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#CHAPTER 2 
#PROBLEM 2.2
#load Remission data 
load("Remission.rda")
#Create a Survival object - this is like the second way 
  #to represent data in Chapter 1. 
Y<-Surv(Remission$survt,Remission$status==1)
#Create a KM model with no partitioning 
kmfit1=survfit(Y~1)
#See your KM estimates 
summary(kmfit1)
#Plot your KM estimates
windows(width=10, height=8)
plot(kmfit1,xlab="survival time in weeks",ylab="survival probabilities")
#Ways to get particular estimate at time or times
summary(kmfit1,times=11)
summary(kmfit1,times=c(11,12,15))
#statify on treatment and get the KM estimates 
kmfit2=survfit(Y~Remission$TR)
#See your KM estimates by TR
summary(kmfit2)
#Plot the KM Estimates 
windows(width=10, height=8)
plot(kmfit2,lty=c('solid','dashed'),col=c('black','grey'),
     xlab="survival time in weeks",ylab="survival probabilities")
legend("topright",c("Treatment","Placebo"),lty=c("solid","dashed"),
       col=c('black','grey'))
#PROBLEM 2.3
#Log-Rank Test for TR in Remission Data Set 
survdiff(Y~Remission$TR)
#set the vets data set as the default with the attach function 
attach(vets)
#Create a survival object for vets
Y<-Surv(Survival.time,Status==1)
#statify on Performance.category and get the KM estimates 
kmvet.per=survfit(Y~Performance.Category)
#See your KM estimates by Performance.Category
summary(kmvet.per)
#Plot the KM Estimates 
windows(width=10, height=8)
plot(kmvet.per,lty=c('solid','dashed','dotted'),col=c('black','grey','blue'),
     xlab="survival time in days",ylab="survival probabilities")
legend("topright",c("1","2","3"),lty=c("solid","dashed",'dotted'),
       col=c('black','grey','blue'))
survdiff(Y~Performance.Category)
#Chapter 7
attach(Remission)
Y=Surv(survt,status)
remmod.exp=survreg(Y~TR,dist="exponential")
summary(remmod.exp)