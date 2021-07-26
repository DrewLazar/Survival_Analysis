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
     xlab="survival time in weeks",ylab="survival probabilities",conf.int=.95)
legend("topright",c("Treatment","Placebo"),lty=c("solid","dashed"),
       col=c('black','grey'))
#PROBLEM 2.3
#Log-Rank Test for TR in Remission Data Set 
survdiff(Y~Remission$TR)
#set the vets data set as the default with the attach function 
attach(vets)
#Create a survival object for vets
W<-Surv(Survival.time,Status==1)
#statify on Performance.category and get the KM estimates 
kmvet.per=survfit(W~Performance.Category)
#See your KM estimates by Performance.Category
summary(kmvet.per)
#Plot the KM Estimates 
windows(width=10, height=8)
plot(kmvet.per,lty=c('solid','dashed','dotted'),col=c('black','grey','blue'),
     xlab="survival time in days",ylab="survival probabilities")
legend("topright",c("1","2","3"),lty=c("solid","dashed",'dotted'),
       col=c('black','grey','blue'))
survdiff(Y~Performance.Category)
#Problem 2.4
#Flemington-Harrington for p=0, 0.15, 0.5, 3
for (p in c(0,.15,0.5,3)){
sd= survdiff(Y~Remission$TR,rho=p)
print(paste("The value of p is:",p)); print(sd)
}
#Problem 2.5 
survdiff(Surv(survt,status) ~ TR + strata(LogWBC.group),data=Remission)
#Problem 2.6
#"plain is Greenwood" 
kmfit2=survfit(Y~Remission$TR,conf.type="plain")
plot(kmfit2,lty=c('solid','dashed'),col=c('black','grey'), xlab="survival time in weeks",ylab="survival probabilities",conf.int=.95)
#Problem 2.7 
kmfit2=survfit(Y~Remission$TR)
summary(kmfit2)
#For TR=1 our inequality works for times 4,5 and 8.
#For time t_4=4 
0.5-1.96*0.1029<0.6667
0.5+1.96*0.1029>0.6677
#For time t_5=5
0.5-1.96*0.1080<0.5714 
0.5+1.96*0.1080>0.5714 
#For time t_6=8
0.5-1.96*0.1060<0.3810 
0.5+1.96*0.1060>0.3810
#For time t_7=11
0.5-1.96*0.0986<0.2857
0.5+1.96*0.0986>0.2857
#We get CI (4,12) as our confidence interval. We use the code below
#to get it directly in R
#Simply comparing medians the survival experience for the treatment group
#is better
#Chapter 3 
#Problem 3.1
#model 1 - just TR
Coxph.Rem.m1=coxph(Y~TR,data=Remission)
summary(Coxph.Rem.m1)
Coxph.Rem.m1$loglik
2*(1-pnorm(3.812)) #pvalue 
#model 2 - TR and logWBC
Coxph.Rem.m2=coxph(Y~TR+logWBC,data=Remission)
summary(Coxph.Rem.m2)
Coxph.Rem.m2$loglik
#model 3 - TR, logWBC and interaction 
Coxph.Rem.m3=coxph(Y~TR+logWBC+logWBC*TR,data=Remission)
summary(Coxph.Rem.m3)
Coxph.Rem.m3$loglik
#95% Confidence Interval for HR in model 3 (assuming interaction) 
#setting logWBC = 2.930238
mean(Remission$logWBC)
vcov(Coxph.Rem.m3)
exp(2.3749-0.3175*2.930238+1.96*sqrt((2.9086116+2*(2.930238)*(-0.8687027)+2.930238^2*0.2764537)))
exp(2.3749-0.3175*2.930238- 1.96*sqrt((2.9086116+2*2.930238*(-0.8687027)+2.930238^2*0.2764537)))
#Problem 3.2
windows(width=10, height=8)
pattern1=data.frame(TR=0,logWBC=2.930238)
summary(survfit(Coxph.Rem.m2,newdata=pattern1))
plot(survfit(Coxph.Rem.m2,newdata=pattern1),conf.int=F,main="Adjusted survival for TR=0 vs TR=1, mean(logWBC)")
pattern2=data.frame(TR=1,logWBC=2.930238)
summary(survfit(Coxph.Rem.m2,newdata=pattern2))
par(new=TRUE)
plot(survfit(Coxph.Rem.m2,newdata=pattern2),conf.int=F,lty='dashed')
legend("topright",c("Treatment","Placebo"),lty=c("solid","dashed"),
       col=c('black','grey'))

#Chapter 7
attach(Remission)
Y=Surv(survt,status)
remmod.exp=survreg(Y~TR,dist="exponential")
summary(remmod.exp)