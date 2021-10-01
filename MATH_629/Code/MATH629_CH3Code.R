#Chapter 3 
#Remove any variables in your environment 
rm(list=ls())
#load the R Survival package 
library(survival)
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#CHAPTER 2 
#PROBLEM 2.2xc    
#load Remission data 
load("Remission.rda")
#Problem 3.1
#Create a Survival Object 
Y<-Surv(Remission$survt,Remission$status==1)
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
exp(2.3749-2.930238*0.3175)
#estimate of HR(TR=0 vs TR=1)
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
#Example 3.2
f1<-function(b) exp(b)/(3*exp(b)+1)
f2<-function(b) 1/(1+exp(b))
f <- function(b) -f1(b)*f2(b)
#f <- function(b) -exp(b)/(3*exp(2*b)+4*exp(b)+1)
x <- seq(-1,10,0.01)
windows(width=10, height=8)
plot(x, f(x))   
optimize(f, lower = -1, upper = 0)
time<-c(2,3,5,8);status<-c(1,0,1,1);Coupon<-c(1,1,0,1)
Sales=data.frame(Coupon,status,time)
S<-Surv(Sales$time,Sales$status==1)
coxph(S~Coupon,data=Sales)
#Problem 3.2 
#From problem 3.1 
#model 1 - just TR
Coxph.Rem.m1=coxph(Y~TR,data=Remission)
summary(Coxph.Rem.m1)
Coxph.Rem.m1$loglik
#model 2 - TR and logWBC
Coxph.Rem.m2=coxph(Y~TR+logWBC,data=Remission)
summary(Coxph.Rem.m2)
Coxph.Rem.m2$loglik
#model 3 - TR, logWBC and interaction 
Coxph.Rem.m3=coxph(Y~TR+logWBC+logWBC*TR,data=Remission)
summary(Coxph.Rem.m3)
Coxph.Rem.m3$loglik
#constant main effect model
Coxph.Rem.me=coxph(Y~1,data=Remission)
summary(Coxph.Rem.me)
Coxph.Rem.me$loglik
#Test for just beta1=0
#Chi-squared statistic, critical value, and p-value
CSstat=-2*(-93.18427-(-85.00842))
CV=qchisq(.95,df=1)
pvalue1=pchisq(CSstat,df = 1,lower.tail = FALSE)
#Test for beta2=0 with TR in the model
CSstat=-2*(-85.00842-(-69.82810))
CV=qchisq(.95,df=1)
pvalue2=pchisq(CSstat,df = 1,lower.tail = FALSE)
#Test for beta3=0 (interaction) with TR and logWBC in the model
CSstat=-2*(-69.82810-(-69.64839))
CV=qchisq(.95,df=1)
pvalue3=pchisq(CSstat,df = 1,lower.tail = FALSE)
#Test for beta2=beta3=0 with TR in the model
CSstat=-2*(-85.00842-(-69.64839))
CV=qchisq(.95,df=2)
pvalue4=pchisq(CSstat,df = 2,lower.tail = FALSE)