#Chapter 7
#Clean up
rm(list=ls())
#load the necessary packages 
library(survival)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
#7.1
#Create a Survival object 
Y<-Surv(Remission$survt,Remission$status)
#1.i.  
mod.exp1=survreg(Y~TR,data=Remission,dist="exponential")
summary(mod.exp1)
alpha0=as.vector(mod.exp1$coefficients[1]); alpha1=as.vector(mod.exp1$coefficients[2])
#estimate AF
AF=exp(alpha1)
#95% CI for AF
lb.af = exp(alpha1 - 1.96*.398)
ub.af = exp(alpha1 + 1.96*.398)
#1.ii. 
beta0 = -alpha0; beta1 = -alpha1;
#estimate HR
HR=exp(beta1)
#95% CI for HR
lb.hr = 1/ub.af; ub.hr = 1/lb.af
#2.i 
mod.exp2=survreg(Y~TR+logWBC,data=Remission,dist="exponential")
summary(mod.exp2)
alpha0=as.vector(mod.exp2$coefficients[1]); alpha1=as.vector(mod.exp2$coefficients[2])
alpha2=as.vector(mod.exp2$coefficients[3])
#estimate AF
AF=exp(alpha1)
#95% CI for AF
lb.af = exp(alpha1 - 1.96*0.413)
ub.af = exp(alpha1 + 1.96*0.413)
#2.ii. 
beta0 = -alpha0; beta1 = -alpha1; beta2=-alpha2 
#estimate HR
HR=exp(beta1)
#95% CI for HR
lb.hr = 1/ub.af; ub.hr = 1/lb.af
#2.iii. 
mod.int.exp2=survreg(Y~TR+logWBC+logWBC:TR,data=Remission,dist="exponential")
summary(mod.int.exp2)
#likelihood ratio test for significance of interaction
CSstat = -2*(mod.exp2$loglik[2]-mod.int.exp2$loglik[2])
CV = qchisq(.95,df=1)
CSstat>CV
pvalue=pchisq(CSstat,df = 1,lower.tail = FALSE)
#2.iv. 
alpha0=as.vector(mod.int.exp2$coefficients[1]); alpha1=as.vector(mod.int.exp2$coefficients[2])
alpha2=as.vector(mod.int.exp2$coefficients[3]); alpha3=as.vector(mod.int.exp2$coefficients[4])
Q1 = as.vector(quantile(Remission$logWBC)[2]); Q2 = as.vector(quantile(Remission$logWBC)[3]) 
Q3 = as.vector(quantile(Remission$logWBC)[4])
AF1 = exp(alpha1 + Q1*alpha3); AF2 = exp(alpha1 + Q2*alpha3); AF3 = exp(alpha1 + Q3*alpha3)
HR1 = exp(-alpha1 - Q1*alpha3); HR2 = exp(-alpha1 - Q2*alpha3); HR3 = exp(-alpha1 - Q3*alpha3)
#Problem 7.2 
#7.2.1 
#Create a Survival Object 
Y<-Surv(Remission$survt,Remission$status==1)
#Fit KM curves against TR
kmfitTR2<-survfit(Y~TR,data=Remission)
#Plot log(-log) survival curves against survt - note this function
#takes the log of the negative log (doesn't use another negative). Also the survt
#is plotted on a log scale. 
windows(width=10, height=8)
plot(kmfitTR2,fun="cloglog",xlab="time in weeks on log scale",ylab="log-log survival", col=c('blue','green'), main="log-log curves by treatment")
#7.2.2. 
mod.wbl1=survreg(Y ~ TR,data=Remission,dist="weibull")
summary(mod.wbl1)
alpha0=as.vector(mod.wbl1$coefficients[1]); alpha1=as.vector(mod.wbl1$coefficients[2]);
#estimate AF
AF=exp(alpha1)
#95% CI for AF 
lb.af = exp(alpha1 - 1.96*0.311)
ub.af = exp(alpha1 + 1.96*0.311)
#7.2.3
#shape parameter
p=1/mod.wbl1$scale
beta0=-p*alpha0; beta1 = -p*alpha1
HR=exp(beta1);
#checking our formula for the HR
(1/AF)^p
#95% CI for HR
lb.hr = (1/ub.af)^p; 
ub.hr = (1/lb.af)^p; 
#7.2.4
#For TR=0
pattern1=data.frame(TR=0)
pct2=0:1000/1000
days2=predict(modpar2,newdata=pattern1,
              type="quantile",p=pct2)
survival=1-pct2
plot(days2,survival,xlab="survival time in days",ylab= "survival
     probabilities",main="Weibull survival estimates for TR=0",xlim=c(0,70))

#For TR=1
pattern1=data.frame(TR=1)
pct2=0:1000/1000
days2=predict(modpar2,newdata=pattern1,
              type="quantile",p=pct2)
survival=1-pct2
plot(days2,survival,xlab="survival time in days",ylab= "survival
     probabilities",main="Weibull survival estimates for TR=0",xlim=c(0,70))

#7.2.5
#For TR=0
pct=c(.25,.50,.75)
days=predict(mod.wbl1,newdata=pattern1,type="quantile",p=pct)
cbind(pct,days)
#For TR=1 

pct=c(.25,.50,.75)
days=predict(mod.wbl1,newdata=pattern1,type="quantile",p=pct)
cbind(pct,days)



            