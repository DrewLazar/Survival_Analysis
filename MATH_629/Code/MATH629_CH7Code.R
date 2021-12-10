#Chapter 7
#Clean up
rm(list=ls())
#load the necessary packages 
library(survival)
library(dplyr)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
#7.1
#Create a Survival object 
Y<-Surv(Remission$survt,Remission$status)
#7.1.i.  
mod.exp1=survreg(Y~TR,data=Remission,dist="exponential")
summary(mod.exp1)
alpha0=as.vector(mod.exp1$coefficients[1]); alpha1=as.vector(mod.exp1$coefficients[2])
#estimate AF
AF=exp(alpha1)
#95% CI for AF
lb.af = exp(alpha1 - 1.96*.398)
ub.af = exp(alpha1 + 1.96*.398)
#7.1.ii. 
beta0 = -alpha0; beta1 = -alpha1;
#estimate HR
HR=exp(beta1)
#95% CI for HR
lb.hr = 1/ub.af; ub.hr = 1/lb.af
#7.2.i 
mod.exp2=survreg(Y~TR+logWBC,data=Remission,dist="exponential")
summary(mod.exp2)
alpha0=as.vector(mod.exp2$coefficients[1]); alpha1=as.vector(mod.exp2$coefficients[2])
alpha2=as.vector(mod.exp2$coefficients[3])
#estimate AF
AF=exp(alpha1)
#95% CI for AF
lb.af = exp(alpha1 - 1.96*0.413)
ub.af = exp(alpha1 + 1.96*0.413)
#7.2.ii. 
beta0 = -alpha0; beta1 = -alpha1; beta2=-alpha2 
#estimate HR
HR=exp(beta1)
#95% CI for HR
lb.hr = 1/ub.af; ub.hr = 1/lb.af
#7.2.iii. 
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
#takes the log of the negative log (doesn't use another negative).
windows(width=10, height=8)
plot(kmfitTR2,fun="cloglog",xlab="time in weeks on log scale",ylab="log-log survival", col=c('blue','green'), main="log-log curves by treatment")
legend("topleft",c("Treatment","Placebo"),lty=('solid'),col=c('green','blue'))
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
windows(width=10, height=8)
pattern1=data.frame(TR=0)
pct2=0:1000/1000
weeks1=predict(mod.wbl1,newdata=pattern1,
              type="quantile",p=pct2)
survival=1-pct2
plot(weeks1,survival,xlab="survival time in weeks",ylab= "survival
     probabilities",main="Weibull survival estimates for TR=0 vs. TR=1",xlim=c(0,70))
par(new=TRUE)
#For TR=1
pattern2=data.frame(TR=1)
pct2=0:1000/1000
weeks2=predict(mod.wbl1,newdata=pattern2,
              type="quantile",p=pct2)
survival=1-pct2
plot(weeks2,survival,xlab="survival time in weeks",ylab= "survival
     probabilities",xlim=c(0,70))

#7.2.5
#For TR=0
pct=c(.25,.50,.75)
weeks1=predict(mod.wbl1,newdata=pattern1,type="quantile",p=pct)
cbind(pct,weeks1)
#For TR=1 
pct=c(.25,.50,.75)
weeks2=predict(mod.wbl1,newdata=pattern2,type="quantile",p=pct)
cbind(pct,weeks2)

#7.2.6
Coxph.TR=coxph(Y~TR,data=Remission)
summary(Coxph.TR)



#7.3 
load("bears.rda")
B<-Surv(bears$time,bears$event)
kmfit2=survfit(B~bears$iron.group)
#7.3.1
kmfit2=survfit(B~bears$iron.group)
windows(width=10, height=8)
plot(kmfit2,lty=c('solid'),col=c('blue','red','green'),
     xlab="survival time in years",ylab="survival probabilities",main=
       "KM Survival Curves by Iron.group")
legend("topright",c("Iron (low)","Iron (med)", "Iron (high)"),lty=c("solid"),
       col=c('blue','red','green'))
survdiff(B~bears$iron.group)
#7.3.2. 
windows(width=10, height=8)
plot(log(kmfit2$time),log(kmfit2$surv/(1-kmfit2$surv)),xlab="ln(time)",
     ylab="ln[(1-S(t))/S(t))]",main="ln(t) vs Failure Odds by Iron.group")
#7.3.3
mod.bears.lgl=survreg(B ~ iron,data=bears,dist="loglogistic")
summary(mod.bears.lgl)
alpha0=as.vector(mod.bears.lgl$coefficients[1]); alpha1=as.vector(mod.bears.lgl$coefficients[2]);
AF=exp(alpha1)
#95% CI for AF 
lb.af = exp(alpha1 - 1.96*0.00425)
ub.af = exp(alpha1 + 1.96*0.00425)
#7.3.4
#shape parameter
p=1/mod.bears.lgl$scale
#PO Model 
beta0=-p*alpha0; beta1 = -p*alpha1
FOR=exp(beta1);
#checking our formula for the FOR
round(FOR,4)==round((1/AF)^p,4)
#95% CI for FOR
lb.for = (1/ub.af)^p 
ub.for = (1/lb.af)^p
#7.3.5 
bears %>%
  group_by(iron.group) %>%
  summarise(mean_iron = mean(iron),
            number_obs = n())
#for iron=114
windows(width=10, height=8)
pattern1=data.frame(iron=114)
pct2=0:1000/1000
years1=predict(mod.bears.lgl,newdata=pattern1,
              type="quantile",p=pct2)
survival=1-pct2
plot(years1,survival,xlab="survival time in years",ylab= "survival
     probabilities",main="log-logistic survival estimates for iron=114, 124, 131",xlim=c(0,170))
par(new=TRUE)
#for iron=124
pattern2=data.frame(iron=124)
pct2=0:1000/1000
years2=predict(mod.bears.lgl,newdata=pattern2,
              type="quantile",p=pct2)
plot(years2,survival,xlab="survival time in days",ylab= "survival
     probabilities",xlim=c(0,170))
par(new=TRUE)
#for iron=131
pattern3=data.frame(iron=131)
pct2=0:1000/1000
years3=predict(mod.bears.lgl,newdata=pattern3,
              type="quantile",p=pct2)
plot(years3,survival,xlab="survival time in days",ylab= "survival
     probabilities",xlim=c(0,170))





