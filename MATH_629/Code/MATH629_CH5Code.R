#Chapter 5
#Clean up
rm(list=ls())
#load the necessary packages 
library(survival)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
#Problem 5.1
#Create a Survival Object 
Y<-Surv(Remission$survt,Remission$status==1)
#Problem 5.1 
#fit our stratified Cox PH model
coxph.Rem.m1<-coxph(Y~ TR + logWBC + strata(Sex),data=Remission)
summary(coxph.Rem.m1)
#A log ratio test for the significance of TR and logWBC
coxph.Rem.m1$loglik
CSstat = -2*(coxph.Rem.m1$loglik[1]-coxph.Rem.m1$loglik[2])
CV = qchisq(.95,df=2)
CSstat>CV
pvalue1=pchisq(CSstat,df = 2,lower.tail = FALSE)
#A log ratio test for the significance of TR with logWBC in the model
#fit a stratified model with just logWBC
coxph.Rem.m2<-coxph(Y~ logWBC + strata(Sex),data=Remission)
coxph.Rem.m2$loglik
CSstat = -2*(coxph.Rem.m2$loglik[2]-coxph.Rem.m1$loglik[2])
CV = qchisq(.95,df=1)
CSstat>CV
pvalue2=pchisq(CSstat,df = 1,lower.tail = FALSE)
#Plot adjusted survival curves for RX from your stratified Cox PH model
mean.logWBC=mean(Remission$logWBC)
pattern1=data.frame(TR=0,logWBC=mean.logWBC)
windows(width=10, height=8)
plot(survfit(coxph.Rem.m1,newdata=pattern1),col=c('blue','red'),conf.int=F,main="Adjusted survival for TR=0 vs TR=1, mean(logWBC) from stratified Cox PH model")
pattern2=data.frame(TR=1,logWBC=2.930238)
par(new=TRUE)
plot(survfit(coxph.Rem.m1,newdata=pattern2),col=c('blue','red'), lty=c('dashed'),conf.int=F)
legend("topright",c("Treatment, Sex=F","Placebo, Sex=F","Treatment, Sex=M","Placebo, Sex=M" ),lty=c("solid","dashed","solid","dashed"),
       col=c('blue','blue','red','red'))
#Problem 5.2
#1
coxph.Rem.int.m1<-coxph(Y~TR+logWBC+Sex:TR+Sex:logWBC+strata(Sex),data=Remission)
summary(coxph.Rem.int.m1)
#2
CSstat = -2*(coxph.Rem.m1$loglik[2]-coxph.Rem.int.m1$loglik[2])
CV = qchisq(.95,df=2)
CSstat>CV
#2
#HR of TR and CI of HR of TR with with Sex=0 (given in R but we compute below)
b1=coxph.Rem.int.m1$coefficients[1]
exp(b1)
exp(b1-1.96*0.5636)
exp(b1+1.96*0.5636)
#HR of TR with Sex=1
b3=coxph.Rem.int.m1$coefficients[3]
exp(b1+b3)
#Trick to find HR and CI for HR of TR with Sex=1 
Remission$Sex2=Remission$Sex-1
coxph.Rem.int.m2<-coxph(Y~TR+logWBC+Sex2:TR+Sex2:logWBC+strata(Sex2),data=Remission)
summary(coxph.Rem.int.m2)
#CI of HR of TR with Sex=1
#HR is 7.227 which agrees with exp(b1+b2) above 
#CI is  1.6974 to 30.775 right from output. 
#Problem 5.3 
W<-Surv(vets$Survival.time,vets$Status)
Coxph.vets=coxph(W~Treatment+Cell.Type.1+Cell.Type.2+Cell.Type.3+Performance.status+Disease.duration+Age+Prior.Therapy,data=vets)
summary(Coxph.vets)
cox.zph(Coxph.vets,transform=rank)

