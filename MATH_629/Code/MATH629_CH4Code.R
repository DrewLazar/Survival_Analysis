#Chapter 4
#Clean up
rm(list=ls())
#load the necessary packages 
library(dplyr); library(survival); library(crone)
#load the R Survival package 
#Remove any variables in your environment 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
#Problem 4.1
#Create a Survival Object 
Y<-Surv(Remission$survt,Remission$status==1)
#Fit KM curves against TR
kmfitTR2<-survfit(Y~TR,data=Remission)
#Plot log(-log) survival curves against survt - note this function
#takes the log of the negative log (doesn't use another negative). Also the survt
#is plotted on a log scale. 
windows(width=10, height=8)
plot(kmfitTR2,fun="cloglog",xlab="time in weeks on log scale",ylab="log-log survival", col=c('blue','green'), main="log-log curves by treatment")
legend("topright",c("Treatment","Placebo"),lty=c("solid"), col=c('blue','green'))
#Fit KM curves against logWBC.group 
kmfitLWBC3<-survfit(Y~logWBC.group,data=Remission)
#Plot log(-log) survival curves against survt (surv has log) 
windows(width=10, height=8)
plot(kmfitLWBC3,fun="cloglog",xlab="time in weeks on log scale",ylab="log-log survival", col=c('blue','green','red'), main="log-log curves by level of logWBC")
legend("topright",c("Low LogWBC","Medium LogWBC","High LogWBC" ),lty=c("solid"),
       col=c('blue','green','red'))
#Fit KM curves against logWBC.group 
kmfitS2<-survfit(Y~Sex,data=Remission)
#Plot log(-log) survival curves against Sex
summary(kmfitS2)
windows(width=10, height=8)
plot(kmfitS2,fun="cloglog",xlab="time in weeks on log scale",ylab="log-log survival", col=c('blue','green','red'), main="log-log curves by Sex")
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
#get the overall mean of logWBC
meanlWBC=mean(Remission$logWBC)
#plot our adjusted survival curves 
windows(width=10, height=8)
pattern=data.frame(logWBC=meanlWBC)
plot(survfit(Coxph.Rem.m1,newdata=pattern),fun="cloglog",conf.int=F,xlim=c(1,23),ylim=c(-3.9,2.3),main="Adjusted survival for TR=0 vs TR=1, mean(logWBC)",col=c('blue'))
par(new=TRUE)
plot(survfit(Coxph.Rem.m0,newdata=pattern),fun="cloglog",conf.int=F,col=('green'),xlim=c(1,23),ylim=c(-3.9,2.3))
legend("topright",c("Treatment","Placebo"),lty=('solid'),col=c('green','blue'))
#Problem 4.3
#Observed vs Expected for TR
windows(width=10, height=8)
plot(kmfitTR2,xlab="time in weeks",ylab="Survival Probabilities", col=c('blue','blue'),lty=c('solid','dashed'), main="Observed vs Expected curves by treatment")
Coxph.Rem.TR=coxph(Y~TR,data=Remission)
pattern1=data.frame(TR=0)
pattern2=data.frame(TR=1)
par(new=TRUE)
plot(survfit(Coxph.Rem.TR,newdata=pattern1),conf.int=F,col=c('green'))
par(new=TRUE)
plot(survfit(Coxph.Rem.TR,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
legend("topright",c("Treatment (observed)","Placebo (observed)","Treatment (expected)","Placebo (expected)"),lty=c("solid","dashed","solid","dashed"),
       col=c('blue','blue','green','green'))
#Observed vs Expected for Sex
windows(width=10, height=8)
plot(kmfitS2,xlab="time in weeks",ylab="Survival Probabilities", col=c('blue','blue'),lty=c('solid','dashed'), main="Observed vs Expected curves by treatment")
Coxph.Rem.Sex=coxph(Y~Sex,data=Remission)
pattern1=data.frame(Sex=0)
pattern2=data.frame(Sex=1)
par(new=TRUE)
plot(survfit(Coxph.Rem.Sex,newdata=pattern1),conf.int=F,col=c('green'))
par(new=TRUE)
plot(survfit(Coxph.Rem.Sex,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
legend("topright",c("Male (observed)","Female (observed)","Male (expected)","Female (expected)"),lty=c("solid","dashed","solid","dashed"),
       col=c('blue','blue','green','green'))
#Observed vs Expected for LogWBC
#Using means for each category of LogWBC
windows(width=10, height=8)
plot(kmfitLWBC3,xlab="time in weeks on log scale",ylab="Survival Probabilities", col=c('blue','green','red'), main="Observed vs. Expected by level of LogWBC")
Remission %>%
  group_by(logWBC.group) %>%
  summarise(mean_logWBC = mean(logWBC),
            number_obs = n())
Coxph.Rem.LWBC=coxph(Y~logWBC,data=Remission)
pattern1=data.frame(logWBC=1.91)
pattern2=data.frame(logWBC=2.64)
pattern3=data.frame(logWBC=3.83)
par(new=TRUE)
plot(survfit(Coxph.Rem.LWBC,newdata=pattern1),conf.int=F,col=c('blue'),lty=c('dashed'))
par(new=TRUE)  
plot(survfit(Coxph.Rem.LWBC,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Rem.LWBC,newdata=pattern3),conf.int=F,col=c('red'),lty=c('dashed'))
legend("topright",c("Low LogWBC (observed)","Medium LogWBC (observed)","High LogWBC (observed)","Low LogWBC (expected)","Medium LogWBC (expected)","High LogWBC (expected)"),lty=c("solid","solid","solid","dashed","dashed","dashed"),
       col=c('blue','green','red','blue','green','red'))
#Using "dummy variables" 
windows(width=10, height=8)
plot(kmfitLWBC3,xlab="time in weeks on log scale",ylab="Survival Probabilities", col=c('blue','green','red'), main="Observed vs. Expected by level of LogWBC")
#Create dummy variables 
Remission$X1<-rep(0,nrow(Remission));Remission$X2<-rep(0,nrow(Remission))
for (i in 1:nrow(Remission))
{
  if (Remission$logWBC.group[i] == 2) {
    Remission$X1[i]=1
  } else if (Remission$logWBC.group[i]==3) {
    Remission$X2[i]=1 
  } 
}
Coxph.Rem.LWBC.d=coxph(Y~X1+X2,data=Remission)
pattern1=data.frame(X1=0,X2=0)
pattern2=data.frame(X1=1,X2=0)
pattern3=data.frame(X1=0,X2=1)
par(new=TRUE)
plot(survfit(Coxph.Rem.LWBC.d,newdata=pattern1),conf.int=F,col=c('blue'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Rem.LWBC.d,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Rem.LWBC.d,newdata=pattern3),conf.int=F,col=c('red'),lty=c('dashed'))
legend("topright",c("Low LogWBC (observed)","Medium LogWBC (observed)","High LogWBC (observed)","Low LogWBC (expected)","Medium LogWBC (expected)","High LogWBC (expected)"),lty=c("solid","solid","solid","dashed","dashed","dashed"),
       col=c('blue','green','red','blue','green','red'))
#Problem 4.4
mod1=coxph(Y~TR, data=Remission)
mod2=coxph(Y~TR+logWBC, data=Remission)
mod3=coxph(Y~TR+logWBC+Sex, data=Remission)
cox.zph(mod1,transform=rank)
cox.zph(mod2,transform=rank)
cox.zph(mod3,transform=rank)
#Plots of Schoenfeld residuals. The fitted line should be mostly horizontal 
#for the variable to meet the Cox PH assumption. 
windows(width=10, height=8)
plot(cox.zph(mod3,transform=rank),se=F,var='TR')
windows(width=10, height=8)
plot(cox.zph(mod3,transform=rank),se=F,var='logWBC')
windows(width=10, height=8)
plot(cox.zph(mod3,transform=rank),se=F,var='Sex')
#Problem 4.5
#Put the Remission data set in counting process format 
Remission.cp=survSplit(Remission,cut=Remission$survt[Remission$status==1],
                     end="survt", event="status",start="start",id="id")
Remission.cp$logtTR=Remission.cp$TR*log(Remission.cp$survt)
#Inspect Remission.cp for one individual 
Remission.cp[Remission.cp$id==30,]
#Run an extended CoxPH model for TR, logWBC and Sex one-at-a-time with 
#g(t)=log(t)
#Create an extended survival object 
YE<-Surv(Remission.cp$start,Remission.cp$survt,Remission.cp$status)
#Test for TR
coxph(YE ~ TR + logtTR+cluster(id),data=Remission.cp)

