rm(list=ls())
library(survival)
library(dplyr)
#1
#i
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
Ven.reset <-read.csv("Venreset.csv", header = TRUE)
#ii
Y<-Surv(Ven.reset$eventtime,Ven.reset$status==1)
#log-log curves for Setting 
kmfitST3=survfit(Y~Ven.reset$Setting)
windows(width=10, height=8)
plot(kmfitST3,fun="cloglog",xlab="time in days on log scale",ylab="log-log survival", main="log-log curves by Setting")
#log-log curves for LO2 
quantile(Ven.reset$LO2)
Ven.reset$LO2.group<-cut(Ven.reset$LO2,c(-2.58,1.0075,2.0350,3.0825,6.2300),labels=c('1','2','3','4'))
length(unique(Ven.reset$LO2.group))
kmfitO24=survfit(Y~Ven.reset$LO2.group)
windows(width=10, height=8)
plot(kmfitO24,fun="cloglog",xlab="time in days on log scale",ylab="log-log survival", main="log-log curves by LO2group")
#Stratify Data set by Setting 
Venreset0<-Ven.reset[Ven.reset$Setting==0, ]
Venreset1<-Ven.reset[Ven.reset$Setting==1, ]
Venreset2<-Ven.reset[Ven.reset$Setting==2, ]
#Create Survival objects for both Strata
Y0<-Surv(Venreset0$eventtime,Venreset0$status==1)
Y1<-Surv(Venreset1$eventtime,Venreset1$status==1)
Y2<-Surv(Venreset2$eventtime,Venreset2$status==1)
#Fit Cox PH models to three strata
Coxph.Ven.m0=coxph(Y0~LO2,data=Venreset0)
Coxph.Ven.m1=coxph(Y1~LO2,data=Venreset1)
Coxph.Ven.m2=coxph(Y0~LO2,data=Venreset2)
#get the overall mean of LO2
meanlo2=mean(Ven.reset$LO2)
#plot our adjusted survival curves 
windows(width=10, height=8)
pattern=data.frame(LO2=meanlo2)
plot(survfit(Coxph.Ven.m0,newdata=pattern),xlim=c(0.08,23),ylim=c(-6,3.7),fun="cloglog",conf.int=F,main="Adjusted survival for TR=0 vs TR=1, mean(logWBC)",col=c('blue'))
par(new=TRUE)
plot(survfit(Coxph.Ven.m1,newdata=pattern),xlim=c(0.08,23),ylim=c(-6,3.7),fun="cloglog",conf.int=F,col=('green'))
par(new=TRUE)
plot(survfit(Coxph.Ven.m2,newdata=pattern),fun="cloglog",xlim=c(0.08,23),ylim=c(-6,3.7),conf.int=F,col=('red'))
legend("topright",c("Treatment","Placebo"),lty=('solid'),col=c('green','blue'))
#Observed vs. Expected Plots
#Observed vs. Expected Plots for Setting 
windows(width=10, height=8)
plot(kmfitST3,xlab="time in days",ylab="Survival Probabilities", col=c('blue','blue','blue'),lty=c('solid','dashed','dotted'), main="Observed vs Expected curves by treatment")
Coxph.Ven.Setting=coxph(Y~Setting,data=Ven.reset)
pattern1=data.frame(Setting=0)
pattern2=data.frame(Setting=1)
pattern3=data.frame(Setting=2)
par(new=TRUE)
plot(survfit(Coxph.Rem.Setting,newdata=pattern1),conf.int=F,col=c('green'),lty=c('solid'))
par(new=TRUE)
plot(survfit(Coxph.Rem.Setting,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Rem.Setting,newdata=pattern3),conf.int=F,col=c('green'),lty=c('dotted'))
#Observed vs. Expected Plots for LO2 
windows(width=10, height=8)
plot(kmfitO24,xlab="time in weeks on log scale",ylab="Survival Probabilities", col=c('blue','green','red'), main="Observed vs. Expected by level of LogWBC")
Ven.reset %>%
  group_by(LO2.group) %>%
  summarise(mean_LO2 = mean(LO2),
            number_obs = n())
Coxph.Ven.LO2=coxph(Y~LO2,data=Ven.reset)
pattern1=data.frame(LO2=-0.421)
pattern2=data.frame(LO2=1.45)
pattern3=data.frame(LO2=2.44)
pattern4=data.frame(LO2=4.01)
par(new=TRUE)
plot(survfit(Coxph.Ven.LO2,newdata=pattern1),conf.int=F,col=c('blue'),lty=c('dashed'))
par(new=TRUE)  
plot(survfit(Coxph.Ven.LO2,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Ven.LO2,newdata=pattern3),conf.int=F,col=c('red'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Ven.LO2,newdata=pattern4),conf.int=F,col=c('red'),lty=c('dashed'))
legend("topright",c("Low LogWBC (observed)","Medium LogWBC (observed)","High LogWBC (observed)","Low LogWBC (expected)","Medium LogWBC (expected)","High LogWBC (expected)"),lty=c("solid","solid","solid","dashed","dashed","dashed"),
       col=c('blue','green','red','blue','green','red'))

#Using "dummy variables" 
windows(width=10, height=8)
plot(kmfitO24,xlab="time in weeks on log scale",ylab="Survival Probabilities", col=c('blue','green','red'), main="Observed vs. Expected by level of LogWBC")
#Create dummy variables 
Ven.reset$X1<-rep(0,nrow(Ven.reset));Ven.reset$X2<-rep(0,nrow(Ven.reset))
Ven.reset$X3<-rep(0,nrow(Ven.reset))
for (i in 1:nrow(Ven.reset))
{
  if (Ven.reset$LO2.group[i] == 2) {
    Ven.reset$X1[i]=1
  } else if (Ven.reset$LO2.group[i]==3) {
    Ven.reset$X2[i]=1 
  } else if (Ven.reset$LO2.group[i]==4) {
    Ven.reset$X3[i]=1 
  }
}
Coxph.Ven.L02.d=coxph(Y~X1+X2+X3,data=Ven.reset)
pattern1=data.frame(X1=0,X2=0,X3=0)
pattern2=data.frame(X1=1,X2=0,X3=0)
pattern3=data.frame(X1=0,X2=1,X3=0)
pattern4=data.frame(X1=0,X2=0,X3=1)
par(new=TRUE)
plot(survfit(Coxph.Ven.L02.d,newdata=pattern1),conf.int=F,col=c('blue'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Ven.L02.d,newdata=pattern2),conf.int=F,col=c('green'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Ven.L02.d,newdata=pattern3),conf.int=F,col=c('red'),lty=c('dashed'))
par(new=TRUE)
plot(survfit(Coxph.Ven.L02.d,newdata=pattern4),conf.int=F,col=c('red'),lty=c('dashed'))
legend("topright",c("Low LogWBC (observed)","Medium LogWBC (observed)","High LogWBC (observed)","Low LogWBC (expected)","Medium LogWBC (expected)","High LogWBC (expected)"),lty=c("solid","solid","solid","dashed","dashed","dashed"),
       col=c('blue','green','red','blue','green','red'))
#log rank test for difference in parameters.
kmfitO24=survfit(Y~Ven.reset$LO2.group)
Coxph.Ven.L02.d=coxph(Y~X1+X2+X3,data=Ven.reset)
Coxph.Ven.LO2=coxph(Y~LO2,data=Ven.reset)
Coxph.Ven.LO2$loglik
Coxph.Ven.L02.d$loglik
qchisq(.025, 2, lower.tail=FALSE)
pchisq(23.7432, 2, lower.tail = FALSE)
plot(kmfitO23,fun="cloglog",xlab="time in days on log scale",ylab="log-log survival", main="log-log curves by LO2group")
#Schoenfeld Residuals
mod1=coxph(Y~Setting, data=Ven.reset)
mod2=coxph(Y~LO2,data=Ven.reset)
mod3=coxph(Y~Setting+LO2, data=Ven.reset)
cox.zph(mod1,transform=rank)
cox.zph(mod2,transform=rank)
cox.zph(mod3,transform=rank)
#Assessing the PH assumption using time dependent covariates. 
Ven.Reset.cp=survSplit(Ven.reset,cut=Ven.reset$status[Ven.reset$status==1],
                       end="eventtime", event="status",start="start",id="id")
Ven.Reset.cp$tLO2=Ven.Reset.cp$LO2(Ven.Reset.cp$eventtime)^2

#Inspect Remission.cp for one individual 
Ven.Reset.cp[Ven.Reset.cp$id==30,]
#Run an extended CoxPH model for TR, logWBC and Sex one-at-a-time with 
#g(t)=log(t)
#Create an extended survival object 
YE<-Surv(Ven.Reset.cp$start,Ven.Reset.cp$eventtime,Ven.Reset.cp$status)
#Test for TR
coxph(YE ~ LO2 + tLO2+cluster(id),data=Ven.Reset.cp)

