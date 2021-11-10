#Chapter 6
#Clean up
rm(list=ls())
#load the necessary packages 
library(survival)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Addicts data 
load("addicts.rda")
addicts=addicts[,2:6]
n=nrow(addicts)
for (i in 1:n)
if (addicts$clinic[i]==2) {
  addicts$clinic[i]=0
}
#Problem 6.2
#Create a Survival Object 
Y<-Surv(addicts$survt,addicts$status==1)
#Problem 6.1 
#1. 
Coxph.addicts3=coxph(Y~prison+dose+clinic,data=addicts)
summary(Coxph.addicts)
cox.zph(Coxph.addicts,transform=rank)
#2. 
windows(width=10, height=8)
prisonmean=mean(addicts$prison); dosemean=mean(addicts$dose); 
pattern1=data.frame(prison=prisonmean,dose=dosemean,clinic=0);
pattern2=data.frame(prison=prisonmean,dose=dosemean,clinic=1);
plot(survfit(Coxph.addicts3,newdata=pattern1),conf.int=F,main="Adjusted survival for clinic=0 vs clinic=1, mean(prison), mean(dose)",col=c("blue"))
par(new=TRUE)
plot(survfit(Coxph.addicts3,newdata=pattern2),conf.int=F,col=c("red"))
legend("topright",c("Clinic=2","Clinic=1"), lty=c("solid"),col=c('blue','red'))
#3 
addicts.cp365=survSplit(addicts,cut=365,end="survt",event="status",start="start",id="id")
addicts.cp365$hv2=addicts.cp365$clinic*(addicts.cp365$start>=365)
addicts.cp365=addicts.cp365[order(addicts.cp365$id,addicts.cp365$start), ]
Y365=Surv(addicts.cp365$start,addicts.cp365$survt,addicts.cp365$status)
coxph.addicts.hs2<-coxph(Y365 ~ prison + dose + clinic + hv2 + cluster(id),data=addicts.cp365)
summary(coxph.clinic.hs2)
#estimated HR before time 365
exp(0.459373)
#estimated HR after time 365
exp(0.459373+1.371144)
#Equivalent model with two heaviside functions
addicts.cp365$hv1=addicts.cp365$clinic*(addicts.cp365$start<365)
coxph.addicts.hs1<-coxph(Y365 ~ prison + dose + hv1 + hv2 + cluster(id),data=addicts.cp365)
#4
addicts.cp=survSplit(addicts,cut=addicts$survt[addicts$status==1],end="survt", event="status",start="start",id="id")
addicts.cp$t.clinic=addicts.cp$clinic*addicts.cp$survt
#Create an extended survival object 
YE<-Surv(addicts.cp$start,addicts.cp$survt,addicts.cp$status)
#Test for TR
coxph.addicts.ct<-coxph(YE ~ prison + dose + clinic + t.clinic + cluster(id),data=addicts.cp)
summary(coxph.addicts.ct)
#estimated HR at Q1, Q2, Q3 
quantile(addicts$survt)
#estimated HR of clinic at Q1
exp(-0.0193951+171.25*0.0030207)
#estimated HR of clinic at Q2
exp(-0.0193951+367.50*0.0030207)
#estimated HR of clinic at Q3
exp(-0.0193951+585.50*0.0030207)