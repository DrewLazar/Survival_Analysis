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
#1. 
Coxph.addicts3=coxph(Y~prison+dose+clinic,data=addicts)
summary(Coxph.addicts3)
cox.zph(Coxph.addicts3,transform=rank)
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
Y365=Surv(addicts.cp365$start,addicts.cp365$survt,addicts.cp365$status)
coxph.addicts.hs2<-coxph(Y365 ~ prison + dose + clinic + hv2 + cluster(id),data=addicts.cp365)
summary(coxph.addicts.hs2)
#estimated HR before time 365
exp(0.459373)
#estimated HR after time 365
exp(0.459373+1.371144)
#Equivalent model with two heaviside functions
addicts.cp365$hv1=addicts.cp365$clinic*(addicts.cp365$start<365)
coxph.addicts.hs1<-coxph(Y365 ~ prison + dose + hv1 + hv2 + cluster(id),data=addicts.cp365)
summary(coxph.addicts.hs1)
#4
addicts.cp=survSplit(addicts,cut=addicts$survt[addicts$status==1],end="survt", event="status",start="start",id="id")
addicts.cp$t.clinic=addicts.cp$clinic*addicts.cp$survt
#Create an extended survival object 
YE<-Surv(addicts.cp$start,addicts.cp$survt,addicts.cp$status)
#Test for clinic
coxph.addicts.ct<-coxph(YE ~ prison + dose + clinic + t.clinic + cluster(id),data=addicts.cp)
summary(coxph.addicts.ct)
#estimated HR at Q1, Q2, Q3 
quantiles=quantile(addicts$survt)
Q1=quantiles[2];Q2=quantiles[3];Q3=quantiles[4]
#estimated HR of clinic at Q1=
exp(-0.0193951+Q1*0.0030207)
#estimated HR of clinic at Q2
exp(-0.0193951+Q2*0.0030207)
#estimated HR of clinic at Q3
exp(-0.0193951+Q3*0.0030207)
#Example 6.1
#HR Age=30 and TMS=0.5
exp(-3.1718+0.4442*.5+0.05552*30)
#HR Age=50 and TMS=2
exp(-3.1718+0.4442*2+0.05552*50)
#Example 6.2
name<-c("Barry","Garry","Susan","John");
time<-c(2,3,5,8);status<-c(1,0,1,1);Coupon<-c(1,1,0,1)
Sales=data.frame(name,Coupon,status,time)
Sales.cp=survSplit(Sales,cut=Sales$time[Sales$status==1],end="time", event="status",start="start")
Sales.cp$t.Coupon=Sales.cp$Coupon*Sales.cp$time
YE<-Surv(Sales.cp$start,Sales.cp$time,Sales.cp$status)
coxph(YE ~ Coupon + t.Coupon+cluster(name),data=Sales.cp)


