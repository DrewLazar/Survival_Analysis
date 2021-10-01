rm(list=ls())
library(survival)
#2a 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data") 
multmyl <-read.csv("multmylenoma.csv", header = TRUE)
Y<-Surv(multmyl$Survt,multmyl$Status==1)
#2c 
#Create a KM model partitioned by Clinic  
kmfit2=survfit(Y~Clinic,data=multmyl)
summary(kmfit2)
windows(width=10, height=8)
plot(kmfit2,lty=c('solid','solid'),col=c('black','blue'),
     xlab="survival time in months",ylab="survival probabilities")
legend("topright",c("Clinic 1","Clinic 2"),lty=c("solid","solid"),
       col=c('black','blue'))
#2e - computing chi-square values for the problem
critical.value=qchisq(.90, df=1);print(critical.value)
p.value=pchisq(3.189,1,lower.tail = FALSE);print(p.value)
#2f 
survdiff(Y~Clinic,data=multmyl)
#2g 
for (p in c(.4,1,2)){
  sd= survdiff(Y~Clinic,rho=p,data=multmyl)
  print(paste("The value of p is:",p)); print(sd)
}  
#2h 
survdiff(Surv(Survt,Status) ~ Clinic + strata(WS),data=multmyl)
#2i 
kmfitws=survfit(Y~WS,data=multmyl)
windows(width=10, height=8)
plot(kmfitws,col=c('black','blue'),
     xlab="survival time in months",ylab="survival probabilities")
legend("topright",c("WS=OW","WS=RW"),lty=c("solid","solid"),
       col=c('black','blue'))
survdiff(Y~WS,data=multmyl)
#2k 
kmfit2=survfit(Y~multmyl$Clinic,conf.type="plain")
windows(width=10, height=8)
par(mar = c(4,4,1,1))
plot(kmfit2[1], conf.int=TRUE, col="green", xlab = "Time in Months", ylab = "Survival probability")
lines(kmfit2[2], conf.int=TRUE, col="red")
legend("topright",c("Clinic 1","Clinic 2"),lty=c("solid"),
       col=c('green','red'))
#2l 
survfit(Y~multmyl$Clinic)
#for time t_4=6
0.5-1.96*0.0854 <0.8800 
0.5+1.96*0.0854 >0.8800 
#for time t_5=10
0.5-1.96*0.0998<0.5911
0.5+1.96*0.0998>0.5911
#for time t_6=12
0.5-1.96*0.1012<0.5489
0.5+1.96*0.1012>0.5489
#for time t_7=15
0.5-1.96*0.1026 <0.5031 
0.5+1.96*0.1026 >0.5031 
#for time t_8=16
0.5-1.96*0.1039<0.4528
0.5+1.96*0.1039>0.4528
#for time t_9=17
0.5-1.96*0.1039<0.4025
0.5+1.96*0.1039>0.4025
#for time t_10=18
0.5-1.96*0.0993<0.3019
0.5+1.96*0.0993>0.3019
#for time t_11=23
0.5-1.96*0.0890<0.1811 
0.5+1.96*0.0890> 0.1811 