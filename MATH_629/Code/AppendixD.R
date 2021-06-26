library(survival)
rm(list=ls())
setwd(file.path("C:","GitStuff","Survival_Analysis","MATH_629","Data"))
load("addicts.rda")
#Create a "survival object" 
Y<-Surv(addicts$survt,addicts$status==1)
#Create a model with intercept only and no partitioning 
kmfit1=survfit(Y~1)
#See your Kaplan Meirer survival curves 
summary(kmfit1)
#summary(survfit(Surv(addicts$survt,addicts$status==1)~1))
#way to get particular estimate at time or times
summary(kmfit1,times=367)
summary(kmfit1,times=c(200,250))
#statify on clinic and get the Kaplain-Meirer curves 
kmfit2=survfit(Y~addicts$clinic)
summary(kmfit2)
summary(kmfit2,100*(0:10))
#Plot survival curves      
plot(kmfit1)
plot(kmfit2)
windows(width=10, height=8)
plot(kmfit2,lty=c('solid','dashed'),col=c('black','grey'),
     xlab="survival time in days",ylab="survival probabilities")
legend("topright",c("Clinic 1","Clinic2"),lty=c("solid","dashed"),
       col=c('black','grey'))
#Different way to conduct a log rank test. 
survdiff(Y~addicts$clinic)
survdiff(Surv(survt,status)~clinic,data=addicts)
#set the addicts data as the default with the attach function 
attach(addicts)
survdiff(Surv(survt,status)~clinic)
#weight the survival times by s(t_j)^rho where t_j is the event time
survdiff(Surv(survt,status)~clinic,rho=1)
#stratified log rank test (by prison)
survdiff(Y~clinic+strata(prison)) 
#Plot a log-log plot with time in the log scale
plot(kmfit2,fun="cloglog",xlab="time in days using logarithmic scale",ylab="log-log survival",main="log-log curves by clinic")
#Creating a log-log plot against time. 
kmfit3=summary(kmfit2); names(kmfit3); attach(kmfit3)
kmfit4=data.frame(strata,time,surv); names(kmfit4)=c("clinic","time","survival")
clinic1=kmfit4[kmfit4$clinic=="addicts$clinic=1",];clinic2=kmfit4[kmfit4$clinic=="addicts$clinic=2",]
plot(clinic1$time,log(-log(clinic1$survival)),xlab="survival time in days",ylab=
       "log-log survival",xlim=c(0,800),col="black",type='l',lty="solid",main="log-log
     curves by clinic")
par(new=T)
#Create a Cox Proportional Hazards Model and get additional info using summary
coxph(Y~prison+dose+clinic,data=addicts)
summary(coxph(Y~prison+dose+clinic,data=addicts))
#Choosing method to handle ties (method can be efron, breslow, exact)
#R uses efron
coxph(Y~prison+dose+clinic,data=addicts,method='efron')
#no interaction and interaction models
mod1=coxph(Y~prison+dose+clinic,data=addicts)
mod2=coxph(Y~prison+dose+clinic+clinic*prison+clinic*dose,data=addicts)
#A log likelihood test for interaction at alpha=0.05
LRT=-2*(mod1$loglik[2]-mod2$loglik[2])
Pvalue=1-pchisq(LRT,2)
alpha=0.05
Pvalue<=alpha
#Create a function for log ration test p-value and apply it
lrt.surv=function(mod.full,mod.reduced,df){
  lrts=-2*(mod.full$loglik[2]-mod.reduced$loglik[2])
  pvalue=1-pchisq(lrts,df)
  return(pvalue)
}
lrt.surv(mod1,mod2,2)
#Stratified Cox Model on clinic wout/interaction and w/interaction
coxph(Y~prison+dose+strata(clinic),data=addicts)
coxph(Y~prison+dose+clinic:prison+clinic:dose+strata(clinic),data=addicts)
#Recode Clinic to get affect of prison when clinic=2
addicts$clinic2=addicts$clinic-2
summary(coxph(Y~prison+dose+clinic2:prison+clinic2:dose+strata(clinic2)
              ,data=addicts))
#Assessing the PH assumption with Schoenfeld residuals 
mod1=coxph(Y~prison+dose+clinic,data=addicts)
cox.zph(mod1,transform=rank)
#Plot the Schoenfeld residuals against time with respect to Clinic. se=F 
#supresses the confidence intervals 
plot(cox.zph(mod1,transform=rank),se=F,var='clinic')
#Obtaining Cox-adjusted survival curves
mod1=coxph(Y~prison+dose+clinic,data=addicts)
pattern1=data.frame(prison=0,dose=70,clinic=2)
summary(survfit(mod1,newdata=pattern1))
plot(survfit(mod1,newdata=pattern1),conf.int=T,main="Adjusted 
     curves for prison=0,dose=70,clinic=2")
#Obtaining Stratified adjusted survival curves
mod3=coxph(Y~prison+dose+strata(clinic),data=addicts)
pattern2=data.frame(prison=0.46,dose=60.40)
windows(width=10, height=8)
plot(survfit(mod3,newdata=pattern2),conf.int=F,lty=c("solid","dashed"), 
col=c("black","grey"), main="Survival curves for clinic, adjusted for prison
and dose") 
legend('topright',c("clinic 1","clinic 2"),lty=c('solid','dashed'), 
       col=c('black','grey'))  
#plot log-log curves adjusted for prison and dose with log(time) on horizontal
#axis
plot(survfit(mod3,newdata=pattern2),fun="cloglog",main="Log-log curves for 
     clinic, adjusted for prison and dose") 
#plot log-log curves adjusted for prison and dose with time on horizontal
#axis
sum.mod3=summary(survfit(mod3,newdata=pattern2))
sum.mod4=data.frame(sum.mod3$strata,sum.mod3$time,sum.mod3$surv)
colnames(sum.mod4)=c("clinic","time","survival")
clinic1=sum.mod4[sum.mod4$clinic=='clinic=1',]
clinic2=sum.mod4[sum.mod4$clinic=='clinic=2',]
windows(width=10, height=8)
plot(clinic1$time,log(-log(clinic1$survival)),xlab="survival
time in days",ylab='log-log survival',xlim=c(0,800),col="black",
type='l',lty='solid',main='log-log curves stratified by clinic, adjusted for 
prison,dose')
par(new=T)
plot(clinic2$time,log(-log(clinic2$survival)),axes=F,xlab="survival 
     time in days",ylab="log-log survival",col="grey50",type='l',
     lty='dashed') 
legend('bottomright',c('clinic 1','clinic 2'),lty=c('solid','dashed'),
       col=c("black",'grey'))
par(new=F)
#Putting the addicts data set in counting process form so that an 
#extended Cox model can be run
addicts.cp=survSplit(addicts,cut=addicts$survt[addicts$status==1],
  end='survt',event='status',start='start')
#Create a new variable dose times logof survival time
addicts.cp$logtdose=addicts.cp$dose*log(addicts.cp$survt)
#Create a time dependent CoxPH model with time dependent covariate logtdoes
#Print observations of addicts.cp
addicts.cp[addicts.cp$id==1,]
coxph(Surv(addicts.cp$start,addicts.cp$survt,addicts.cp$status)~prison+dose+clinic+logtdose+cluster(id),data=addicts.cp)
#Create a time dependent CoxPH model with time dependent covariate using heaviside functions
addicts.cp365=survSplit(addicts,cut=365,end='survt',event='status',start='start')
addicts.cp365$hv1=addicts.cp365$clinic*(addicts.cp365$start<365)
addicts.cp365$hv2=addicts.cp365$clinic*(addicts.cp365$start>=365)
addicts.cp365=addicts.cp365[order(addicts.cp365$id,addicts.cp365$start),]
Y365=Surv(addicts.cp365$start,addicts.cp365$survt,addicts.cp365$status)
coxph(Y365~prison+dose+hv1+hv2+cluster(id),data=addicts.cp365,method='breslow')
coxph(Y365~prison+dose+clinic+hv2+cluster(id),data=addicts.cp365,method='breslow')
#Parametric models - test PH assumption of Weibull model 
plot(survfit(Y~addicts$clinic),fun='cloglog',xlab='time in days using logarithmic scale',ylab='log-
     log survival',main='log-log curves by clinic')
#Fit an exponential model
modpar1=survreg(Surv(addicts$survt,addicts$status)~prison+dose+clinic,data=addicts,dist="exponential")