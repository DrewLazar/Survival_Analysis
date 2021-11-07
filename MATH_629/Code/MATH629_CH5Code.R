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
#Cell Type and Performance.status don't seem to satisfy PH assumption. As Performance.status 
#is continuous we need to stratify it and then form strata against Cell.Type to form our Z*. 
vets$PSBin<-cut(vets$Performance.status,c(5,60,99),labels=c(0,1));
#To make our "dummy variable" code easier to read we create separate columns
#CT and PS for vets$
#Use "alternate" dummy variables for Z*
vets$Z1=vets$Cell.Type.1; vets$Z2=vets$Cell.Type.2; vets$Z3=vets$Cell.Type.3
vets$Z4=as.numeric(vets$PSBin)-1; vets$Z5=vets$Z1*vets$Z4; vets$Z6=vets$Z2*vets$Z4; 
vets$Z7=vets$Z3*vets$Z4
#We have 7 dummy variables representing our 8 strata of Z*. If we make an interaction
#model with Treatment, Age, Disease Duration and Prior Therapy we would have
#4+4*7=32 terms. Instead we just include Age and Treatment, and we will have
#2+2*7=16 terms. 
Coxph.vets.S.nint<-coxph(W ~ Age + Treatment + strata(Cell.Type.1,Cell.Type.2,Cell.Type.3,PSBin),data=vets)
Coxph.vets.S.int<-coxph(W ~ Age + Treatment+Age:Z1+Treatment:Z1+Age:Z2+Treatment:Z2 
                        +Age:Z3+Treatment:Z3+Age:Z4+Treatment:Z4+Age:Z5+Treatment:Z5
                        +Age:Z6+Treatment:Z6+Age:Z7+Treatment:Z7+strata(Cell.Type.1,Cell.Type.2,Cell.Type.3,PSBin),data=vets)
#Log-likehood of our Reduced model 
LLR=Coxph.vets.S.nint$loglik[2]
#Log-likehood of our Full model 
LLF=Coxph.vets.S.int$loglik[2] 
CSStat=-2*(LLR-LLF); 
CV=qchisq(.05, 14, lower.tail=FALSE)
pvalue=pchisq(CSStat, 14, lower.tail = FALSE)
CSStat>CV
print(paste0("The pvalue of test statistic is:",pvalue))
int.cf=Coxph.vets.S.int$coefficients
#HR for Age when Cell Type is squamous and PSBin=0 is
exp(int.cf[1])
#HR for TR when when Cell Type is squamous and PSBin=0 is
exp(int.cf[2])
#HR for Age when Cell Type is small and PSBin=1 is
exp(int.cf[1]+int.cf[7]+int.cf[9]+int.cf[15])
#HR for TR when Cell Type is small and PSBin=1 is 
exp(int.cf[2]+int.cf[8]+int.cf[10]+int.cf[16])

#Problem 5.4
f1.1<-function(b) exp(b)/(3*exp(b)+1)
f1.2<-function(b) 1/(1+exp(b))
f2.1<-function(b) exp(b)/(2*exp(b)+1)
f2.2<-function(b) exp(b)/(exp(b)+1)
f<-function(b) -f1.1(b)*f1.2(b)*f2.1(b)*f2.2(b)
#f <- function(b) -exp(b)/(3*exp(2*b)+4*exp(b)+1)
x <- seq(-1,10,0.01)
windows(width=10, height=8)
plot(x, f(x))   
optimize(f, lower = 0, upper = 2)
time<-c(2,3,5,8,3,5,7);status<-c(1,0,1,1,1,1,1);Coupon<-c(1,1,0,1,1,1,0)
location<-c(0,0,0,0,1,1,1)
Sales=data.frame(Coupon,status,time,location)
S<-Surv(Sales$time,Sales$status==1)
coxph(S~Coupon+strata(location),data=Sales)



