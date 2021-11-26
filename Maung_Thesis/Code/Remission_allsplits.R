library(dplyr)
rm(list=ls())
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
X<-Remission$TR; Y<-Remission$logWBC
Rem.XY<-data.frame(X,Y)
Rem.XY<-Rem.XY %>% arrange(X,Y)
Rem.XY0<-Rem.XY[Rem.XY$X==0,]
Rem.XY1<-Rem.XY[Rem.XY$X==1,]
min0<-min(Rem.XY0$Y)-0.10; max0<-max(Rem.XY0$Y)+0.10
min1<-min(Rem.XY1$Y)-0.10; max1<-max(Rem.XY1$Y)+0.10
Rem.XY0<-rbind(c(0,min0),Rem.XY0,c(0,max0))
Rem.XY1<-rbind(c(1,min1),Rem.XY0,c(1,max1))
Rem.md0<-rep(0,22)
for (i in 1:22)
{
  Rem.md0[i]<-(Rem.XY0$Y[i]+Rem.XY0$Y[i+1])/2
}
Rem.md1<-rep(1,22)
for (i in 1:22)
{
  Rem.md1[i]<-(Rem.XY1$Y[i]+Rem.XY1$Y[i+1])/2
}
a=rep(0,484);b=rep(0,484);k=0
for (i in 1:22)
{
  for (j in 1:22)
  {
    k=k+1
    a[k]=Rem.md0[i]-Rem.md1[j]
    b[k]=Rem.md0[i]
  }
}
lines<-as.data.frame(cbind(a,b))
#lines are of the form y+ax=b 
save(lines, file = "allsplits.rda")