library(dplyr)
#Set work directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
Remission<- read.table("anderson.dat",header = FALSE)
Remission<-rename(Remission,survt=V1,status=V2,Sex=V3,logWBC=V4,TR=V5)
#Add strata LogWBC.group 
Remission$logWBC.group<-cut(Remission$logWBC,c(1.44,2.30,2.96,5.00),labels=c('1','2','3'))
save(Remission, file = "Remission.rda")