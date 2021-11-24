#Chapter 7
#Clean up
rm(list=ls())
#load the necessary packages 
library(survival)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
#Create a Survival object 
Y<-Surv(Remission$survt,Remission$status)
modpar1=survreg(Y~TR,data=Remission,dist="exponential")
            