#Chapter 7
#Clean up
rm(list=ls())
#load the necessary packages 
library(survival)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Addicts data 
load("addicts.rda")
modpar1=survreg(Surv(addicts$survt,addicts$status)~prison+dose+clinic,data=addicts,dist="exponential")