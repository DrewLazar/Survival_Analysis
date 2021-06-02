library(survival)
rm(list=ls())
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
load("Remission.rda")
#Chapter 7
attach(Remission)
Y=Surv(survt,status)
remmod.exp=survreg(Y~TR,dist="exponential")
summary(remmod.exp)