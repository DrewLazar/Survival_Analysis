#split a test set according to obliquetree
rm(list=ls())
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda") 
library(lpSolve)
library(survival)
data=Remission; lowerQ = 0.35; upperQ = 0.65;delta = 0.1;epsilon = 10^(-4); time="survt"
censor="status"; covariates = c("TR", "logWBC"); nsize=10
obliquetree=meansplit(data,lowerQ, upperQ, delta, epsilon,time, censor,covariates,nsize)
tree=obliquetree[[1]]
#create a function for splitting a test set according to our tree
testdata=data
covariates = c("TR", "logWBC");
X=data[covariates]
Z=as.matrix(cbind(1,X))
v=tree[[1]]
splits=Z%*%v
XL=X[splits<0,]; XR=X[splits>=0,]