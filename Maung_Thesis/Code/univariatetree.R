rm(list=ls())
library(survival)
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
load("Remission.rda")
data=Remission 
covariates = c("TR", "logWBC");
time="survt"
censor="status";

#bestsplit <- function(data, covariates = c(),time,censor)
ncovas=length(covariates)
opt.split.bycov = data.frame(covariate=NA,bestsplitval=NA)

for (i in 1:ncovas) {
  X<-data[covariates[i]]
  data.sort<- data %>% arrange(X)
  n = nrow(data)
  lrstat.old=0
  k=1
  while (k<=n-1){
   while ((data.sort[covariates[i]][k,]==data.sort[covariates[i]][k+1,]) & k<n){
     k=k+1
   }
  if(k<n){
   ind=c(rep(1,k),rep(0,n-k))
   Y<-Surv(data.sort[time][[1]],data.sort[censor][[1]]==1)
   lrstat=survdiff(Y~ind)[[5]]
   if (lrstat>lrstat.old){
     lrstat.max=lrstat
     split.pos=k 
   }
   lrstat.old=lrstat
   k=k+1
  }
}
  opt.split.bycov[i,1]=covariates[i]
  opt.split.bycov[i,2]=data.sort[covariates[i]][split.pos,1]
}
    