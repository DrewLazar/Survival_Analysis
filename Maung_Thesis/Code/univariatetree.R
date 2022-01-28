
rm(list=ls())
library(survival)
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
load("Remission.rda")
data=Remission 
covariates = c("TR", "logWBC");
time="survt"
censor="status";

bestsplit <- function(data, covariates = c(),time,censor)
ncov=length(covariates)
opt.split.bycov = data.frame(covariate=NA,bestsplitval=NA)

for (i in covariates) {
  X<-data[covariates[i]]
  data.sort<- data %>% arrange(X)
  n = nrow(data)
  lrstat.old=0
  for (k in 1:(n-1)){
   ind=c(rep(1,k),rep(0,n-k))
   Y<-Surv(data.sort[time][[1]],data.sort[censor][[1]]==1)
   lrstat=survdiff(Y~ind)[[5]]
   if (lrstat>lrstat.old){
     lrstat.max=lrstat
     split.pos=k 
   }
   lrstat.old=lrstat
  }
  opt.split.bycov[i,1]=covariate[i]
  opt.split.bycov[i,2]=X[k]
}
    