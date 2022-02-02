library(survival)
library(dplyr)
bestsplit <- function(data, covariates = c(),time,censor){
  ncovas=length(covariates)
  opt.split.bycov = data.frame(covariate=NA,bestsplitval=NA,lrstat=NA)
  for (i in 1:ncovas) {
    data.sort<- data %>% arrange(data[covariates[i]])
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
    opt.split.bycov[i,3]=lrstat.max
  }
  logr=max(opt.split.bycov[,3])
  max=which.max(opt.split.bycov[,3])
  split=opt.split.bycov[max,]
  v=c(-split[[2]],rep(0,length(covariates)))
  v[max+1]=1
  mylist=list(v,logr)
  return(mylist)
}
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
load("Remission.rda")
data=Remission 
covariates = c("TR", "logWBC");
time="survt"
censor="status"
optsplit=bestsplit(data, covariates,time,censor)