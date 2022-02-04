rm(list=ls())
setwd("C:/GitStuff/Survival_Analysis/Maung_Thesis/Code")
#load Remission data 
load("C:/gitstuff/Survival_Analysis/Maung_Thesis/Data/Remission.rda")
#load univariate splitting function
source("univariatesplitfunction2.R")
library(lpSolve)
library(survival)
library(dplyr)

meansplit<-function(data, time, censor,covariates=c(),nsize){
  #splitting function 
  splittingf<-function(X,ninfo) {
    nrx = nrow(X)
    if (nrx>nsize){
      #split X with v
      #find v 
      Z=as.matrix(cbind(1,X))
      subsetnames=data.frame(rownames(X))
      subsetX=strtoi(as.vector(subsetnames[[1]]))
      datasubset = data[subsetX,]
      opt.split.bycov =bestsplit(datasubset,covariates,time,censor)
      v = opt.split.bycov[[1]]
      #compute logrank stat 
      lrstat=opt.split.bycov[[2]]
      #compute KM estimate
      kmfit='notterminal'
      #split the data 
      splits=Z%*%v
      XL=X[splits<=0,]; XR=X[splits>0,]
      ninfol=paste(ninfo,'l'); ninfor=paste(ninfo,'r')
      mylist<-list(v,XL,ninfol,XR,ninfor)
      sizeflag=FALSE
      return(list(mylist,sizeflag,lrstat,kmfit))
    } else {
      XL=X;XR=X 
      ninfol=paste(ninfo,'x'); ninfor=paste(ninfo,'x')
      v=rep(0,length(covariates)+1)
      mylist<-list(v,XL,ninfol,XR,ninfor)
      sizeflag=TRUE
      lrstat=0
      #Compute KM estimate 
      subsetnames=data.frame(rownames(X))
      subsetX=strtoi(as.vector(subsetnames[[1]]))
      datasubset = data[subsetX,]
      Y<-Surv(datasubset[time][[1]],datasubset[censor][[1]]==1)
      kmfit=survfit(Y~1)
      return(list(mylist,sizeflag,lrstat,kmfit))
    }
  }
  #firstsplit 
  X=data[covariates]
  apply.split = splittingf(X,'0')
  nlist<-apply.split[[1]]
  #store v's and node info 
  vlist <- vector(mode = "list", length = 2) 
  vlist[[1]]<-nlist[[1]]
  vlist[[2]]<-'0'
  #store the log-rank stat
  lrstat<-c()
  lrstat[1]=apply.split[[3]]
  #store the KMestimate
  KMest <-vector(mode = "list", length = 1) 
  KMest[[1]]=apply.split[[4]]
  #produce input for next iteration 
  nolist <- nlist[-1]
  #splits 2 to splitlevel 
  nsizecheck=apply.split[[2]];i=0 
  while (!all(nsizecheck)){
    #initialize list of list of splits
    i=i+1
    #nsizecheckrep=rep(nsizecheck,each=2)
    nlist <- vector(mode = "list", length = 2^i) 
    nsizecheck=c(rep(FALSE,2^i))
    for (k in 1:(2^i)){
      splitlist <- vector(mode = "list", length = 1) 
      splitlist<-splittingf(nolist[[2*k-1]],nolist[[2*k]])
      a=2^(i+1)-1+2*(k-1)
      #store the split information 
      vlist[[a]]<-splitlist[[1]][[1]]; vlist[[a+1]]<-nolist[[2*k]] 
      nlist[[k]]<-splitlist[[1]]
      #store the log-rank stat
      lrstat[2^i-1+k]=splitlist[[3]]
      #store the KM estimate
      KMest[[2^i-1+k]]=splitlist[[4]]
      #size check 
      nsizecheck[k]=splitlist[[2]]
    }
    nolist <- vector(mode = "list", length = 2^i) 
    for (k in 1:(2^i)){
      nolist[[k]]<-nlist[[k]][-1]
    }
    nolist<-unlist(nolist,recursive=FALSE)
  }
  nodesx=vector();j=0 
  for (i in 1:(length(vlist)/2)){
    if (grepl("x",vlist[[2*i]], fixed=TRUE)){
      j=j+1;nodesx[j]=2*i
    }
  }
  vwithx=nodesx-1; remvlist =c(rbind(vwithx,nodesx))
  vlistf<-vlist[-remvlist]
  lrstat<-lrstat[-(nodesx)/2]
  KMest<-KMest[-(nodesx)/2]
  outlist <- vector(mode = "list", length = 2) 
  outlist[[1]]<-vlistf
  outlist[[2]]<-lrstat
  outlist[[3]]<-KMest 
  return(outlist)
}   
#test out function - split by the mean into levels until each node
#has less than nsize members. Odd elements are v and even elements
# are node that is split)
data=Remission; time="survt"; censor="status"; covariates = c("TR", "logWBC"); nsize=10
obliquetree=meansplit(data,time, censor,covariates,nsize)
print(paste("Our oblique tree with max node size of", nsize, "is:"))
print(obliquetree)