library(data.tree)
library(survival)
rm(list=ls())
#setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda") 

X=Remission[,3:4]; Y=Remission[,1]; C=Remission[,2]

meansplit<-function(X,splitlevel){
#splitting function 
splittingf<-function(X,ninfo) {
  v=c(-mean(X[,2]),0,1);
  Z=as.matrix(cbind(1,X))
  splits=Z%*%v
  XL=X[splits<0,]; XR=X[splits>=0,]
  ninfol=paste(ninfo,'l'); ninfor=paste(ninfo,'r')
  mylist<-list(v,XL,ninfol,XR,ninfor)
}
#firstsplit 
  nlist<-splittingf(X,'0')
  #store v's and node info 
    vlist <- vector(mode = "list", length = 2) 
    vlist[[1]]<-nlist[[1]]
    vlist[[2]]<-'0'
#produce input for next iteration 
    nolist <- nlist[-1]
#splits 2 to splitlevel 
for (i in 1:(splitlevel-1)){
#initialize list of list of splits
  nlist <- vector(mode = "list", length = 2^i) 
  for (k in 1:(2^i)){
    splitlist <- vector(mode = "list", length = 1) 
    splitlist<-splittingf(nolist[[2*k-1]],nolist[[2*k]])
    a=2^(i+1)-1+2*(k-1)
    #store the split information 
    vlist[[a]]<-splitlist[[1]]; vlist[[a+1]]<-nolist[[2*k]] 
    nlist[[k]]<-splitlist
  }
  nolist <- vector(mode = "list", length = 2^i) 
  for (k in 1:(2^i)){
    nolist[[k]]<-nlist[[k]][-1]
  }
  nolist<-unlist(nolist,recursive=FALSE)
}
    return(vlist)
}
#test out function - split by the mean into 3 levels 
#with 2^3-1 nodes total. Function will produce 2^splitlevel-1 nodes
#total and meansplit(X,splitlevel) will be a list with 
#2^(splitlevel+1)-2 elements (odd elements are v and even elements node 
# node that is split)
splitlevel=3; 
test=meansplit(X,splitlevel)
