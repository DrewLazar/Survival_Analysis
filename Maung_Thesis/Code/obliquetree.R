rm(list=ls())
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda") 
library(lpSolve)
library(survival)

meansplit<-function(data,lowerQ, upperQ, delta, epsilon,
                    time, censor,covariates=c(),nsize){

#pure or mixed matrix function XZ poms 
  XZpoms = function(data, lowerQ, upperQ, time, censor, covariates = c()) {
    data.uncensor <- data[data[censor] == 1,]
    data.absdists <- c(dist(data.uncensor[time],
                            method = "manhattan"))
    QV <- quantile(data.absdists, c(lowerQ, upperQ))
    # Determines if a pair of covariates (xi, xj) is pure, mixed or neither
    # according to Kretowska 2017
    pom = function(xi, xj) {
      xitime = xi[time]
      xjtime = xj[time]
      xicensor = xi[censor]
      xjcensor = xj[censor]
      lQV = QV[1]
      uQV = QV[2]
      # Direct implementation of Kretowska's conditions:
      if (
        (xicensor == 1) && (xjcensor == 1) && (abs(xitime - xjtime) < lQV)
      ) {
        return("pure")
      } else if (
        ((xicensor == 1) && (xjcensor == 1) && (abs(xitime - xjtime) > uQV)) ||
        ((xicensor == 0) && (xjcensor == 1) && ((xitime - xjtime) > uQV)) ||
        ((xicensor == 1) && (xjcensor == 0) && ((xjtime - xitime) > uQV))
      ) {
        return("mixed")
      } else {
        return("neither")
      }
    }
    # Classifies all data points according to pom(...) above
    pommatrix = function() {
      N = nrow(data)
      matrix = matrix("neither",N,N)
      for (i in 1:(N-1)) {
        for (j in (i+1):N) {
          matrix[i,j] = pom(data[i,], data[j,])
        }
      }
      return(matrix)
    }
    poms = pommatrix()
    X = if(length(covariates) == 0) {
      data[, !(colnames(data) %in% c(time, censor))]
    } else {
      data[covariates]
    }
    Z <- as.matrix(cbind("1" = 1, X))
    return(
      list(X = X, Z = Z, poms = poms)
    )
  }
#End of pure or mixed (pom) matrix function 
  
#Function for initial guess
  v0.default <- function(X, j) {
    v0 <- rep(0, ncol(X) + 1)
    v0[1] <- -mean(X[,j])
    v0[j + 1] <- 1
    return(v0)
  }

criterion <- function(Z, v, poms, delta) {
    phipm <- phipmcount(Z, v, poms)
    Zv <- Z %*% v
    phipm %*% pmax(delta + c(-Zv, Zv), 0)
}

phipmcount = function(Z, vorient, poms){
  Zv <- Z %*% vorient
  N <- nrow(Z)
  phip <- rep(0, N)
  phim <- rep(0, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      if (poms[i, j] == "pure") {
        # if we EXPECT both zi and zj on +ve side of vorient:
        if (Zv[i] + Zv[j] > 0) {
          phip[i] = phip[i] + 1
          phip[j] = phip[j] + 1
          # otherwise:
        } else {
          phim[i] = phim[i] + 1
          phim[j] = phim[j] + 1
        }
      } else if (poms[i, j] == "mixed") {
        
        # if we EXPECT zi on +ve side of vorient
        # while zj on -ve side of vorient:
        if (Zv[i] - Zv[j] > 0) {
          phip[i] = phip[i] + 1
          phim[j] = phim[j] + 1
          # otherwise:
        } else {
          phim[i] = phim[i] + 1
          phip[j] = phip[j] + 1
        }
      }
    }
  }
  return(c(phip, phim))
}
  
#Optimization function to find splitting v 
  optimizer <- function(Z, v0, poms, delta, epsilon) {
    phipm <- phipmcount(Z, v0, poms)
    # Set up linear program
    N <- nrow(Z)
    Dp1 <- ncol(Z)
    # Coefficients of objective
    # NOTE: lp assumes ALL variables are >= 0.
    # This means the free variable v must be written as
    # v = v' - v'' : v', v'' >= 0
    # to get an lp in standard form
    objcoeffs <- c(rep(0, 2*Dp1), rep(1, 2*N))
    I2N <- diag(1, 2*N)
    pZmZ <- phipm * rbind(Z, -Z)
    # Constraint matrix
    # NOTE: lp assumes ALL variables are >= 0.
    # This means the free variable v must be written as
    # v = v' - v'' : v', v'' >= 0
    # to get an lp in standard form
    constmat <- cbind(pZmZ, -pZmZ, I2N)
    # Constraint RHS
    constrhs <- delta * phipm
    # NOTE: lp assumes ALL variables are >= 0
    lpsoln <- lp(direction = "min",
                 objective.in = objcoeffs,
                 const.mat = constmat,
                 const.dir = ">=",
                 const.rhs = constrhs)
    v <- lpsoln$solution[1 : Dp1] -  lpsoln$solution[(Dp1+1) : (2*Dp1)]
    #print(v)
    #plot(logWBC ~ TR, data = Remission, pch = 20)
    #abline(a = -v[1]/v[3],
     #      b = -v[2]/v[3])
    # Reorientation phase
    vold = v
    criterion.old = criterion(Z, v, poms, delta)
    i = 0
    repeat {
      phipm.new <- phipmcount(Z, vold, poms)
      pZmZ.new <- phipm.new * rbind(Z, -Z)
      # Constraint matrix
      # NOTE: lp assumes ALL variables are >= 0.
      # This means the free variable v must be written as
      # v = v' - v'' : v', v'' >= 0
      # to get an lp in standard form
      constmat.new <- cbind(pZmZ.new, -pZmZ.new, I2N)
      constrhs.new <- delta * phipm.new
      lpsoln.new <- lp(direction = "min",
                       objective.in = objcoeffs,
                       const.mat = constmat.new,
                       const.dir = ">=",
                       const.rhs = constrhs.new)
      vnew <- lpsoln.new$solution[1 : Dp1] -  lpsoln.new$solution[(Dp1+1) : (2*Dp1)]
      criterion.new = criterion(Z, vnew, poms, delta)
      vold = vnew
      i = i + 1
      if (abs(criterion.old - criterion.new) < epsilon) {
        break
      }
      criterion.old = criterion.new
    }
    #print(i)
    return(vold)
  }
#End of optimization function to find splitting v 

#get the pom matrix and the covariates, X
  X=data[covariates]
  poms= XZpoms(data, lowerQ, upperQ, time, censor, covariates)
  #splitting function 
  splittingf<-function(X,ninfo) {
    nrx = nrow(X)
    if (nrx>nsize){
      #split X with v
      v0=v0.default(X,2)
      subsetnames=data.frame(rownames(X))
      subsetX=strtoi(as.vector(subsetnames[[1]]))
      newpoms=poms[[3]][subsetX,subsetX]
      Z=as.matrix(cbind(1,X))
      v=optimizer(Z, v0, newpoms, delta, epsilon)
      splits=Z%*%v
      XL=X[splits<0,]; XR=X[splits>=0,]
      ninfol=paste(ninfo,'l'); ninfor=paste(ninfo,'r')
      mylist<-list(v,XL,ninfol,XR,ninfor)
      sizeflag=FALSE
      #logrankstat
       dataflr=data[subsetX,]
       Y<-Surv(dataflr[time][[1]],dataflr[censor][[1]]==1)
       lrstat=survdiff(Y~splits<0)[[5]]
      #compute KM estimate
      kmfit='notterminal'
       return(list(mylist,sizeflag,lrstat,kmfit))
    } else {
      v=rep(0,3)
      XL=X;XR=X 
      ninfol=paste(ninfo,'x'); ninfor=paste(ninfo,'x')
      mylist<-list(v,XL,ninfol,XR,ninfor)
      sizeflag=TRUE
      lrstat=0
      #Compute the KM estimate 
      subsetnames=data.frame(rownames(X))
      subsetX=strtoi(as.vector(subsetnames[[1]]))
      datasubset = data[subsetX,]
      Y<-Surv(datasubset[time][[1]],datasubset[censor][[1]]==1)
      kmfit=survfit(Y~1)
      return(list(mylist,sizeflag,lrstat,kmfit))
    }
  }
  #firstsplit 
  nlist<-splittingf(X,'0')[[1]]
  #store v's and node info 
  vlist <- vector(mode = "list", length = 2) 
  vlist[[1]]<-nlist[[1]]
  vlist[[2]]<-'0'
  #store the log-rank stat
  lrstat<-c()
  lrstat[1]=splittingf(X,'0')[[3]]
  #store the KMestimate
  KMest <-vector(mode = "list", length = 1) 
  KMest[[1]]=splittingf(X,'0')[[4]]
  #produce input for next iteration 
  nolist <- nlist[-1]
  #splits 2 to splitlevel 
  nsizecheck=splittingf(X,'0')[[2]];i=0 
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
data=Remission; lowerQ = 0.35; upperQ = 0.65;delta = 0.1;epsilon = 10^(-4); time="survt"
censor="status"; covariates = c("TR", "logWBC"); nsize=10
obliquetree=meansplit(data,lowerQ, upperQ, delta, epsilon,time, censor,covariates,nsize)
print(paste("Our oblique tree with max node size of", nsize, "is:"))
print(obliquetree)