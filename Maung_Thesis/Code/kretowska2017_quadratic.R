# Linear Programming Library
if (!require("lpSolve")) {
  install.packages("lpSolve")
}
library(lpSolve)

XZpoms_quadratic = function(data,
                  lowerQ = 0.35, upperQ = 0.65,
                  time, censor, covariates = c()) {
  
  # Make sure the variable names are actually in the data frame
  for (name in c(time, censor, covariates)) {
    if (!(name %in% names(data))) {
      stop(
        paste(name, "is not a variable in the data", sep = " ")
      )
    }
  }
  
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
  
  # Quadratic Augmentation:
  augment.quadratic <- function(input){
    output <- do.call("cbind", lapply(1:ncol(input), FUN=function(z){
      do.call("cbind", lapply(z:ncol(input), FUN=function(x){
        tmp <- data.frame(input[,z] * input[,x])
        colnames(tmp)[1] <- paste(colnames(input)[z], ":", colnames(input)[x], sep="") # Multiplication symbol changed to : as per R style instead of x to avoid confusion with variables containing x
        tmp
      }))
    }))
    cbind(input, output)
  }
  
  X <- augment.quadratic(X)
  
  Z <- as.matrix(cbind("1" = 1, X))
  
  return(
    list(X = X, Z = Z, poms = poms)
  )
}

# Test data Remission:
load(file = "../Data/Remission.rda")

data = XZpoms_quadratic(
  Remission, time = "survt", censor = "status",
  covariates = c("TR", "logWBC")
  )


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

criterion <- function(Z, v, poms, delta = 0.1) {
  phipm <- phipmcount(Z, v, poms)
  Zv <- Z %*% v
  phipm %*% pmax(delta + c(-Zv, Zv), 0)
}


optimizer <- function(Z, v0, poms, delta = 0.1, epsilon = 10^(-4)) {
  
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
  # print(v)
  # plot(logWBC ~ TR, data = Remission, pch = 20)
  # abline(a = -v[1]/v[3],
  #        b = -v[2]/v[3])
  
  
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
  
  print(i)
  
  return(vold)
}

v0.default <- function(X, j) {
  v0 <- rep(0, ncol(X) + 1)
  v0[1] <- -mean(X[,j])
  v0[j + 1] <- 1
  return(v0)
}

# Test

v <- optimizer(data$Z,
               v0.default(data$X, 2),
               data$poms)

plot(logWBC ~ TR, data = Remission, pch = 20)
abline(a = -v[1]/v[3],
       b = -v[2]/v[3])