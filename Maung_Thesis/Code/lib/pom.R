XZpoms = function(data,
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
  
  N = nrow(data)
  DT <- c()
  for (i in 1:(N-1)) {
    
    xitime = data[i, time]
    xicensor = data[i, censor]
    
    print(xitime)
    
    for (j in (i+1):N) {
      xjtime = data[j, time]
      xjcensor = data[j, censor]
      
      if (
        (xicensor == 1) && (xjcensor == 1) ||
        ((xicensor == 0) && (xjcensor == 1) && (xitime > xjtime)) ||
        ((xicensor == 1) && (xjcensor == 0) && (xjtime > xitime))
      ) {
        DT <- append(DT, abs(xitime - xjtime))
      }
      
    }
  }
  
  print(DT)
  
  QV <- quantile(DT, c(lowerQ, upperQ))
  

  
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

# Test data Remission:
load(file = "../../Data/Remission.rda")

data = XZpoms(Remission,
              time = "survt", censor = "status",
              covariates = c("TR", "logWBC"))