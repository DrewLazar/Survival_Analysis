poms_and_X = function(data,
                      lowerQ = 0.35, upperQ = 0.65,
                      time = "survt", censor = "status", covariates = c()) {

  
  # Determines if a pair of covariates (xi, xj) is pure, mixed or neither
  # according to Kretowska 2017
  pom = function(xi, xj) {
    
    xitime = xi[time]
    xjtime = xj[time]
    xicensor = xi[censor]
    xjcensor = xj[censor]
    
    # Direct implementation of Kretowska's conditions:
    if (
      (xicensor == 1) && (xjcensor == 1) && (abs(xitime - xjtime) < lowerQ)
    ) {
      return("pure")
    } else if (
      ((xicensor == 1) && (xjcensor == 1) && (abs(xitime - xjtime) > upperQ)) ||
      ((xicensor == 0) && (xjcensor == 1) && ((xitime - xjtime) > upperQ)) ||
      ((xicensor == 1) && (xjcensor == 0) && ((xjtime - xitime) > upperQ))
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
  
  
  
  data.uncensor <- data[data[censor] == 1,]
  data.absdists <- c(dist(data.uncensor[time],
                          method = "manhattan"))
  Q <- quantile(data.absdists, c(lowerQ, upperQ))
  
  poms = pommatrix()
  X = if(length(covariates) == 0) {
    data[, !(colnames(data) %in% c(time, censor))]
  } else {
    data[covariates]
  }
  
  return(
    list(poms = poms, X = X)
  )
}

# Test data Remission:
load(file = "../Data/Remission.rda")

data = poms_and_X(Remission, covariates = c("TR", "logWBC"))