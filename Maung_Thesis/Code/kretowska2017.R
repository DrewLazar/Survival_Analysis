
renamesubset = function(data, time = "survt", censor = "status", covariates = c()) {
  names(data)[names(data) == time] = "time"
  names(data)[names(data) == censor] = "censor"
  
  if (length(covariates) == 0) {
    return(data)
  } else {
    return(data[c("time", "censor", covariates)])
  }
}

# Determines if a pair of covariates (xi, xj) is pure, mixed or neither
# according to Kretowska 2017
pom = function(xi, xj, lowerQ, upperQ) {
  
  xitime = xi["time"]
  xjtime = xj["time"]
  xicensor = xi["censor"]
  xjcensor = xj["censor"]
  
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
pommatrix = function(data, lowerQ, upperQ) {
  N = nrow(data)
  matrix = matrix("neither",N,N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      matrix[i,j] = pom(data[i,], data[j,], lowerQ, upperQ)
    }
  }
  return(matrix)
}


poms_and_X = function(data,
                      lowerQ = 0.35, upperQ = 0.65,
                      time = "survt", censor = "status", covariates = c()) {
  
  data = renamesubset(data, time = time, censor = censor, covariates = covariates)
  
  data.uncensor <- data[data$censor == 1,]
  data.absdists <- c(dist(data.uncensor$time,
                          method = "manhattan"))
  Q <- quantile(data.absdists, c(lowerQ, upperQ))
  
  return(
    list(poms = pommatrix(data, Q[1], Q[2]), X = data[-c("time", "censor")])
  )
}

# Test data Remission:
load(file = "../Data/Remission.rda")

data = poms_and_X(Remission, covariates = c("TR", "logWBC"))