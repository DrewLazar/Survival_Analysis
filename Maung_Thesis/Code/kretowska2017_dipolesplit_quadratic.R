# Determines if a pair of covariates (xi, xj) is pure, mixed or neither
# according to Kretowska 2017
pom = function(xi,xj,lowerQ,upperQ) {
  
  xitime = xi$survt
  xjtime = xj$survt
  xistat = xi$status
  xjstat = xj$status
  
  # Direct implementation of Kretowska's conditions:
  if (
    (xistat == 1) && (xjstat == 1) && (abs(xitime - xjtime) < lowerQ)
  ) {
    return("pure")
  } else if (
    ((xistat == 1) && (xjstat == 1) && (abs(xitime - xjtime) > upperQ)) ||
    ((xistat == 0) && (xjstat == 1) && ((xitime - xjtime) > upperQ)) ||
    ((xistat == 1) && (xjstat == 0) && ((xjtime - xitime) > upperQ))
  ) {
    return("mixed")
  } else {
    return("neither")
  }
  
}

# Classifies all data points according to pom(...) above
pommatrix = function(survdata,lowerQ,upperQ) {
  n = nrow(survdata)
  matrix = matrix("neither",n,n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      matrix[i,j] = pom(survdata[i,], survdata[j,], lowerQ, upperQ)
    }
  }
  return(matrix)
}


# Test pommatrix(...)
setwd("C:/gitstuff/Survival_Analysis/Maung_Thesis/Data") 
load("Remission.rda")
load(file = "../Data/Remission.rda")

Remission.event<-Remission[Remission$status==1,]
#take quantile of magnitudes between event times. 
Q <- quantile(Remission.event$survt, c(0.30, 0.70))

poms <- pommatrix(Remission, Q["30%"], Q["70%"])

Remission.Covariates <- Remission[c("TR", "logWBC")]
