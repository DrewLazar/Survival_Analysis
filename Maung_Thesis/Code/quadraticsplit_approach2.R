pom = function(s1,s2,eta,zeta) {
  
  s1time = s1$survt
  s2time = s2$survt
  s1stat = s1$status
  s2stat = s2$status
  
  if (
    (s1stat == 1) && (s2stat == 1) && (abs(s1time - s2time) < eta)
  ) {
    return("pure")
  } else if (
    ((s1stat == 1) && (s2stat == 1) && (abs(s1time - s2time) > zeta)) ||
    ((s1stat == 0) && (s2stat == 1) && ((s1time - s2time) > zeta)) ||
    ((s1stat == 1) && (s2stat == 0) && ((s2time - s1time) > zeta))
  ) {
    return("mixed")
  } else {
    return("neither")
  }
  
}

pommatrix = function(data,eta,zeta) {
  n = nrow(data)
  matrix = matrix("neither",n,n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      matrix[i,j] = pom(data[i,], data[j,], eta, zeta)
    }
  }
  return(matrix)
}

#test pommatrix(..)
load(file = "../Data/Remission.rda")
pommatrix(Remission,4,10)








