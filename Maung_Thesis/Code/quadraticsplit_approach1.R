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

rot2d = function(theta) {
  rot = matrix(c(cos(theta), -sin(theta),
                 sin(theta), cos(theta)), 2, 2)
  return(rot)
}

stdquadpure2d = function(yi, yj, a) {
  ((yi[2] >= a*yi[1]^2) && (yj[2] >= a*yj[1]^2)) ||
  ((yi[2] < a*yi[1]^2) && (yj[2] < a*yj[1]^2))  
}

objfunc2d = function(data, pommatrix, cvec, a, theta) {
  n = nrow(data)
  
  purecorrect = 0
  mixedcorrect = 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      xi = data[i,]
      xj = data[j,]
      
      yi = t(rot2d(theta))%*%t(xi - cvec)
      yj = t(rot2d(theta))%*%t(xj - cvec)
      
      if (stdquadpure2d(yi, yj, a)) {
        if (pommatrix[i,j] == "pure") {
          purecorrect = purecorrect + 1
        }
      } else {
        if (pommatrix[i,j] == "mixed") {
          mixedcorrect = mixedcorrect + 1
        }
      }
    }
  }
  
  return(purecorrect + mixedcorrect)
}

# test optimization with Remission data
RemissionCov = Remission[c("TR", "logWBC")]
Remissionpom = pommatrix(Remission, 4, 10)

objfunc2d_Remission = function(par) {
  cvec = c(par[1], par[2])
  a = par[3]
  theta = par[4]
  
  -objfunc2d(RemissionCov, Remissionpom, cvec, a, theta)
}

# fails to optimize!!!
testoptim = function() {
  par = c(0, 0, 0, 0)
  optim(par, objfunc2d_Remission)
}

#objective test
pommatrix = matrix("neither",4,4)
pommatrix[1,2]="pure";pommatrix[1,3]="mixed";pommatrix[1,4]="pure"
pommatrix[2,4]="neither";pommatrix[3,4]="pure";pommatrix[2,3]="mixed"
data=cbind(c(.5,.75,-.5,2),c(1,1,1,0))
a=1;theta=0; cvec=c(1,0)
objfunc2d(data, pommatrix, cvec, a, theta)

stdquadpure2d(data[1,],data[2,],1)