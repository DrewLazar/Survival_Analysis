# Determines if a pair of covariates (xi, xj) is pure, mixed or neither
# according to Kretowska 2017
# pom = function(xi,xj,lowerQ,upperQ) {
#   
#   xitime = xi$survt
#   xjtime = xj$survt
#   xistat = xi$status
#   xjstat = xj$status
#   
#   # Direct implementation of Kretowska's conditions:
#   if (
#     (xistat == 1) && (xjstat == 1) && (abs(xitime - xjtime) < lowerQ)
#   ) {
#     return("pure")
#   } else if (
#     ((xistat == 1) && (xjstat == 1) && (abs(xitime - xjtime) > upperQ)) ||
#     ((xistat == 0) && (xjstat == 1) && ((xitime - xjtime) > upperQ)) ||
#     ((xistat == 1) && (xjstat == 0) && ((xjtime - xitime) > upperQ))
#   ) {
#     return("mixed")
#   } else {
#     return("neither")
#   }
#   
# }

# Classifies all data points according to pom(...) above
# pommatrix = function(survdata,lowerQ,upperQ) {
#   N = nrow(survdata)
#   matrix = matrix("neither",N,N)
#   for (i in 1:(N-1)) {
#     for (j in (i+1):N) {
#       matrix[i,j] = pom(survdata[i,], survdata[j,], lowerQ, upperQ)
#     }
#   }
#   return(matrix)
# }


# Test data Remission:


#load(file = "../Data/Remission.rda")

#Remission <- Remission[sample(nrow(Remission), 6), ]

# Subset the uncensored survival times
#Remission.uncensored <- Remission[Remission$status==1,]

# Collect all pairwise (without double counting) absolute
# difference between survival times
#Remission.absdists <- c(dist(Remission.uncensored$survt,
#                             method = "manhattan"))

# Take quantiles of absolute difference between survival times
#Q <- quantile(Remission.absdists, c(0.30, 0.70))

# The "pure or mixed" matrix for the data set:
#Remission.poms <- pommatrix(Remission, Q["30%"], Q["70%"])
Remission.poms <- matrix(
  c("neither", "neither", "mixed", "neither"), nrow = 2
  )

# The covariates of the data set
Remission <- data.frame(TR = c(0, 1), logWBC = c(0.1, 3.7))
Remission.X <- Remission[c("TR", "logWBC")]

# Augment the covariates of the data set
Remission.Z <- cbind(augment = 1, Remission.X)
Remission.Zmatrix <- as.matrix(Remission.Z)

# Count the number of plus and minus penalty functions
# corresponding to each data point, as per Kretowska 2017
phipmcount = function(Zmatrix, vorient, pommatrix){
  
  Zv <- Zmatrix %*% vorient
  
  N <- nrow(Zmatrix)
  
  phip <- rep(0, N)
  phim <- rep(0, N)
  
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      
      if (pommatrix[i, j] == "pure") {
        
        if (Zv[i] + Zv[j] > 0) {
          phip[i] = phip[i] + 1
          phip[j] = phip[j] + 1
        } else {
          phim[i] = phim[i] + 1
          phim[j] = phim[j] + 1
        }
        
      } else if (pommatrix[i, j] == "mixed") {
        
        if (Zv[i] - Zv[j] > 0) {
          phip[i] = phip[i] + 1
          phim[j] = phim[j] + 1
        } else {
          phim[i] = phim[i] + 1
          phip[j] = phip[j] + 1
        }
        
      }
      
    }
  }
  
  return(cbind(phipcount = phip, phimcount = phim))
}

# Test phipmcount(...)
Remission.phipm <- phipmcount(Remission.Zmatrix,
                              c(-mean(Remission$logWBC), 0, 1),
                              Remission.poms)

# Linear Programming Library
if (!require("lpSolve")) {
  install.packages("lpSolve")
}
library(lpSolve)

# Set up linear program

N <- nrow(Remission.Z)
Dp1 <- ncol(Remission.Z)

# Coefficients of objective
objcoeffs <- c(rep(0, 2*Dp1), rep(1, 2*N))

I2N <- diag(1, 2*N)

pZmZ <- rbind( Remission.phipm[,"phipcount"] * Remission.Zmatrix,
              -Remission.phipm[,"phimcount"] * Remission.Zmatrix)

O2NDp1 <- matrix(0, 2*N, Dp1)

# Constraint matrix
constmat <- cbind(pZmZ, -pZmZ, I2N)

# Constraint RHS
delta <- 0.1
constrhs <- c(delta * Remission.phipm[,"phipcount"],
              delta * Remission.phipm[,"phimcount"])

lpsoln <- lp(direction = "min",
             objective.in = objcoeffs,
             const.mat = constmat,
             const.dir = ">=",
             const.rhs = constrhs)

lpsoln$solution[1 : Dp1] -  lpsoln$solution[(Dp1+1) : (2*Dp1)]

plot(logWBC ~ TR, data = Remission)