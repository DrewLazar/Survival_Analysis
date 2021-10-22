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
  N = nrow(survdata)
  matrix = matrix("neither",N,N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      matrix[i,j] = pom(survdata[i,], survdata[j,], lowerQ, upperQ)
    }
  }
  return(matrix)
}


# Test data Remission:

# setwd("C:/gitstuff/Survival_Analysis/Maung_Thesis/Data") 
# load("Remission.rda")
load(file = "../Data/Remission.rda")

# Remission <- Remission[sample(nrow(Remission), 6), ]

# Subset the uncensored survival times
Remission.uncensored <- Remission[Remission$status==1,]

# Collect all pairwise (without double counting) absolute
# difference between survival times
Remission.absdists <- c(dist(Remission.uncensored$survt,
                             method = "manhattan"))

# Take quantiles of absolute difference between survival times
Q <- quantile(Remission.absdists, c(0.30, 0.70))

# The "pure or mixed" matrix for the data set:
Remission.poms <- pommatrix(Remission, Q["30%"], Q["70%"])

# The covariates of the data set
Remission.X <- Remission[c("TR", "logWBC")]
# FOR QUADRATIC AUGMENTATION OF COVARIATES USE THIS INSTEAD:
# augment.quadratic <- function(input){
#   output <- do.call("cbind", lapply(1:ncol(input), FUN=function(z){ 
#     do.call("cbind", lapply(z:ncol(input), FUN=function(x){
#       tmp <- data.frame(input[,z] * input[,x])
#       colnames(tmp)[1] <- paste(colnames(input)[z], ":", colnames(input)[x], sep="") # Multiplication symbol changed to : as per R style instead of x to avoid confusion with variables containing x
#       tmp
#     }))
#   }))
#   cbind(input, output)
# }
# Remission.X <- augment.quadratic(Remission[c("TR", "logWBC")])

# Augment the covariates of the data set
Remission.Z <- cbind("1" = 1, Remission.X)
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
        
        # if we EXPECT both zi and zj on +ve side of vorient:
        if (Zv[i] + Zv[j] > 0) {
          phip[i] = phip[i] + 1
          phip[j] = phip[j] + 1
        # otherwise:
        } else {
          phim[i] = phim[i] + 1
          phim[j] = phim[j] + 1
        }
        
      } else if (pommatrix[i, j] == "mixed") {
        
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
# NOTE: lp assumes ALL variables are >= 0.
# This means the free variable v must be written as
# v = v' - v'' : v', v'' >= 0
# to get an lp in standard form
objcoeffs <- c(rep(0, 2*Dp1), rep(1, 2*N))

I2N <- diag(1, 2*N)

pZmZ <- rbind( Remission.phipm[,"phipcount"] * Remission.Zmatrix,
              -Remission.phipm[,"phimcount"] * Remission.Zmatrix)

# Constraint matrix
# NOTE: lp assumes ALL variables are >= 0.
# This means the free variable v must be written as
# v = v' - v'' : v', v'' >= 0
# to get an lp in standard form
constmat <- cbind(pZmZ, -pZmZ, I2N)

# Constraint RHS
delta <- 0.1
constrhs <- delta * c(Remission.phipm[,"phipcount"],
                      Remission.phipm[,"phimcount"])

# NOTE: lp assumes ALL variables are >= 0
lpsoln <- lp(direction = "min",
             objective.in = objcoeffs,
             const.mat = constmat,
             const.dir = ">=",
             const.rhs = constrhs)

v <- lpsoln$solution[1 : Dp1] -  lpsoln$solution[(Dp1+1) : (2*Dp1)]

plot(logWBC ~ TR, data = Remission)
abline(a = -v[1]/v[3], b = -v[2]/v[3])

v.planeeqn <- paste(
  paste(v, names(Remission.Z), sep = " * ", collapse = "    +    "),
  "    =    0"
)