# Create a dummy data set
Remission <- data.frame(TR = c(0, 1), logWBC = c(0.1, 3.7))

# Create a dummy "pure or mixed (or neither)" matrix
Remission.poms <- matrix(
  c("neither", "neither", "mixed", "neither"), nrow = 2
  )

# The covariates of the data set
Remission.X <- Remission[c("TR", "logWBC")]

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

v <- lpsoln$solution[1 : Dp1] -  lpsoln$solution[(Dp1+1) : (2*Dp1)]

plot(logWBC ~ TR, data = Remission)
abline(a = -v[1]/v[3], b = -v[2]/v[3])

v.planeeqn <- paste(
  paste(v, names(Remission.Z), sep = " * ", collapse = "    +    "),
  "    =    0"
)