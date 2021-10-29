rm(list=ls())
#load the necessary packages 
library(survival)
library(e1071)
#load the R Survival package 
#Set working directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#load Remission data 
load("Remission.rda")
y<-Remission$survt; x<-cbind(Remission$TR,Remission$logWBC);
#Create categorical survival time 
yf=rep(0,42) 
for (i in 1:42)
{
  if (Remission$survt[i]<median(y)) {
    yf[i]=1
  } else {
    yf[i]=-1
  }
}
#Fit and plot SVM 
plot(x, col = yf + 3, pch = 19)
dat = data.frame(x, yf = as.factor(yf))
svmfit = svm(yf ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit,dat)
#Better Plot 
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
xgrid[1:10,]
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = yf + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
#extract coefficient 
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = yf + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)





