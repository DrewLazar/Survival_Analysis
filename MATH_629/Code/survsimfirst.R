

install.packages("simsurv")
library(simsurv)
library(survival)

cov <- data.frame(id = 1:150,
                  trt = c(rep(1,50),rep(0,50),rep(2,50)),
                  trt2=round(rnorm(150,2,1.5),2))

dat <- simsurv(lambdas = 0.1, 
               gammas = 1.5, 
               betas = c(trt = -1,trt2=1), 
               x = cov, 
               maxt = 20)
dat <- merge(cov, dat)

Y<-Surv(dat$eventtime,dat$status==1)
Coxph.Rem.m1=coxph(Y~trt+trt2,data=dat)
summary(Coxph.Rem.m1)