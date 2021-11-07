---
title: "MATH 629 HW 4 Solutions"
author: "Drew Lazar"
output: 
  pdf_document: 
    keep_tex: yes
---




## Cleaning up and loading necessary packages 

```r
rm(list = ls())
library(survival)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## 1 loading the data 

```r
load("addicts.rda")
```
##2 Test the PH assumption
##2i. GOF using Schoenfeld Residuals 

```r
Y <- Surv(addicts$survt, addicts$status)
Coxph.addicts = coxph(Y ~ prison + dose + clinic, data = addicts)
summary(Coxph.addicts)
```

```
## Call:
## coxph(formula = Y ~ prison + dose + clinic, data = addicts)
## 
##   n= 238, number of events= 150 
## 
##             coef exp(coef)  se(coef)      z Pr(>|z|)    
## prison  0.326555  1.386184  0.167225  1.953   0.0508 .  
## dose   -0.035369  0.965249  0.006379 -5.545 2.94e-08 ***
## clinic -1.009896  0.364257  0.214889 -4.700 2.61e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##        exp(coef) exp(-coef) lower .95 upper .95
## prison    1.3862     0.7214    0.9988    1.9238
## dose      0.9652     1.0360    0.9533    0.9774
## clinic    0.3643     2.7453    0.2391    0.5550
## 
## Concordance= 0.665  (se = 0.025 )
## Likelihood ratio test= 64.56  on 3 df,   p=6e-14
## Wald test            = 54.12  on 3 df,   p=1e-11
## Score (logrank) test = 56.32  on 3 df,   p=4e-12
```

```r
cox.zph(Coxph.addicts, transform = rank)
```

```
##         chisq df       p
## prison  0.853  1 0.35567
## dose    0.608  1 0.43557
## clinic 11.302  1 0.00077
## GLOBAL 12.465  3 0.00595
```
##2ii. Log-log plots for each variable

```r
# Create dose.group
quantile(addicts$dose)
```

```
##   0%  25%  50%  75% 100% 
##   20   50   60   70  110
```

```r
addicts$dose.group <- cut(addicts$dose, c(19, 50, 70, 110), labels = c("1", "2",
    "3"))
# create a survival object from the addicts data set
Y <- Surv(addicts$survt, addicts$status == 1)
# Create Cox PH model for clinic
kmfitC2 = survfit(Y ~ addicts$clinic)
# Create Cox PH model for prison
kmfitP2 = survfit(Y ~ addicts$prison)
# Create Cox PH model for dose.group
kmfitDG32 = survfit(Y ~ addicts$dose.group)
```

```r
# log-log curves for clinic
plot(kmfitC2, fun = "cloglog", xlab = "time in days on log scale", ylab = "log-log survival",
    main = "log-log curves by clinic", col = c("red", "green"))
legend("topright", c("Clinic=1", "Clinic=2"), lty = c("solid"), col = c("red", "green"))
```

![](HW4_629Solutions_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

```r
# log-log curves for prison
plot(kmfitP2, fun = "cloglog", xlab = "time in days on log scale", ylab = "log-log survival",
    main = "log-log curves by prison", col = c("red", "green"))
legend("topright", c("prison=0", "prison=1"), lty = c("solid"), col = c("red", "green"))
```

![](HW4_629Solutions_files/figure-latex/unnamed-chunk-6-2.pdf)<!-- --> 

```r
# log-log curves for dose.group
plot(kmfitDG32, fun = "cloglog", xlab = "time in days on log scale", ylab = "log-log survival",
    main = "log-log curves by dose.group", col = c("red", "green", "blue"))
legend("topleft", c("dose.group=1", "dose.group=2", "dose.group=3"), lty = c("solid"),
    col = c("red", "green", "blue"))
```

![](HW4_629Solutions_files/figure-latex/unnamed-chunk-6-3.pdf)<!-- --> 












