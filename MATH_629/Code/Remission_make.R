setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
RX<-append(rep(0,21),rep(1,21)) 
status<-append(append(rep(1,9),rep(0,12)),rep(1,21))
survt<-c(6, 6, 6, 7, 10, 13, 16, 22, 23, 6, 9, 10, 11, 17, 
        19, 20, 25, 32, 32, 34, 35,1, 1, 2, 2, 3, 4, 4, 5,5,
        58, 8, 8, 8,11, 11, 12, 12, 15, 17, 22, 23) 
Remission=data.frame(RX,status,survt)
save(Remission, file = "Remission.rda")