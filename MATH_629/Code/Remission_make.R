#Set work directory 
setwd("C:/GitStuff/Survival_Analysis/MATH_629/Data")
#Make our Remission data in R in the 'first way' in
  #chapter 1, slide 8.    
#Make a vector of 21 0's for group 1 and 21 1's for group 2
  #and append them
TR<-append(rep(0,21),rep(1,21)) 
#Make a column for our status variable 
status<-append(append(rep(1,9),rep(0,12)),rep(1,21))
#Make a column for our survival times 
survt<-c(6, 6, 6, 7, 10, 13, 16, 22, 23, 6, 9, 10, 11, 17, 
        19, 20, 25, 32, 32, 34, 35,1, 1, 2, 2, 3, 4, 4, 5,5,
        8, 8, 8,8,11, 11, 12, 12, 15, 17, 22, 23) 
#Make a data frame with TR, status, survt 
Remission=data.frame(TR,status,survt)
#Save the Remission data in the work directory 
save(Remission, file = "Remission.rda")
#Add LogWBC
logWBC<-c(2.31,4.06,3.28,4.43,2.96,2.88,3.60,2.32,2.57,3.20,
          2.80,2.70,2.60,2.16,2.05,2.01,1.78,2.20,2.53,1.47,
          1.45,2.80,5.00,4.91,4.48,4.01,4.36,2.42,3.49,3.97,
          3.52,3.05,2.32,3.26,3.49,2.12,1.50,3.06,2.30,2.95,
          2.73,1.97)
Remission=data.frame(TR,status,survt,logWBC)
#Add strata LogWBC.group 
Remission$LogWBC.group<-cut(Remission$logWBC,c(1.44,2.30,2.96,5.00),labels=c('1','2','3'))
save(Remission, file = "Remission.rda")