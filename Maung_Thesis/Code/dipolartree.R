library(data.tree)
#Remission data
X=Remission[,3:5]; Y=Remission[,1]; C=Remission[,2]
v=c(1,-2,-1,1) 

library(data.tree)

dipolartree = function(X,C,Y) {
  #v=dipolarsplit(X,C,Y)
  v=c(-mean(X[,1]),1,0,0); 
  tree = Node$new("root2",v)
  Z=as.matrix(cbind(1,X))  
  splits=Z%*%v
}