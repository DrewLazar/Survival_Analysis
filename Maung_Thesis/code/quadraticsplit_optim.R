pom = function(s1,s2,eta,zeta){
if (s1$status==1 && s2$status==1)
{
  if (abs(s1$survt-s2$survt)< eta)
  {
    return("pure")
  } else if (abs(s1$survt-s2$survt)< zeta) {
  return ("neither")} else {
    return("mixed")
  } 
} else if (s1$status==1 && s2$status==0) {
  if (s2$survt-s1$survt>zeta) {
    return("pure")
  } else {
  return("neither")
  } 
  }else if (s2$status==1 && s1$status==0) {
    if (s1$survt-s2$survt>zeta) {
      return("pure")
    } else {
      return("neither")
    }
  } else {return("neither")
    }
}

pomarray = function(data,eta,zeta)
{
  n=nrow(data)
 pairmatrix= matrix("neither",n,n)
 for (i in 1:(n-1))
 {
   for (j in (i+1):n)
 {
     pairmatrix[i,j]=pom(data[i,],data[j,],eta,zeta)
   }
 }
 return(pairmatrix)
}
#test pom array
pomarray(Remission,4,10)
 

  
  




