

##########################process the bootstrap estimates ##############
processPENCOMP=function(mulResult){
  
  x=mulResult[1,]
  y=mulResult[2,]
  x=x[!is.na(x)]
  y=y[!is.na(y)]
  
  numT=length(x)
  theta_d=mean(x) 
  Wd=mean(y)  ### within imputation variance
  Bd=(1/(numT-1))*(sum((x-mean(x))^2))###between imputation variance
  Td=Wd+(1+1/numT)*Bd ###total variability associated with mean
  v=(numT-1)*(1+(1/(1/numT+1))*(Wd/Bd))^2 ##degree of freedom
  
  return( c(theta_d[1], sqrt(Td[1]),theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1]))  )
  
}

