

#########################################################
###assume a truncated linear basis
### right now it only supports truncated linear basis with equally spaced knot
### will expand on this later
###x.varnames enter as c("L1", "L3"), separate variable names by comma
pencompFit=function(y.varname, x.varnames, propen.score, data) {
  
  library("nlme")
  K1=min(c(floor(0.25*length(unique(propen.score))),35))
  space=(max(propen.score)-min(propen.score))/(K1+1)
  knots=(min(propen.score)+space*(1:K1))
  
  linearB=NULL
  linearB =outer(propen.score, knots, "-")
  linearB =linearB * (linearB > 0)
  
  response=data[, y.varname]
  covariateX=NULL
  for(i in 1:length(x.varnames)){
    covariateX=cbind(covariateX, data[, x.varnames[i] ])
  }
  covariateX=cbind(rep(1,nrow(data)), covariateX, propen.score)
  
  
  all=rep(1, dim(data)[1])
  psppM=lme(response ~ covariateX-1, random=list(all=pdIdent(~0+linearB)) ) 
  fixCoef=psppM$coefficients$fixed
  names(fixCoef)=c("intercept", x.varnames, "propen.score")
  randCoef=psppM$coefficients$random$all
  
  return(list(fixed=fixCoef, random=randCoef, knot.loc=knots, sigmaRes=psppM$sigma))
  
}






######################################################################
##########prediction values############################################ 
###assume a truncated linear basis
##model is fitted model from lmeFit
###imputing missing potential outcomes 

imputeF=function(newdata, model, x.varnames, propen.score.new) {
  
  knots=model$knot.loc
  
  linearB=NULL
  linearB =outer(propen.score.new, knots, "-")
  linearB =linearB * (linearB > 0)
  
  designM=cbind(rep(1,nrow(newdata)), newdata[, x.varnames], propen.score.new)
  
  designM=as.matrix(designM)
  predicted = designM %*% model$fixed + as.matrix(linearB) %*% as.vector(unlist(model$random)) + rnorm(nrow(newdata), 0, model$sigmaRes)
  
  return(predicted)
  
}


