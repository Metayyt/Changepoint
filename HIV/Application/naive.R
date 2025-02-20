###two time point treatment
###two baseline covariates, two intermediate covariates, one final outcome of interest
##
naive=function(dataInput, original=1) {
  tryCatch ( 
    {
      
      bootSample=NULL
      
      if (original==1){
        
        bootSample=dataInput
        
      } else {
        
        ###stratified bootstrap
        num00=which( (dataInput[,"A1"]==0) & (dataInput[,"A2"]==0) )
        num01=which( (dataInput[,"A1"]==0) & (dataInput[,"A2"]==1) )
        num10=which( (dataInput[,"A1"]==1) & (dataInput[,"A2"]==0) )
        num11=which( (dataInput[,"A1"]==1) & (dataInput[,"A2"]==1) )
        
        bootSample=dataInput[c(sample(x=num00, size=length(num00),replace=T),
                               sample(x=num01, size=length(num01),replace=T),
                               sample(x=num10, size=length(num10),replace=T),
                               sample(x=num11, size=length(num11),replace=T)),]
        
      }
      
      estimate11_all=mean(bootSample$LEU3N3[(bootSample$A1==1 & bootSample$A2==1)], na.rm=T)-mean(bootSample$LEU3N3[(bootSample$A1==0 & bootSample$A2==0)], na.rm=T) 
      estimate10_all=mean(bootSample$LEU3N3[(bootSample$A1==1 & bootSample$A2==0)], na.rm=T)-mean(bootSample$LEU3N3[(bootSample$A1==0 & bootSample$A2==0)], na.rm=T)  
      estimate01_all=mean(bootSample$LEU3N3[(bootSample$A1==0 & bootSample$A2==1)], na.rm=T)-mean(bootSample$LEU3N3[(bootSample$A1==0 & bootSample$A2==0)], na.rm=T)  ##
      

      return( c(mean(estimate11_all),mean(estimate10_all),mean(estimate01_all) ) )
      
    }, error=function(e) return( NA ) )
  
}


