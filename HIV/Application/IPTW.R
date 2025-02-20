###two time point treatment
###two baseline covariates, two intermediate covariates, one final outcome of interest
#### original = 1 for original dataset
###truncate-weight truncation at some alpha level
IPTW=function(dataInput, original=1, truncate=0.05) {
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
      
      ############ estimating the weight ##############################
      model1a=glm(A1 ~ 1, data=bootSample, family="binomial") 
      temp1a=(bootSample$A1)*model1a$fitted.values + (1-bootSample$A1)*(1-model1a$fitted.values)
      summary(model1a)
      
      model1b=glm(A2 ~ A1, data=bootSample, family="binomial") ### correctly specified treatment mechanism 
      temp1b=(bootSample$A2)*model1b$fitted.values + (1-bootSample$A2)*(1-model1b$fitted.values)
      summary(model1b)
      
      ######
      model2a=glm(A1 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + dosage, data=bootSample, family="binomial") 
      temp2a=(bootSample$A1)*model2a$fitted.values + (1-bootSample$A1)*(1-model2a$fitted.values)
      summary(model2a)
      
      model2b=glm(A2 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + A1 + dosage, 
                  data=bootSample, family="binomial") ### correctly specified treatment mechanism 
      temp2b=(bootSample$A2)*model2b$fitted.values + (1-bootSample$A2)*(1-model2b$fitted.values)
      summary(model2b)
      
      
      bootSample$weight=(temp1a * temp1b)/(temp2a * temp2b)

      ####################################################################################
      ### E(u11)
      term1a_=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==1 & bootSample$A2==1))/sum(bootSample$weight * as.numeric(bootSample$A1==1 & bootSample$A2==1))
      
      ####################################################################################
      ### E(u10)
      term1b_=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==1 & bootSample$A2==0))/sum(bootSample$weight * as.numeric(bootSample$A1==1 & bootSample$A2==0))

      ####################################################################################
      ### E(u01)
      term1c_=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==0 & bootSample$A2==1))/sum(bootSample$weight * as.numeric(bootSample$A1==0 & bootSample$A2==1))

      ####################################################################################
      ### E(u00)
      term1d_=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==0 & bootSample$A2==0))/sum(bootSample$weight * as.numeric(bootSample$A1==0 & bootSample$A2==0))

      #########including everyone ATE
      estimate11_all=term1a_-term1d_ 
      estimate10_all=term1b_-term1d_  
      estimate01_all=term1c_-term1d_  
      
      
      ##################with weight truncation########################
      cutoff_boot=quantile(bootSample$weight, probs =c(truncate, 1-truncate) )  ### 
      bootSample$weight[which(bootSample$weight<cutoff_boot[1])]=cutoff_boot[1]
      bootSample$weight[which(bootSample$weight>cutoff_boot[2])]=cutoff_boot[2]
      
      #######recalculate the estimates after weight truncation at some alpha level
      ####################################################################################
      ### E(u11)
      term1a=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==1 & bootSample$A2==1))/sum(bootSample$weight * as.numeric(bootSample$A1==1 & bootSample$A2==1))
      term1a
      
      ####################################################################################
      ### E(u10)
      term1b=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==1 & bootSample$A2==0))/sum(bootSample$weight * as.numeric(bootSample$A1==1 & bootSample$A2==0))
      term1b
      
      ####################################################################################
      ### E(u01)
      term1c=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==0 & bootSample$A2==1))/sum(bootSample$weight * as.numeric(bootSample$A1==0 & bootSample$A2==1))
      term1c
      
      ####################################################################################
      ### E(u00)
      term1d=sum(bootSample$weight * bootSample$LEU3N3 * as.numeric(bootSample$A1==0 & bootSample$A2==0))/sum(bootSample$weight * as.numeric(bootSample$A1==0 & bootSample$A2==0))
      term1d
      
      estimate11_trunc=term1a - term1d 
      estimate10_trunc=term1b - term1d  
      estimate01_trunc=term1c - term1d  
      
      return( c(mean(estimate11_all), mean(estimate10_all), mean(estimate01_all),
                mean(estimate11_trunc), mean(estimate10_trunc), mean(estimate01_trunc) ) )
      
    }, error=function(e) return( NA ) )
  
}


