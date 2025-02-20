###two time point treatment
###two baseline covariates, two intermediate covariates, one final outcome of interest
#### original = 1 for original dataset
###truncate-weight truncation at some alpha level
###numG-number of runs used in g computation
gcompute=function(dataInput, original=1, numCarlo=2000, visit) {
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
      
      
      
      #### use g-computation to compute beta(Qn) 
      mod2_0=lm(LEU3N2 ~ WBC1 + RBC1 + PLATE1 + LEU2N1 + LEU3N1, data=bootSample[bootSample$A1==0,]) 
      mod2_1=lm(LEU3N2 ~ WBC1 + RBC1 + PLATE1 + LEU2N1 + LEU3N1, data=bootSample[bootSample$A1==1,]) 
      
      mod3_00=lm(LEU3N3 ~ WBC1 + RBC1 + PLATE1 + LEU2N1 + LEU3N1 + LEU3N2, data=bootSample[bootSample$A1==0 & bootSample$A2==0,]) 
      mod3_11=lm(LEU3N3 ~ WBC1 + RBC1 + PLATE1 + LEU2N1 + LEU3N1 + LEU3N2, data=bootSample[bootSample$A1==1 & bootSample$A2==1,]) 
      
      if(visit>=7 & visit <=12){
        
      mod3_01=lm(LEU3N3 ~ WBC1 + RBC1 + PLATE1 + LEU2N1 + LEU3N1 + LEU3N2, data=bootSample[bootSample$A1==0 & bootSample$A2==1,]) 
      mod3_10=lm(LEU3N3 ~ LEU3N2, data=bootSample[bootSample$A1==1 & bootSample$A2==0,]) 
      
      } else {
        
      mod3_01=lm(LEU3N3 ~ LEU3N2, data=bootSample[bootSample$A1==0 & bootSample$A2==1,]) 
      mod3_10=lm(LEU3N3 ~ LEU3N2, data=bootSample[bootSample$A1==1 & bootSample$A2==0,]) 
        
      }
      ############################################################### 
      baselineVar=c("WBC1", "RBC1", "PLATE1", "LEU2N1", "LEU3N1")
      ############################################################### 
      ###simulate potential outcomes under each treatment regime
      Gcomp11=gcomputeFunc(inter.mod1=mod2_1, final.mod=mod3_11, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(1,1) )
      
      Gcomp01=gcomputeFunc(inter.mod1=mod2_0, final.mod=mod3_01, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(0,1) )
      
      Gcomp10=gcomputeFunc(inter.mod1=mod2_1, final.mod=mod3_10, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(1,0) )
      
      Gcomp00=gcomputeFunc(inter.mod1=mod2_0, final.mod=mod3_00, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(0,0) )
      

      outcome.varname="LEU3N3"
      estFinal_msm11=mean(Gcomp11[, outcome.varname])-mean(Gcomp00[, outcome.varname])  
      estFinal_msm10=mean(Gcomp10[, outcome.varname])-mean(Gcomp00[, outcome.varname])  
      estFinal_msm01=mean(Gcomp01[, outcome.varname])-mean(Gcomp00[, outcome.varname])  
      
      
      return( c(estFinal_msm11, estFinal_msm10, estFinal_msm01) )
      
      
    }, error=function(e) return( NA ) )
  
}


