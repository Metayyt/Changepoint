###two time point treatment
###two baseline covariates, two intermediate covariates, one final outcome of interest
##
pencomp=function(dataInput, original=1, visit, qcutoff=c(0.025, 0.975)) {
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
      
      ####################################
      ###### propensity model at first time point
      model2a=glm(A1 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + dosage, data=bootSample, family="binomial") 
      temp2a=(bootSample$A1)*model2a$fitted.values + (1-bootSample$A1)*(1-model2a$fitted.values)
      summary(model2a)
      
      ###propensity model at second time point
      model2b=glm(A2 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + A1 + dosage, 
                  data=bootSample, family="binomial") ### correctly specified treatment mechanism 
      temp2b=(bootSample$A2)*model2b$fitted.values + (1-bootSample$A2)*(1-model2b$fitted.values)
      summary(model2b)
      
      bootSample$sw=log((temp2a*temp2b)/(1-temp2a*temp2b))
      
      bootSample$pslogit = log(temp2a/(1-temp2a))  ####here the name pslogit is probability of observed treatment at first time point
      
      
      ##############################################################
      ###fit spline model at first time point under treatment= 0
      bootSample0=bootSample[bootSample$A1==0,]
      pspp0 = pencompFit(y.varname="LEU3N2", x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1"), propen.score=bootSample0$pslogit, data=bootSample0) 
      
      ############# IMPUTE THE MISSING VALUES IN THE ORIGINAL DATA SET#####################################
      newData0=data.frame(RBC1=dataInput$RBC1,LEU3N1=dataInput$LEU3N1, 
                          WBC1=dataInput$WBC1,LEU2N1=dataInput$LEU2N1,
                          PLATE1=dataInput$PLATE1, LEU3N2=rep(NA, dim(dataInput)[1]), 
                          RBC2=rep(NA, dim(dataInput)[1]),
                          college=dataInput$college, white=dataInput$white, age=dataInput$age, 
                          dosage=dataInput$dosage, 
                          A1=rep(0, dim(dataInput)[1]))
      newData0$id=1
      predict2a=1-predict(model2a, newData0, type="response")
      newData0$pslogit=log(predict2a/(1-predict2a))
      
      ###imputing missing potential outcomes under treatment = 0
      predictedM0=imputeF(newdata=newData0, model=pspp0, x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1"), propen.score.new=newData0$pslogit) 
      predictedM0[which(predictedM0<0)]=0
      newData0$LEU3N2=predictedM0
      newData0$LEU3N2[which(!is.na(dataInput$LEU3N2) & (dataInput$A1==0))]=dataInput$LEU3N2[which(!is.na(dataInput$LEU3N2) & (dataInput$A1==0))]
      
      
      ##############################################################
      ###fit spline model at second time point under treatment = 1
      bootSample1=bootSample[bootSample$A1==1,]
      pspp1 = pencompFit(y.varname="LEU3N2", x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1"), propen.score=bootSample1$pslogit, data=bootSample1) 
      
      
      
      #######################imputing the missing potential outcomes under treatment = 1##################
      newData1=data.frame(RBC1=dataInput$RBC1,LEU3N1=dataInput$LEU3N1, 
                          WBC1=dataInput$WBC1,LEU2N1=dataInput$LEU2N1, PLATE1=dataInput$PLATE1, LEU3N2=rep(NA, dim(dataInput)[1]), 
                          RBC2=rep(NA, dim(dataInput)[1]),  
                          college=dataInput$college, white=dataInput$white, age=dataInput$age,
                          dosage=dataInput$dosage, 
                          A1=rep(1, dim(dataInput)[1]))
      newData1$id=1
      predict2a=predict(model2a, newData1, type="response")
      newData1$pslogit=log(predict2a/(1-predict2a))
      
      predictedM1=imputeF(newdata=newData1, model=pspp1, x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1"), propen.score.new=newData1$pslogit)
      predictedM1[which(predictedM1<0)]=0
      newData1$LEU3N2=predictedM1
      newData1$LEU3N2[which(!is.na(dataInput$LEU3N2) & (dataInput$A1==1))]=dataInput$LEU3N2[which(!is.na(dataInput$LEU3N2) & (dataInput$A1==1))]
      
      mean(newData1$LEU3N2 - newData0$LEU3N2)
      
      #############################################################################################################################
      ####################IMPUTING THE MISSING VALUES IN THE ORIGINAL DATA SET################################
      newData00=data.frame(RBC1=dataInput$RBC1,LEU3N1=dataInput$LEU3N1, 
                           WBC1=dataInput$WBC1,LEU2N1=dataInput$LEU2N1, PLATE1=dataInput$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                           college=dataInput$college, white=dataInput$white, age=dataInput$age,
                           dosage=dataInput$dosage, 
                           A1=rep(0, dim(dataInput)[1]), A2=rep(0, dim(dataInput)[1]))
      predict2a=1-predict(model2a, newData00, type="response")
      predict2b=1-predict(model2b, newData00, type="response")
      newData00$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
      newData00$id=1
      
      
      newData01=data.frame(RBC1=dataInput$RBC1,LEU3N1=dataInput$LEU3N1, 
                           WBC1=dataInput$WBC1,LEU2N1=dataInput$LEU2N1, PLATE1=dataInput$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                           college=dataInput$college, white=dataInput$white, age=dataInput$age, 
                           dosage=dataInput$dosage, 
                           A1=rep(0, dim(dataInput)[1]), A2=rep(1, dim(dataInput)[1]))
      predict2a=1-predict(model2a, newData01, type="response")    
      predict2b=predict(model2b, newData01, type="response")
      newData01$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
      newData01$id=1
      
      
      newData10=data.frame(RBC1=dataInput$RBC1,LEU3N1=dataInput$LEU3N1, 
                           WBC1=dataInput$WBC1,LEU2N1=dataInput$LEU2N1, PLATE1=dataInput$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, 
                           college=dataInput$college, white=dataInput$white, age=dataInput$age, 
                           dosage=dataInput$dosage, 
                           A1=rep(1, dim(dataInput)[1]), A2=rep(0, dim(dataInput)[1]))
      predict2a=predict(model2a, newData10, type="response")    
      predict2b=1-predict(model2b, newData10, type="response")
      newData10$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
      newData10$id=1
      
      
      newData11=data.frame(RBC1=dataInput$RBC1,LEU3N1=dataInput$LEU3N1, 
                           WBC1=dataInput$WBC1,LEU2N1=dataInput$LEU2N1, PLATE1=dataInput$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, 
                           college=dataInput$college, white=dataInput$white, age=dataInput$age, 
                           dosage=dataInput$dosage, 
                           A1=rep(1, dim(dataInput)[1]), A2=rep(1, dim(dataInput)[1]))
      predict2a=predict(model2a, newData11, type="response")    
      predict2b=predict(model2b, newData11, type="response")
      newData11$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
      newData11$id=1
      
      
      #######################################################################################################
      ###use equally spaced fixed knots assuming K knots
      bootSample00=bootSample[bootSample$A1==0 & bootSample$A2==0,]
      pspp00=pencompFit(y.varname="LEU3N3", x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1", "LEU3N2"), propen.score=bootSample00$sw, data=bootSample00) 
      
      ###imputing missing potential outcomes under treatment sequence 00
      imputed00=imputeF(newdata=newData00, model=pspp00, x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1", "LEU3N2"), propen.score.new=newData00$sw)
      
      ############################################################################################
      ############################################################################################
      #######################################################################################################
      ###use equally spaced fixed knots assuming K knots
      bootSample01=bootSample[bootSample$A1==0 & bootSample$A2==1,]
      pspp01=NULL
      imputed01=NULL
      
      if(visit >= 13 & visit <=21){
        
        pspp01=pencompFit(y.varname="LEU3N3", x.varnames=c("LEU3N2"), propen.score=bootSample01$sw, data=bootSample01) 
        
        ###imputing missing potential outcomes under treatment sequence 00
        imputed01=imputeF(newdata=newData01, model=pspp01, x.varnames=c("LEU3N2"), propen.score.new=newData01$sw)
        
        
      } else {
        
        pspp01=pencompFit(y.varname="LEU3N3", x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1", "LEU3N2"), propen.score=bootSample01$sw, data=bootSample01) 
        ###imputing missing potential outcomes under treatment sequence 00
        imputed01=imputeF(newdata=newData01, model=pspp01, x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1", "LEU3N2"), propen.score.new=newData01$sw)
        
        
      }
      
      summary(pspp01)
      
      ############################################################################################
      ############################################################################################
      #######################################################################################################
      ###there are very few subjects with treatment sequence of 10, if number of observation is too small, spline model does not converge, so use linear regression model only
      bootSample10=bootSample[bootSample$A1==1 & bootSample$A2==0,]
      pspp10=NULL
      imputed10=NULL
      
      pspp10=pencompFit(y.varname="LEU3N3", x.varnames=c("LEU3N2"), propen.score=bootSample10$sw, data=bootSample10) 
      ###imputing missing potential outcomes under treatment sequence 00
      imputed10=imputeF(newdata=newData10, model=pspp10, x.varnames=c("LEU3N2"), propen.score.new=newData10$sw)
        

      
      summary(pspp10)
      
      
      ############################################################################################
      #######################################################################################################
      ###use equally spaced fixed knots assuming K knots
      bootSample11=bootSample[bootSample$A1==1 & bootSample$A2==1,]
      pspp11=NULL
      imputed11=NULL
      
      pspp11=pencompFit(y.varname="LEU3N3", x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1", "LEU3N2"), propen.score=bootSample11$sw, data=bootSample11) 
      ###imputing missing potential outcomes under treatment sequence 00
      imputed11=imputeF(newdata=newData11, model=pspp11, x.varnames=c("WBC1", "PLATE1", "RBC1", "LEU3N1", "LEU3N2"), propen.score.new=newData11$sw)

      
      summary(pspp11)
      
      ###replace negative values with 0
      imputed00[which(imputed00<0)]=0
      imputed01[which(imputed01<0)]=0
      imputed10[which(imputed10<0)]=0
      imputed11[which(imputed11<0)]=0
      
      
      
      
      ################# inclusion and exclusion criteria ################################################
    
      ####################################################################################################
      ##FIND THE OVERLAPPING REGIONS AT FIRST TIME POINT ##################################################
      ####looking of probability of getting treated for both control and treated groups
      probTreat=predict(model2a, dataInput, type="response")
      summary(probTreat)
      overlapTreat=quantile(probTreat[which(dataInput$A1==1)], probs = qcutoff)
      
      
      ##############################
      ####looking of probability of getting treated for both control and treated groups
      probControl=1-predict(model2a, dataInput, type="response")
      overlapControl=quantile(probControl[which(dataInput$A1==0)], probs = qcutoff)
      
      dataInput$includedT1=(probTreat >= overlapTreat[1] &  probTreat <= overlapTreat[2]) *
        (probControl >= overlapControl[1] &  probControl <= overlapControl[2])
      sum(dataInput$includedT1)  
      
      
      #################################################################################################
      ########based on estimated propensity of getting treatment pattern 11
      group11=newData11$sw[dataInput$A1==1 & dataInput$A2==1]
      overlap=quantile(group11, probs = qcutoff)
      sum((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))/dim(newData11)[1]
      
      dataInput$included11=0  ###included for 01
      dataInput$included11[which((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))]=1
      sum(dataInput$included11)
      
      rm(overlap)
      
      
      ####################################################################################
      ########based on estimated propensity of getting treatment pattern 00
      group00=newData00$sw[dataInput$A1==0 & dataInput$A2==0]
      overlap=quantile(group00, probs = qcutoff)
      sum((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))/dim(newData00)[1]
      
      dataInput$included00=0  ###included for 00
      dataInput$included00[which((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))]=1
      sum(dataInput$included00)
      
      rm(overlap)
      
      
      
      ####################################################################################
      ########based on estimated propensity of getting treatment pattern 01
      group01=newData01$sw[dataInput$A1==0 & dataInput$A2==1]
      overlap=quantile(group01, probs = qcutoff)
      sum((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))/dim(newData01)[1]
      
      
      dataInput$included01=0  ###included for 00
      dataInput$included01[which((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))]=1
      sum(dataInput$included01)
      rm(overlap)
      
      ####################################################################################
      ########based on estimated propensity of getting treatment pattern 00
      group10=newData10$sw[dataInput$A1==1 & dataInput$A2==0]
      overlap=quantile(group10, probs = qcutoff)
      sum((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))/dim(newData10)[1]
      
      
      dataInput$included10=0  ###included for 00
      dataInput$included10[which((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))]=1
      sum(dataInput$included10)
      
      rm(overlap)
      
      
      
      dataInput$includeAll_01=as.numeric(dataInput$included00==1 & dataInput$included01==1 & dataInput$includedT1==1)
      sum(dataInput$includeAll_01)
      
      
      dataInput$includeAll_11=as.numeric(dataInput$included00==1 & dataInput$included11==1 & dataInput$includedT1==1)
      sum(dataInput$includeAll_11)
      
      
      dataInput$includeAll_10=as.numeric(dataInput$included00==1 & dataInput$included10==1 & dataInput$includedT1==1)
      sum(dataInput$includeAll_10)
      

      summary(imputed00)
      summary(imputed01)
      summary(imputed10)
      summary(imputed11)
      
      
      ####ORIGINAL DATASET  
      y_00=imputed00  
      y_01=imputed01
      y_10=imputed10  
      y_11=imputed11
      
      ###predict y when A0=0 and A1=0 
      y_00[which(dataInput$A1==0 & dataInput$A2==0)] = dataInput$LEU3N3[which(dataInput$A1==0 & dataInput$A2==0)]
      y_01[which(dataInput$A1==0 & dataInput$A2==1)] = dataInput$LEU3N3[which(dataInput$A1==0 & dataInput$A2==1)]
      y_10[which(dataInput$A1==1 & dataInput$A2==0)] = dataInput$LEU3N3[which(dataInput$A1==1 & dataInput$A2==0)]
      y_11[which(dataInput$A1==1 & dataInput$A2==1)] = dataInput$LEU3N3[which(dataInput$A1==1 & dataInput$A2==1)]
      
      
      mean(y_11)
      mean(y_10)
      mean(y_01)
      mean(y_00)
      
      #################################################################################################
      ### Without excluding nonoverlaps  estimate from each imputation ####
      ###including everyone
      estimate11_all = y_11-y_00
      estimate10_all = y_10-y_00
      estimate01_all = y_01-y_00
      
      ###excluding subjects outside of overlap regions
      estimate11_rest = (y_11-y_00)[which(dataInput$includeAll_11==1)]
      estimate10_rest = (y_10-y_00)[which(dataInput$includeAll_10==1)]
      estimate01_rest = (y_01-y_00)[which(dataInput$includeAll_01==1)]
      
      
      ###sample size for each treatment regime, before and after excluding subjects outside overlap region
      sampleNumBoot=c(sum(dataInput$A1==1 & dataInput$A2==1),
                      sum(dataInput$A1==1 & dataInput$A2==0),
                      sum(dataInput$A1==0 & dataInput$A2==1),
                      sum(dataInput$A1==0 & dataInput$A2==0), 
                      sum(dataInput$includeAll_11), sum(dataInput$includeAll_10), sum(dataInput$includeAll_01), dim(dataInput)[1])
      ###the entries are num11, num10, num01, num00, num11_include, num10_include, num01_include, total
      ###(the last 3 are the number of subjects included after excluding subject outside overlap region)
      
      
      ###including everyone
      return( c(mean(estimate11_all), var(estimate11_all)/length(estimate11_all), 
                mean(estimate10_all), var(estimate10_all)/length(estimate10_all),
                mean(estimate01_all), var(estimate01_all)/length(estimate01_all),
                
                ###restrict to overlap regions
                mean(estimate11_rest), var(estimate11_rest)/length(estimate11_rest), 
                mean(estimate10_rest), var(estimate10_rest)/length(estimate10_rest),
                mean(estimate01_rest), var(estimate01_rest)/length(estimate01_rest),
                
                sampleNumBoot ###sample size
                
      ) )
      
    }, error=function(e) return( NA ) )
  
}


