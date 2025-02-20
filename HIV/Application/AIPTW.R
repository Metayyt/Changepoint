###
#####this calculate aiptw estimate
####dataInput--input dataset
###original=1 for original dataset, 0 for bootstrap sample
###numCarlo=2000: number of replicates to calculate the augmentation term
###truncate-truncate at some alpha level

AIPTW= function(dataInput, baselineVar, original=1, numCarlo=2000, truncation=0.05, visit) { 
  
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
      ###weight truncations
      cutoff=quantile(bootSample$weight, probs = c(truncation, 1-truncation) ) 
      
      if(length(truncation)!=0){ ###with weight truncation

        bootSample$weight[which(bootSample$weight<cutoff[1])]=cutoff[1]
        bootSample$weight[which(bootSample$weight>cutoff[2])]=cutoff[2]
        
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
      ###simulate potential outcomes under each treatment regime
      Gcomp11=gcomputeFunc(inter.mod1=mod2_1, final.mod=mod3_11, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(1,1) )
      
      Gcomp01=gcomputeFunc(inter.mod1=mod2_0, final.mod=mod3_01, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(0,1) )
      
      Gcomp10=gcomputeFunc(inter.mod1=mod2_1, final.mod=mod3_10, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(1,0) )
      
      Gcomp00=gcomputeFunc(inter.mod1=mod2_0, final.mod=mod3_00, data=bootSample, baselineVar=baselineVar, numCarlo=numCarlo, treatSeq=c(0,0) )
      
      
      Gcomp=rbind(Gcomp11, Gcomp10, Gcomp01, Gcomp00) 
      dim(Gcomp) 
      
      ##get the treatment coefficients in the MSM model #### 
      designMat=cbind(Gcomp[, which(names(Gcomp)=="A1")], Gcomp[, which(names(Gcomp)=="A2")],
                      Gcomp[, which(names(Gcomp)=="A1")] * Gcomp[, which(names(Gcomp)=="A2")])
      designY=Gcomp[, which(names(Gcomp)=="LEU3N3")]
      msmModel=lm(designY ~ designMat, data=Gcomp) 
      summary(msmModel)
      
      
      
      ####################################################################  
      ########## first term in the augmentation term #####################
      ########here I calculate the augmentation term, in a two time point treatment, there are 4 sub parts in the augmentation term
      d=matrix(NA, nrow = nrow(bootSample), ncol=4)
      for (numSubj in 1:nrow(bootSample)){  ###for each individual, I simulate numCarlo replicates
        
        currentData=NULL ##store baseline coariate
        ### generate L0 from empirical distribution of L0 obtained from the data
        for(ind in 1:length(baselineVar)){
          draw_g=NULL
          draw_g=cbind(draw_g, rep(as.numeric(bootSample[numSubj, baselineVar[ind] ]), numCarlo))
          colnames(draw_g)=baselineVar[ind]
          currentData=cbind(currentData, draw_g)
          
        }
        currentData=data.frame(currentData, A1=bootSample[numSubj, "A1"])
        
        
        ### intermediate outcome L1 first expectation given history up to time point 0 (baseline) 
        L1_mean0=predict(mod2_0, newdata = currentData) 
        L1_draw0=rnorm(numCarlo, mean=L1_mean0, sd=summary(mod2_0)$sigma)   
        L1_draw0[which(L1_draw0<0)]=0
        
        L1_mean1=predict(mod2_1, newdata = currentData) 
        L1_draw1=rnorm(numCarlo, mean=L1_mean1, sd=summary(mod2_1)$sigma)  
        L1_draw1[which(L1_draw1<0)]=0
        firstTreat = currentData[, "A1"]
        L1_draw=L1_draw0*(1 - firstTreat) + L1_draw1 * firstTreat
        currentData[, "LEU3N2"] = L1_draw
        
        
        prob=predict(model2b, newdata = currentData, type="response") 
        
        A1_draw=rbinom(numCarlo,1,prob) ### draw treatment at time point 2 
        currentData[, "A2"] = A1_draw
        
        y_mean00=predict(mod3_00, newdata = currentData) 
        y_draw00=rnorm(numCarlo, mean=y_mean00, sd=summary(mod3_00)$sigma) 
        y_draw00[which(y_draw00<0)]=0
        
        ##########################################################
        y_mean01=predict(mod3_01, newdata = currentData) 
        y_draw01=rnorm(numCarlo, mean=y_mean01, sd=summary(mod3_01)$sigma) 
        y_draw01[which(y_draw01<0)]=0
        
        #############################################################
        y_mean10=predict(mod3_10, newdata = currentData) 
        y_draw10=rnorm(numCarlo, mean=y_mean10, sd=summary(mod3_10)$sigma) 
        y_draw10[which(y_draw10<0)]=0
        
        #############################################################
        y_mean11=predict(mod3_11, newdata = currentData) 
        y_draw11=rnorm(numCarlo, mean=y_mean11, sd=summary(mod3_11)$sigma) 
        y_draw11[which(y_draw11<0)]=0
        
        secondTreat = currentData[, "A2"]
        currentData[, "LEU3N3"]=y_draw11 * as.numeric(firstTreat==1 & secondTreat==1) + y_draw10*as.numeric(firstTreat==1 & secondTreat==0) +
          y_draw01*as.numeric(firstTreat==0 & secondTreat==1) + y_draw00*as.numeric(firstTreat==0 & secondTreat==0)
        
        temp1a_mc_=predict(model1a, currentData, type="response") 
        temp1a_mc=firstTreat * temp1a_mc_ + (1-firstTreat)*(1-temp1a_mc_)
        
        temp1b_mc_=predict(model1b, currentData, type="response") 
        temp1b_mc=secondTreat * temp1b_mc_ + (1-secondTreat)*(1-temp1b_mc_)
        
        temp2a_mc_=predict(model2a, currentData, type="response") 
        temp2a_mc=firstTreat * temp2a_mc_ + (1-firstTreat)*(1-temp2a_mc_)
        
        temp2b_mc_=predict(model2b, currentData, type="response") 
        temp2b_mc=secondTreat * temp2b_mc_ + (1-secondTreat)*(1-temp2b_mc_)
        
        weight_mc=(temp1a_mc * temp1b_mc)/(temp2a_mc * temp2b_mc) 
        
        
        ###weight trunation
        if(length(truncation) !=0){
          
          weight_mc[which(weight_mc<cutoff[1])]=cutoff[1]
          weight_mc[which(weight_mc>cutoff[2])]=cutoff[2]
          
        }
        
        
        temp=weight_mc * (currentData[, "LEU3N3"] - (cbind(rep(1,numCarlo), firstTreat, secondTreat, firstTreat * secondTreat) %*% msmModel$coef)) 
        d[numSubj,]=c(mean(temp), mean(firstTreat * temp), mean(secondTreat * temp), mean(firstTreat * secondTreat * temp)) 
        
      }
      
      
      
      ##################################################################################################### 
      ### second expectation given history up to time point 1  
      ###################################################################################
      ##################################################################################
      
      d2=matrix(NA, nrow=nrow(bootSample), ncol=4)
      
      for(numSubj in 1:nrow(bootSample)){
        
        currentData=NULL
        ########### generate second intermediate outcome L1b 
        includeVar = NULL
        includeVar = c(baselineVar, "LEU3N2", "A1", "A2")
        for(ind in 1:length(includeVar)){
          draw_g=rep(as.numeric(bootSample[numSubj, includeVar[ind]]), numCarlo)
          currentData=cbind(currentData, draw_g)
        }
        currentData=data.frame(currentData)
        names(currentData)=includeVar

        ###########################
        y_mean00=predict(mod3_00, newdata = currentData) 
        y_draw00=rnorm(numCarlo, mean=y_mean00, sd=summary(mod3_00)$sigma) 
        y_draw00[which(y_draw00<0)]=0
        
        ###########################
        y_mean01=predict(mod3_01, newdata = currentData) 
        y_draw01=rnorm(numCarlo, mean=y_mean01, sd=summary(mod3_01)$sigma) 
        y_draw01[which(y_draw01<0)]=0
        
        ###########################
        y_mean10=predict(mod3_10, newdata = currentData) 
        y_draw10=rnorm(numCarlo, mean=y_mean10, sd=summary(mod3_10)$sigma) 
        y_draw10[which(y_draw10<0)]=0
        
        ###########################
        y_mean11=predict(mod3_11, newdata = currentData) 
        y_draw11=rnorm(numCarlo, mean=y_mean11, sd=summary(mod3_11)$sigma) 
        y_draw11[which(y_draw11<0)]=0
        
        firstTreat = currentData[, "A1"]
        secondTreat = currentData[, "A2"]
        currentData[, "LEU3N3"]=y_draw11 * as.numeric(firstTreat==1 & secondTreat==1) + y_draw10*as.numeric(firstTreat==1 & secondTreat==0) +
          y_draw01*as.numeric(firstTreat==0 & secondTreat==1) + y_draw00*as.numeric(firstTreat==0 & secondTreat==0)
        
        temp1a_mc_=predict(model1a, currentData, type="response") 
        temp1a_mc=firstTreat * temp1a_mc_ + (1-firstTreat)*(1-temp1a_mc_)
        
        temp1b_mc_=predict(model1b, currentData, type="response") 
        temp1b_mc=secondTreat * temp1b_mc_ + (1-secondTreat)*(1-temp1b_mc_)
        
        temp2a_mc_=predict(model2a, currentData, type="response") 
        temp2a_mc=firstTreat * temp2a_mc_ + (1-firstTreat)*(1-temp2a_mc_)
        
        temp2b_mc_=predict(model2b, currentData, type="response") 
        temp2b_mc=secondTreat * temp2b_mc_ + (1-secondTreat)*(1-temp2b_mc_)
        
        weight_mc=(temp1a_mc * temp1b_mc)/(temp2a_mc * temp2b_mc) 
        
        ###weight truncation
        if(length(truncation) !=0){
          
          weight_mc[which(weight_mc<cutoff[1])]=cutoff[1]
          weight_mc[which(weight_mc>cutoff[2])]=cutoff[2]
          
        }
        
        temp=weight_mc * (currentData[, "LEU3N3"] - (cbind(rep(1,numCarlo), firstTreat, secondTreat, firstTreat * secondTreat) %*% msmModel$coef)) 
        d2[numSubj,]=c(mean(temp), mean(firstTreat * temp), mean(secondTreat * temp), mean(firstTreat * secondTreat * temp)) 
        
      }
      
      d_sum=d+d2 
      
      
      #######################################################################
      #######################################################################
      ########################################################################################################### 
      ### first expectation given history up to time poin 0 (baseline) 
      d_=matrix(NA, nrow=nrow(bootSample), ncol=4)
      for(numSubj in 1:nrow(bootSample)){
        
        currentData=NULL
        ########### generate second intermediate outcome L1b 
        includeVar = NULL
        includeVar = baselineVar
        for(ind in 1:length(includeVar)){
          draw_g=rep(as.numeric(bootSample[numSubj, includeVar[ind]]), numCarlo)
          currentData=cbind(currentData, draw_g)
        }
        currentData=data.frame(currentData)
        names(currentData)=includeVar
        
        
        #######################################################
        ######first generate the first treatment ###############
        prob1=predict(model2a, newdata = currentData, type = "response")
        A0_draw=rbinom(numCarlo, 1, prob = prob1) ### draw treatment at time point 2 
        currentData[, "A1"]=A0_draw
        
        
        ### intermediate outcome L1 first expectation given history up to time point 0 (baseline) 
        L1_mean0=predict(mod2_0, newdata = currentData) 
        L1_draw0=rnorm(numCarlo, mean=L1_mean0, sd=summary(mod2_0)$sigma)
        L1_draw0[which(L1_draw0<0)]=0
        
        L1_mean1=predict(mod2_1, newdata = currentData) 
        L1_draw1=rnorm(numCarlo, mean=L1_mean1, sd=summary(mod2_1)$sigma)  
        L1_draw1[which(L1_draw1<0)]=0
        
        firstTreat = currentData[, "A1"]
        L1_draw=L1_draw0*(1 - firstTreat) + L1_draw1 * firstTreat
        currentData[, "LEU3N2"] = L1_draw
        
        
        prob=predict(model2b, newdata = currentData, type="response") 
        
        A1_draw=rbinom(numCarlo,1,prob) ### draw treatment at time point 2 
        currentData[, "A2"] = A1_draw
        
        y_mean00=predict(mod3_00, newdata = currentData) 
        y_draw00=rnorm(numCarlo, mean=y_mean00, sd=summary(mod3_00)$sigma) 
        y_draw00[which(y_draw00<0)]=0
        
        ##########################################################
        y_mean01=predict(mod3_01, newdata = currentData) 
        y_draw01=rnorm(numCarlo, mean=y_mean01, sd=summary(mod3_01)$sigma) 
        y_draw01[which(y_draw01<0)]=0
        
        #############################################################
        y_mean10=predict(mod3_10, newdata = currentData) 
        y_draw10=rnorm(numCarlo, mean=y_mean10, sd=summary(mod3_10)$sigma) 
        y_draw10[which(y_draw10<0)]=0
        
        #############################################################
        y_mean11=predict(mod3_11, newdata = currentData) 
        y_draw11=rnorm(numCarlo, mean=y_mean11, sd=summary(mod3_11)$sigma) 
        y_draw11[which(y_draw11<0)]=0
        
        secondTreat = currentData[, "A2"]
        currentData[, "LEU3N3"]=y_draw11 * as.numeric(firstTreat==1 & secondTreat==1) + y_draw10*as.numeric(firstTreat==1 & secondTreat==0) +
          y_draw01*as.numeric(firstTreat==0 & secondTreat==1) + y_draw00*as.numeric(firstTreat==0 & secondTreat==0)
        
        temp1a_mc_=predict(model1a, currentData, type="response") 
        temp1a_mc=firstTreat * temp1a_mc_ + (1-firstTreat)*(1-temp1a_mc_)
        
        temp1b_mc_=predict(model1b, currentData, type="response") 
        temp1b_mc=secondTreat * temp1b_mc_ + (1-secondTreat)*(1-temp1b_mc_)
        
        temp2a_mc_=predict(model2a, currentData, type="response") 
        temp2a_mc=firstTreat * temp2a_mc_ + (1-firstTreat)*(1-temp2a_mc_)
        
        temp2b_mc_=predict(model2b, currentData, type="response") 
        temp2b_mc=secondTreat * temp2b_mc_ + (1-secondTreat)*(1-temp2b_mc_)
        
        weight_mc=(temp1a_mc * temp1b_mc)/(temp2a_mc * temp2b_mc) 
        
        
        if(length(truncation) !=0){
          
          weight_mc[which(weight_mc<cutoff[1])]=cutoff[1]
          weight_mc[which(weight_mc>cutoff[2])]=cutoff[2]
          
        }
        
        temp=weight_mc * (currentData[, "LEU3N3"] - (cbind(rep(1,numCarlo), firstTreat, secondTreat, firstTreat * secondTreat) %*% msmModel$coef)) 
        d_[numSubj,]=c(mean(temp), mean(firstTreat * temp), mean(secondTreat * temp), mean(firstTreat * secondTreat * temp)) 
        
      }
      
      ########################################################## 
      ### second expectation given history up to time point 1 
      d2_=matrix(NA, nrow=nrow(bootSample), ncol=4)
      
      for(numSubj in 1:nrow(bootSample)){
        
        currentData=NULL
        ########### replicate numCarlo copies of prior history for each subject up to including second time point covariate history
        ##### before treatment assignment
        includeVar = NULL
        includeVar = c(baselineVar, "LEU3N2", "A1")
        for(ind in 1:length(includeVar)){
          draw_g=rep(as.numeric(bootSample[numSubj, includeVar[ind]]), numCarlo)
          currentData=cbind(currentData, draw_g)
        }
        currentData=data.frame(currentData)
        names(currentData)=includeVar
        
        #### simulate second treatment assignment based on subject prior treatment and covariate histories
        prob = predict(model2b, currentData, type = "response")  ###probability of treatment at second time point 
        
        A1_draw=rbinom(numCarlo, 1, prob = prob) ### draw treatment at time point 2 
        currentData[, "A2"]=A1_draw
        
        y_mean00=predict(mod3_00, newdata = currentData) 
        y_draw00=rnorm(numCarlo, mean=y_mean00, sd=summary(mod3_00)$sigma) 
        y_draw00[which(y_draw00<0)]=0
        
        
        ##########################################################
        y_mean01=predict(mod3_01, newdata = currentData) 
        y_draw01=rnorm(numCarlo, mean=y_mean01, sd=summary(mod3_01)$sigma) 
        y_draw01[which(y_draw01<0)]=0
        
        #############################################################
        y_mean10=predict(mod3_10, newdata = currentData) 
        y_draw10=rnorm(numCarlo, mean=y_mean10, sd=summary(mod3_10)$sigma) 
        y_draw10[which(y_draw10<0)]=0
        
        
        #############################################################
        y_mean11=predict(mod3_11, newdata = currentData) 
        y_draw11=rnorm(numCarlo, mean=y_mean11, sd=summary(mod3_11)$sigma) 
        y_draw11[which(y_draw11<0)]=0
        
        firstTreat = currentData[, "A1"]
        secondTreat = currentData[, "A2"]
        currentData[, "LEU3N3"]=y_draw11 * as.numeric(firstTreat==1 & secondTreat==1) + y_draw10*as.numeric(firstTreat==1 & secondTreat==0) +
          y_draw01*as.numeric(firstTreat==0 & secondTreat==1) + y_draw00*as.numeric(firstTreat==0 & secondTreat==0)
        
        temp1a_mc_=predict(model1a, currentData, type="response") 
        temp1a_mc=firstTreat * temp1a_mc_ + (1-firstTreat)*(1-temp1a_mc_)
        
        temp1b_mc_=predict(model1b, currentData, type="response") 
        temp1b_mc=secondTreat * temp1b_mc_ + (1-secondTreat)*(1-temp1b_mc_)
        
        temp2a_mc_=predict(model2a, currentData, type="response") 
        temp2a_mc=firstTreat * temp2a_mc_ + (1-firstTreat)*(1-temp2a_mc_)
        
        temp2b_mc_=predict(model2b, currentData, type="response") 
        temp2b_mc=secondTreat * temp2b_mc_ + (1-secondTreat)*(1-temp2b_mc_)
        
        weight_mc=(temp1a_mc * temp1b_mc)/(temp2a_mc * temp2b_mc) 
        
        
        if(length(truncation) !=0){
          
          weight_mc[which(weight_mc<cutoff[1])]=cutoff[1]
          weight_mc[which(weight_mc>cutoff[2])]=cutoff[2]
          
        }
        
        temp=weight_mc * (currentData[, "LEU3N3"] - (cbind(rep(1,numCarlo), firstTreat, secondTreat, firstTreat * secondTreat) %*% msmModel$coef)) 
        d2_[numSubj,]=c(mean(temp), mean(firstTreat * temp), mean(secondTreat * temp), mean(firstTreat * secondTreat * temp)) 
        
      }
      
      d_sum2=d_+d2_ 
      
      ######### Newton-Raphson to solve the estimating equations 
      #install.packages("rootSolve") 
      #library("rootSolve") 
      #################### solving the estimating equation to obtain doubly robust estimate
      
      firstTreat = as.vector(bootSample[, "A1"])
      secondTreat = as.vector(bootSample[, "A2"])
      
      m2= t(d_sum - d_sum2)%*% rep(1, nrow(bootSample))  ###this is the augmentation term
      
      D<-function(beta){
        m<-cbind(rep(1,nrow(bootSample)), firstTreat, secondTreat, firstTreat * secondTreat)%*%beta
        return(
          t(cbind(1, firstTreat, secondTreat, firstTreat * secondTreat) ) %*% (bootSample[,"weight"] * (bootSample[, "LEU3N3"] - m )) - m2
        )
      }
      
      oldmybeta<-c(5,0,0,0)
      
      repeat{
        mybeta <- oldmybeta - solve( gradient(D, oldmybeta) ) %*% D(oldmybeta)
        if ( max( abs(oldmybeta-mybeta) )<1e-8 ) break
        oldmybeta <- mybeta
      }
      
      result<-c(c(1,1,1,1)%*%mybeta-c(1,0,0,0)%*%mybeta,
                c(1,1,0,0)%*%mybeta-c(1,0,0,0)%*%mybeta,
                c(1,0,1,0)%*%mybeta-c(1,0,0,0)%*%mybeta)
      
      return(result) 
      
    }, error=function(e) return(NA) )
  
} 