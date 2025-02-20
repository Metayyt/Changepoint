##############################################################################################
##############################################################################################
################PENCOMP #########################################################################
############# BALANCE CHECKING ##################################
### This script calculates the balance for each 3-visit window
### Table 3 shows the balance results for the 8th window 

rm(list=ls())
library(nlme)



################### checking balance 1st time point ################
######function to calculate standardized difference between treated and control 
std.diff <- function (C, K, A, pslogit) {  ### 
  
  space=(max(pslogit)-min(pslogit))/(K+1)
  knots=(min(pslogit)+space*(1:K))
  linear=outer(pslogit, knots, "-")
  linear=linear * (linear > 0)
  colnames(linear)=paste("basis", 1:K, sep="")
  
  tempData=data.frame(id=rep(1, length(pslogit)), C=C, A=A, pslogit=pslogit)
  tempData=data.frame(tempData, linear)
  
  diag1 <- lme(C ~ pslogit, random=list(id=pdIdent(~0+basis1+basis2+basis3+basis4+basis5+basis6+basis7+basis8+basis9+
                                                     basis10)), data=tempData)
  tempData$res=residuals(diag1)
  
  #########################
  meanTreat=mean(tempData[tempData$A==1, which(names(tempData)=="res")] ) 
  meanControl=mean(tempData[tempData$A==0, which(names(tempData)=="res")] )  
  s2_T=var(tempData[tempData$A==1, which(names(tempData)=="res")] )  
  s2_C=var(tempData[tempData$A==0, which(names(tempData)=="res")] ) 
  
  diff_after=abs((meanTreat - meanControl)/sqrt( (s2_T + s2_C)/2 ))
  stats_after=abs(t.test(tempData$res ~ tempData$A)$statistic)
  
  
  ####################
  meanTreat=mean(tempData[tempData$A==1, which(names(tempData)=="C")] ) 
  meanControl=mean(tempData[tempData$A==0, which(names(tempData)=="C")] )  
  s2_T=var(tempData[tempData$A==1, which(names(tempData)=="C")] )  
  s2_C=var(tempData[tempData$A==0, which(names(tempData)=="C")] ) 
  
  diff_before=abs((meanTreat - meanControl)/sqrt( (s2_T + s2_C)/2 ))
  stats_before=abs(t.test(tempData$C ~ tempData$A)$statistic)
  
  
  result = c(before=diff_before, after=diff_after, beforeStats=stats_before, afterStats=stats_after)
  return(result)
  
}


DIREC="Application/"  ###where dataset is


###import the dataset
simdat2=read.csv(paste(DIREC, "dataset/data.csv", sep=""), header=T)

dim(simdat2)
table(simdat2$yearPos)
table(simdat2$visit)

summary(simdat2$age)
hist(simdat2$age)
table(simdat2$white)
table(simdat2$college)

################################################################################
###number of imputed datasets
start= 7
end= 21



for (d in start:end){
  
  print(d)
  
  set.seed(d)
  
  ############for every 3-visit moving window, run the following script
  ########################################################
  simdat=data.frame(CASEID=simdat2$CASEID)
  
  ###treatment indicator at first time point A1, at second time point A2
  simdat$A1=simdat2[,which(names(simdat2)==paste("TreatVisit", (d+1)*10, sep=""))] ###treatment at first time point
  simdat$A2=simdat2[,which(names(simdat2)==paste("TreatVisit", (d+2)*10, sep=""))]  ##treatment at second time point
  
  ###CD4 count, LEU3N1 at first time point, LEU3N2 at second time point, LEU3N3-final outcome of interest after the second treatment A2
  simdat$LEU3N1=simdat2[,which(names(simdat2)==paste("LEU3N.", (d)*10, sep=""))]  ###
  simdat$LEU3N2=simdat2[,which(names(simdat2)==paste("LEU3N.", (d+1)*10, sep=""))]
  simdat$LEU3N3=simdat2[,which(names(simdat2)==paste("LEU3N.", (d+2)*10, sep=""))]
  
  ##CD8 counts
  simdat$LEU2N1=simdat2[,which(names(simdat2)==paste("LEU2N.", (d)*10, sep=""))]
  simdat$LEU2N2=simdat2[,which(names(simdat2)==paste("LEU2N.",(d+1)*10, sep=""))]
  simdat$LEU2N3=simdat2[,which(names(simdat2)==paste("LEU2N.", (d+2)*10, sep=""))]
  
  ###white blood cell counts
  simdat$WBC1=simdat2[,which(names(simdat2)==paste("WBC.", (d)*10, sep=""))]
  simdat$WBC2=simdat2[,which(names(simdat2)==paste("WBC.", (d+1)*10, sep=""))]
  simdat$WBC3=simdat2[,which(names(simdat2)==paste("WBC.", (d+2)*10, sep=""))]
  
  
  ###Red blood cell counts
  simdat$RBC1=simdat2[,which(names(simdat2)==paste("RBC.", (d)*10, sep=""))]
  simdat$RBC2=simdat2[,which(names(simdat2)==paste("RBC.", (d+1)*10, sep=""))]
  simdat$RBC3=simdat2[,which(names(simdat2)==paste("RBC.", (d+2)*10, sep=""))]
  
  ###platelet counts
  simdat$PLATE1=simdat2[,which(names(simdat2)==paste("PLATE.", (d)*10, sep=""))]
  simdat$PLATE2=simdat2[,which(names(simdat2)==paste("PLATE.", (d+1)*10, sep=""))]
  simdat$PLATE3=simdat2[,which(names(simdat2)==paste("PLATE.",(d+2)*10, sep=""))]
  
  ##viral load
  simdat$VLOAD1=simdat2[,which(names(simdat2)==paste("VLOAD.", (d)*10, sep=""))]
  simdat$VLOAD2=simdat2[,which(names(simdat2)==paste("VLOAD.", (d+1)*10, sep=""))]
  simdat$VLOAD3=simdat2[,which(names(simdat2)==paste("VLOAD.",(d+2)*10, sep=""))]
  
  ####demographic information
  simdat$age=simdat2[,which(names(simdat2)=="age")]
  simdat$white=simdat2[,which(names(simdat2)=="white")]
  simdat$college=simdat2[,which(names(simdat2)=="college")]
  simdat$deathYear=simdat2[,which(names(simdat2)=="DTHDATEyy")]
  
  ###HIV status
  simdat$hiv1=simdat2[,which(names(simdat2)==paste("hivVisit", (d)*10, sep=""))]
  
  ######dosage at the start of each 3-visit window
  simdat$dosage=simdat2[,which(names(simdat2)==paste("dosageFill", (d), sep=""))]
  
  
  simdat=simdat[which(simdat$hiv1==1),] #### keep only the hiv+ in the baseline of each 3 time point window
  
  
  ####transform the blood values by taking the square root, distributions of raw values are highly skewed
  simdat$LEU3N1=sqrt(simdat$LEU3N1)
  simdat$LEU2N1=sqrt(simdat$LEU2N1) 
  simdat$WBC1=sqrt(simdat$WBC1)  
  simdat$RBC1=sqrt(simdat$RBC1)
  simdat$PLATE1=sqrt(simdat$PLATE1)
  
  simdat$LEU3N2=sqrt(simdat$LEU3N2)
  simdat$LEU2N2=sqrt(simdat$LEU2N2) 
  simdat$WBC2=sqrt(simdat$WBC2)  
  simdat$RBC2=sqrt(simdat$RBC2)
  simdat$PLATE2=sqrt(simdat$PLATE2)
  
  simdat$LEU3N3=sqrt(simdat$LEU3N3)
  
  dim(simdat)
  
  sum(is.na(simdat$LEU3N1))
  sum(is.na(simdat$PLATE1))  
  sum(is.na(simdat$WBC1))
  sum(is.na(simdat$RBC1))  
  sum(is.na(simdat$RBC2))
  sum(is.na(simdat$LEU3N2))
  
  
  ######for our analysis, we looked only the complete data for each 3-visit window, avoid dealing with lost to followup/death topic of future research
  simdatFit=simdat[which(!is.na(simdat$LEU3N1) & !is.na(simdat$LEU2N1) & !is.na(simdat$WBC1) & 
                           !is.na(simdat$RBC1) & !is.na(simdat$PLATE1) & 
                           !is.na(simdat$LEU3N2) &
                           !is.na(simdat$LEU3N3) &
                           !is.na(simdat$A1) & !is.na(simdat$A2) & !is.na(simdat$dosage)),]
  
  dim(simdatFit)
  
  simdatFit$id2=1:dim(simdatFit)[1]
  simdatFit$id=1
  
  sum(simdatFit$A1==1 & simdatFit$A2==1)
  sum(simdatFit$A1==1 & simdatFit$A2==0)
  sum(simdatFit$A1==0 & simdatFit$A2==1)
  sum(simdatFit$A1==0 & simdatFit$A2==0)
  
  
  mean(simdatFit$LEU3N3[(simdatFit$A1==1 & simdatFit$A2==1)], na.rm=T)
  mean(simdatFit$LEU3N3[(simdatFit$A1==1 & simdatFit$A2==0)], na.rm=T)
  mean(simdatFit$LEU3N3[(simdatFit$A1==0 & simdatFit$A2==1)], na.rm=T)
  mean(simdatFit$LEU3N3[(simdatFit$A1==0 & simdatFit$A2==0)], na.rm=T)
  
  
  
  ################Propensity score models at the first and second time points #################
  model1a=glm(A1 ~ 1, data=simdatFit, family="binomial") 
  temp1a=(simdatFit$A1)*model1a$fitted.values + (1-simdatFit$A1)*(1-model1a$fitted.values)
  summary(model1a)
  
  model1b=glm(A2 ~ A1, data=simdatFit, family="binomial") ### correctly specified treatment mechanism 
  temp1b=(simdatFit$A2)*model1b$fitted.values + (1-simdatFit$A2)*(1-model1b$fitted.values)
  summary(model1b)
  
  ######
  model2a=glm(A1 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + dosage, data=simdatFit, family="binomial") 
  temp2a=(simdatFit$A1)*model2a$fitted.values + (1-simdatFit$A1)*(1-model2a$fitted.values)
  summary(model2a)
  
  model2b=glm(A2 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + A1 + dosage, 
              data=simdatFit, family="binomial") ### correctly specified treatment mechanism 
  temp2b=(simdatFit$A2)*model2b$fitted.values + (1-simdatFit$A2)*(1-model2b$fitted.values)
  summary(model2b)
  
  summary(temp1a/temp2a)
  sd(temp1a/temp2a)
  
  summary((temp1a * temp1b)/(temp2a * temp2b))
  sd((temp1a * temp1b)/(temp2a * temp2b))
  

  simdatFit$sw=log((temp2a*temp2b)/(1-temp2a*temp2b))
  
  simdatFit$pslogit = log(temp2a/(1-temp2a))  ####here the name pslogit is probability of observed treatment at first time point
  
  
  
  ##############################################################
  ###use equally spaced fixed knots assuming K knots
  simdatFit0=simdatFit[simdatFit$A1==0,]
  K1=min(c(round(0.25*dim(simdatFit0)[1]),35))
  space0=(max(simdatFit0$pslogit)-min(simdatFit0$pslogit))/(K1+1)
  knots0=(min(simdatFit0$pslogit)+space0*(1:K1))
  
  ###assume a truncated linear basis
  linear0=NULL
  for (var in 1:K1) {
    temp=(simdatFit0$pslogit-knots0[var])
    temp2=(as.numeric(simdatFit0$pslogit<knots0[var]))*0 + (as.numeric(simdatFit0$pslogit>=knots0[var]))*temp
    linear0=cbind(linear0, temp2)
  }
  
  colnames(linear0)=paste("basis", 1:K1, sep="")
  
  
  pspp0 <- lme(LEU3N2 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + pslogit, random=list(id=pdIdent(~0+linear0)), data=simdatFit0)
  summary(pspp0)
  
  
  ############# IMPUTE THE MISSING Intermediate outcomes under control A1=0 IN THE ORIGINAL DATA SET#####################################
  newData0=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                      WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1,
                      LEU3N2=rep(NA, dim(simdatFit)[1]), 
                      RBC2=rep(NA, dim(simdatFit)[1]),
                      college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, 
                      dosage=simdatFit$dosage, 
                      A1=rep(0, dim(simdatFit)[1]), A1_Orig=simdatFit$A1)
  newData0$id=1
  predict2a=1-predict(model2a, newData0, type="response")
  newData0$pslogit=log(predict2a/(1-predict2a))
  newData0$prop=predict2a
  
  linear0=NULL
  for (var in 1:K1) {
    temp=(newData0$pslogit-knots0[var])
    temp2=(as.numeric(newData0$pslogit<knots0[var]))*0 + (as.numeric(newData0$pslogit>=knots0[var]))*temp
    linear0=cbind(linear0, temp2)
  }
  
  colnames(linear0)=paste("basis", 1:K1, sep="")
  
  predictedM0=predict(pspp0, newData0) + rnorm(dim(newData0)[1], 0, summary(pspp0)$sigma)
  predictedM0[which(predictedM0<0)]=0
  newData0$LEU3N2=predictedM0
  newData0$LEU3N2[which(!is.na(simdatFit$LEU3N2) & (simdatFit$A1==0))]=simdatFit$LEU3N2[which(!is.na(simdatFit$LEU3N2) & (simdatFit$A1==0))]
  
  
  ##############################################################
  ###use equally spaced fixed knots assuming K knots
  simdatFit1=simdatFit[simdatFit$A1==1,]
  K1=round(min(c(0.25*dim(simdatFit1)[1], 35)))
  space1=(max(simdatFit1$pslogit)-min(simdatFit1$pslogit))/(K1+1)
  knots1=(min(simdatFit1$pslogit)+space1*(1:K1))
  
  ###assume a truncated linear basis
  linear1=NULL
  for (var in 1:K1) {
    temp=(simdatFit1$pslogit-knots1[var])
    temp2=(as.numeric(simdatFit1$pslogit<knots1[var]))*0 + (as.numeric(simdatFit1$pslogit>=knots1[var]))*temp
    linear1=cbind(linear1, temp2)
  }
  
  colnames(linear1)=paste("basis", 1:K1, sep="")
  
  
  pspp1 <- lme(LEU3N2 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + pslogit, random=list(id=pdIdent(~0+linear1)), data=simdatFit1)
  summary(pspp1)
  
  
  ##################################################
  ############# IMPUTE THE missing intermediate outcomes under treatment A1=1 IN THE ORIGINAL DATA SET#####################################
  newData1=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                      WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1,
                      LEU3N2=rep(NA, dim(simdatFit)[1]), 
                      RBC2=rep(NA, dim(simdatFit)[1]),  
                      college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, 
                      dosage=simdatFit$dosage, 
                      A1=rep(1, dim(simdatFit)[1]), A1_Orig=simdatFit$A1)
  newData1$id=1
  predict2a=predict(model2a, newData1, type="response")
  newData1$pslogit=log(predict2a/(1-predict2a))
  newData1$prop=predict2a
  
  linear1=NULL
  for (var in 1:K1) {
    temp=(newData1$pslogit-knots1[var])
    temp2=(as.numeric(newData1$pslogit<knots1[var]))*0 + (as.numeric(newData1$pslogit>=knots1[var]))*temp
    linear1=cbind(linear1, temp2)
  }
  
  colnames(linear1)=paste("basis", 1:K1, sep="")
  
  predictedM1=predict(pspp1, newData1) + rnorm(dim(newData1)[1], 0, summary(pspp0)$sigma)
  predictedM1[which(predictedM1<0)]=0
  newData1$LEU3N2=predictedM1
  newData1$LEU3N2[which(!is.na(simdatFit$LEU3N2) & (simdatFit$A1==1))]=simdatFit$LEU3N2[which(!is.na(simdatFit$LEU3N2) & (simdatFit$A1==1))]
  
  mean(newData1$LEU3N2 - newData0$LEU3N2)
  
  #############################################################################################################################
  ####################IMPUTING THE MISSING FInal outcomes IN THE ORIGINAL DATA SET################################
  newData00=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, 
                       dosage=simdatFit$dosage, 
                       A1=rep(0, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=1-predict(model2a, newData00, type="response")
  predict2b=1-predict(model2b, newData00, type="response")
  newData00$sw=predict2a * predict2b
  newData00$swlog=log(newData00$sw/(1-newData00$sw))
  newData00$id=1
  
  
  newData01=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, 
                       dosage=simdatFit$dosage, 
                       A1=rep(0, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=1-predict(model2a, newData01, type="response")    
  predict2b=predict(model2b, newData01, type="response")
  newData01$sw=predict2a * predict2b
  newData01$swlog=log(newData01$sw/(1-newData01$sw))
  newData01$id=1
  
  
  newData10=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, ### corrected 8/7/2017
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, 
                       dosage=simdatFit$dosage, 
                       A1=rep(1, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=predict(model2a, newData10, type="response")    
  predict2b=1-predict(model2b, newData10, type="response")
  newData10$sw=predict2a * predict2b
  newData10$swlog=log(newData10$sw/(1-newData10$sw))
  newData10$id=1
  
  
  newData11=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, ###corrected 8/7/2017
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age,
                       dosage=simdatFit$dosage, 
                       A1=rep(1, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=predict(model2a, newData11, type="response")    
  predict2b=predict(model2b, newData11, type="response")
  newData11$sw=predict2a * predict2b
  newData11$swlog=log(newData11$sw/(1-newData11$sw))
  newData11$id=1
  
  

  
  
  ################# inclusion and exclusion criteria ################################################
  
  ####################################################################################################
  ##FIND THE OVERLAPPING REGIONS AT FIRST TIME POINT ##################################################
  ####looking of probability of getting treated for both control and treated groups
  probTreat=predict(model2a, simdatFit, type="response")
  summary(probTreat)
  
  overlapTreat=c(min(probTreat[which(simdatFit$A1==1)]), max(probTreat[which(simdatFit$A1==1)]))
  
  
  ##############################
  ####looking of probability of getting treated for both control and treated groups
  probControl=1-predict(model2a, simdatFit, type="response")
  overlapControl=c(min(probControl[which(simdatFit$A1==0)]), max(probControl[which(simdatFit$A1==0)]))
  
  simdatFit$includedT1=(probTreat >= overlapTreat[1] &  probTreat <= overlapTreat[2]) *
    (probControl >= overlapControl[1] &  probControl <= overlapControl[2])
  sum(simdatFit$includedT1)  
  
  
  #################################################################################################
  ########based on estimated propensity of getting treatment pattern 11
  group11=newData11$sw[simdatFit$A1==1 & simdatFit$A2==1]
  overlap=c(min(group11), max(group11))
  sum((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))/dim(newData11)[1]
  
  
  simdatFit$included11=0  ###included for 11
  simdatFit$included11[which((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))]=1
  sum(simdatFit$included11)
  
  rm(overlap)
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  group00=newData00$sw[simdatFit$A1==0 & simdatFit$A2==0]
  overlap=c(min(group00), max(group00))
  sum((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))/dim(newData00)[1]
  
  
  simdatFit$included00=0  ###included for 00
  simdatFit$included00[which((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))]=1
  sum(simdatFit$included00)
  
  rm(overlap)
  
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 01
  group01=newData01$sw[simdatFit$A1==0 & simdatFit$A2==1]
  overlap=c(min(group01), max(group01))
  sum((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))/dim(newData01)[1]
  
  
  simdatFit$included01=0  ###included for 01
  simdatFit$included01[which((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))]=1
  sum(simdatFit$included01)
  rm(overlap)
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  group10=newData10$sw[simdatFit$A1==1 & simdatFit$A2==0]
  overlap=c(min(group10), max(group10))
  sum((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))/dim(newData10)[1]
  
  
  simdatFit$included10=0  ###included for 10
  simdatFit$included10[which((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))]=1
  sum(simdatFit$included10)
  
  rm(overlap)
  
  
  ############subjects whose propensity scores are within the overlap region ##################################
  simdatFit$includeAll_01=as.numeric(simdatFit$included00==1 & simdatFit$included01==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_01)
  
  
  simdatFit$includeAll_11=as.numeric(simdatFit$included00==1 & simdatFit$included11==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_11)
  
  
  simdatFit$includeAll_10=as.numeric(simdatFit$included00==1 & simdatFit$included10==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_10)
  
  

  
  ############ balance checking for treatment A1 ##############
  ###### first time point ###################
  balance=NULL
  balance=rbind(balance, sapply(newData1[,1:5], std.diff, K=10, A=as.numeric(simdatFit$A1==1), pslogit=newData1$pslogit))
  #balance=rbind(balance, sapply(newData0[,1:5], std.diff, K=10, A=as.numeric(simdatFit$A1==0), pslogit=newData0$pslogit))
  balance=t(balance)
  balance=format(balance, digits=2)
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                   rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                   balance[,4], rep("\\\\", dim(balance)[1]))
  
  ###standardized difference before, t statistics before, diff after, t stats after
  #write.table(balance, paste(DIRECOUT, "FinalResult/balanceChecking/balance/balance_Time1_sample", d, ".txt",sep=""), quote=F, sep="\t")
  

  
  
  ######## SECOND TIME POINT ################################################
  ########################################################################
  ########################################################################
  ####condition on product of propensity scores: consider removing subjects outside of overlap regions, results in the appendix
  ########## treatment regime of 11 vs others ##############################
  newData11Ind=newData11[which(simdatFit$includeAll_11==1), ]
  balance=NULL
  balance=rbind(balance, sapply(newData11Ind[,1:6], std.diff, K=10, A=as.numeric(newData11Ind$A1_Orig==1 & newData11Ind$A2_Orig==1), pslogit=newData11Ind$swlog))
  balance=t(balance)
  balance=format(balance, digits=2)
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                balance[,4], rep("\\\\", dim(balance)[1]))
  
  write.table(balance, paste(DIREC, "FiguresandTables/balanceChecking/balance_Time11_sample", d, ".txt",sep=""), quote=F, sep="\t")
  
  #########treatment regime of 01 vs others ########
  newData01Ind=newData01[which(simdatFit$includeAll_01==1), ]
  balance=NULL
  balance=rbind(balance, sapply(newData01Ind[,1:6], std.diff, K=10, A=as.numeric(newData01Ind$A1_Orig==0 & newData01Ind$A2_Orig==1), pslogit=newData01Ind$swlog)) 
  balance=t(balance)
  balance=format(balance, digits=2)
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                balance[,4], rep("\\\\", dim(balance)[1]))
  
  write.table(balance, paste(DIREC, "FiguresandTables/balanceChecking/balance_Time01_sample", d, ".txt",sep=""), quote=F, sep="\t")
  
  
  #########treatment regime of 10 vs others ########
  newData10Ind=newData10[which(simdatFit$includeAll_10==1), ]
  balance=NULL
  balance=rbind(balance, sapply(newData10Ind[,1:6], std.diff, K=10, A=as.numeric(newData10Ind$A1_Orig==1 & newData10Ind$A2_Orig==0), pslogit=newData10Ind$swlog)) 
  balance=t(balance)
  balance=format(balance, digits=2)
  
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                balance[,4], rep("\\\\", dim(balance)[1]))
  
  write.table(balance, paste(DIREC, "FiguresandTables/balanceChecking/balance_Time10_sample", d, ".txt",sep=""), quote=F, sep="\t")
  
  

 ####################################################################################################################### 
  ########################### including everyone, before removing subjects outside overlapping regions, considered in the main paper
  
  ######## SECOND TIME POINT ################################################################
  ##########################################################################################
  ##########################################################################################
  ####condition on product of propensity scores
  ##################   for d=14, this produces Table 3 in the main paper #######################
  ############# treatment regime of 11 vs others ##############################################
  newData11Ind=newData11
  balance=NULL
  balance=rbind(balance, sapply(newData11Ind[,1:6], std.diff, K=10, A=as.numeric(newData11Ind$A1_Orig==1 & newData11Ind$A2_Orig==1), pslogit=newData11Ind$swlog))
  balance=t(balance)
  balance=format(balance, digits=2)
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                balance[,4], rep("\\\\", dim(balance)[1]))
  
  write.table(balance, paste(DIREC, "FiguresandTables/balanceChecking/ATE_balance_Time11_sample", d, ".txt",sep=""), quote=F, sep="\t")
  
  #########treatment regime of 01 vs others ########
  newData01Ind=newData01
  balance=NULL
  balance=rbind(balance, sapply(newData01Ind[,1:6], std.diff, K=10, A=as.numeric(newData01Ind$A1_Orig==0 & newData01Ind$A2_Orig==1), pslogit=newData01Ind$swlog)) 
  balance=t(balance)
  balance=format(balance, digits=2)
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                balance[,4], rep("\\\\", dim(balance)[1]))
  
  write.table(balance, paste(DIREC, "FiguresandTables/balanceChecking/ATE_balance_Time01_sample", d, ".txt",sep=""), quote=F, sep="\t")
  
  
  ################### treatment regime of 10 vs others ########
  newData10Ind=newData10
  balance=NULL
  balance=rbind(balance, sapply(newData10Ind[,1:6], std.diff, K=10, A=as.numeric(newData10Ind$A1_Orig==1 & newData10Ind$A2_Orig==0), pslogit=newData10Ind$swlog)) 
  balance=t(balance)
  balance=format(balance, digits=2)
  
  
  balance=cbind(rep("&", dim(balance)[1]), balance[,1], rep("&", dim(balance)[1]), balance[,3], 
                rep("&", dim(balance)[1]), balance[,2], rep("&", dim(balance)[1]),
                balance[,4], rep("\\\\", dim(balance)[1]))
  
  write.table(balance, paste(DIREC, "FiguresandTables/balanceChecking/ATE_balance_Time10_sample", d, ".txt",sep=""), quote=F, sep="\t")
  
  
  
  
  }











