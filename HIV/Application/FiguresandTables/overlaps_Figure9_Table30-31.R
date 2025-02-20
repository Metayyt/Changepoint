##############################################################################################
##############################################################################################
#########################################################################################
############# BALANCE CHECKING ##################################
#########This R script produces overlap distributions (Figure 9),
###########summary of weights (Table 30)
########overlap proportions (Table 31 in Appendix)

rm(list=ls())
library(splines)
library(nlme)
require(stats)
require(graphics)

DIRECOUT="codes/Application/" ###where dataset is stored

###import dataset
simdat2=read.csv(paste(DIRECOUT, "dataset/data.csv", sep=""), header=T)

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


weightSum=matrix(NA, nrow=end, ncol=4)
overlapProp=matrix(NA, nrow=end, ncol=8)

for (d in start:end){
  
  print(d)
  
  set.seed(d)
  
  ###select the variables needed
  ########################################################
  simdat=data.frame(CASEID=simdat2$CASEID)
  
  simdat$A0Before=simdat2[,which(names(simdat2)==paste("TreatVisit", d*10, sep=""))]
  simdat$A1=simdat2[,which(names(simdat2)==paste("TreatVisit", (d+1)*10, sep=""))]
  simdat$A2=simdat2[,which(names(simdat2)==paste("TreatVisit", (d+2)*10, sep=""))]
  
  simdat$LEU3N0=simdat2[,which(names(simdat2)==paste("LEU3N.", (d-1)*10, sep=""))]
  simdat$LEU3N1=simdat2[,which(names(simdat2)==paste("LEU3N.", (d)*10, sep=""))]
  simdat$LEU3N2=simdat2[,which(names(simdat2)==paste("LEU3N.", (d+1)*10, sep=""))]
  simdat$LEU3N3=simdat2[,which(names(simdat2)==paste("LEU3N.", (d+2)*10, sep=""))]
  
  simdat$LEU2N0=simdat2[,which(names(simdat2)==paste("LEU2N.", (d-1)*10, sep=""))]
  simdat$LEU2N1=simdat2[,which(names(simdat2)==paste("LEU2N.", (d)*10, sep=""))]
  simdat$LEU2N2=simdat2[,which(names(simdat2)==paste("LEU2N.",(d+1)*10, sep=""))]
  simdat$LEU2N3=simdat2[,which(names(simdat2)==paste("LEU2N.", (d+2)*10, sep=""))]
  
  simdat$WBC0=simdat2[,which(names(simdat2)==paste("WBC.", (d-1)*10, sep=""))]
  simdat$WBC1=simdat2[,which(names(simdat2)==paste("WBC.", (d)*10, sep=""))]
  simdat$WBC2=simdat2[,which(names(simdat2)==paste("WBC.", (d+1)*10, sep=""))]
  simdat$WBC3=simdat2[,which(names(simdat2)==paste("WBC.", (d+2)*10, sep=""))]
  
  simdat$RBC0=simdat2[,which(names(simdat2)==paste("RBC.", (d-1)*10, sep=""))]
  simdat$RBC1=simdat2[,which(names(simdat2)==paste("RBC.", (d)*10, sep=""))]
  simdat$RBC2=simdat2[,which(names(simdat2)==paste("RBC.", (d+1)*10, sep=""))]
  simdat$RBC3=simdat2[,which(names(simdat2)==paste("RBC.", (d+2)*10, sep=""))]
  
  
  simdat$PLATE0=simdat2[,which(names(simdat2)==paste("PLATE.", (d-1)*10, sep=""))]
  simdat$PLATE1=simdat2[,which(names(simdat2)==paste("PLATE.", (d)*10, sep=""))]
  simdat$PLATE2=simdat2[,which(names(simdat2)==paste("PLATE.", (d+1)*10, sep=""))]
  simdat$PLATE3=simdat2[,which(names(simdat2)==paste("PLATE.",(d+2)*10, sep=""))]
  
  
  simdat$VLOAD0=simdat2[,which(names(simdat2)==paste("VLOAD.", (d-1)*10, sep=""))]
  simdat$VLOAD1=simdat2[,which(names(simdat2)==paste("VLOAD.", (d)*10, sep=""))]
  simdat$VLOAD2=simdat2[,which(names(simdat2)==paste("VLOAD.", (d+1)*10, sep=""))]
  simdat$VLOAD3=simdat2[,which(names(simdat2)==paste("VLOAD.",(d+2)*10, sep=""))]
  
  
  simdat$age=simdat2[,which(names(simdat2)=="age")]
  simdat$white=simdat2[,which(names(simdat2)=="white")]
  simdat$college=simdat2[,which(names(simdat2)=="college")]
  simdat$deathYear=simdat2[,which(names(simdat2)=="DTHDATEyy")]
  
  
  simdat$hiv1=simdat2[,which(names(simdat2)==paste("hivVisit", (d)*10, sep=""))]
  simdat$hiv2=simdat2[,which(names(simdat2)==paste("hivVisit", (d+1)*10, sep=""))]
  simdat$hiv3=simdat2[,which(names(simdat2)==paste("hivVisit", (d+2)*10, sep=""))]
  
  
  simdat$dosage=simdat2[,which(names(simdat2)==paste("dosageFill", (d), sep=""))]
  
  
  names(simdat)
  
  
  simdat$yearPos=simdat2$yearPos
  simdat$yearPosInd=numeric(length=dim(simdat)[1])
  simdat$yearPosInd[which(simdat$yearPos==1984)]=1
  simdat$yearPosInd[which(simdat$yearPos>=1985 & simdat$yearPos<=1987)]=2
  simdat$yearPosInd[which(simdat$yearPos>=1988)]=3
  simdat$yearPosInd=as.factor(simdat$yearPosInd)
  table(simdat$yearPosInd)
  
  
  simdat=simdat[which(simdat$hiv1==1),] #### keep only the hiv+ in the baseline of each 3 time point window
  
  
  ####transform the blood values by taking the square root
  simdat$LEU3N0=sqrt(simdat$LEU3N0)
  simdat$LEU2N0=sqrt(simdat$LEU2N0) 
  simdat$WBC0=sqrt(simdat$WBC0)  
  simdat$RBC0=sqrt(simdat$RBC0)
  simdat$PLATE0=sqrt(simdat$PLATE0)
  
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
  
  
  ###restrict analysis to subjects with complete data on those variables
  simdatFit=simdat[which(!is.na(simdat$LEU3N1) & !is.na(simdat$LEU2N1) & !is.na(simdat$WBC1) & 
                           !is.na(simdat$RBC1) & !is.na(simdat$PLATE1) & 
                           
                           !is.na(simdat$LEU3N2) &
                           !is.na(simdat$LEU3N3) &
                           
                           !is.na(simdat$A1) & !is.na(simdat$A2) & !is.na(simdat$dosage)),]
  
  
  dim(simdatFit)
  
  simdatFit$id2=1:dim(simdatFit)[1]
  simdatFit$id=1
  
  table(simdatFit$yearPosInd)
  
  sum(simdatFit$A1==1 & simdatFit$A2==1)
  sum(simdatFit$A1==1 & simdatFit$A2==0)
  sum(simdatFit$A1==0 & simdatFit$A2==1)
  sum(simdatFit$A1==0 & simdatFit$A2==0)
  
  
  mean(simdatFit$LEU3N3[(simdatFit$A1==1 & simdatFit$A2==1)], na.rm=T)
  mean(simdatFit$LEU3N3[(simdatFit$A1==1 & simdatFit$A2==0)], na.rm=T)
  mean(simdatFit$LEU3N3[(simdatFit$A1==0 & simdatFit$A2==1)], na.rm=T)
  mean(simdatFit$LEU3N3[(simdatFit$A1==0 & simdatFit$A2==0)], na.rm=T)
  
  
  
  ####################################
  model1a=glm(A1 ~ 1, data=simdatFit, family="binomial") 
  temp1a=(simdatFit$A1)*model1a$fitted.values + (1-simdatFit$A1)*(1-model1a$fitted.values)
  summary(model1a)
  
  model1b=glm(A2 ~ A1, 
              data=simdatFit, family="binomial") ### correctly specified treatment mechanism 
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
  weights=(temp1a * temp1b)/(temp2a * temp2b)
  
  simdatFit$sw=log((temp2a*temp2b)/(1-temp2a*temp2b)) ###logit of propensity of observed treatment at second time point
  
  simdatFit$pslogit = log(temp2a/(1-temp2a))  ####logit of probability of observed treatment at first time point
  
  #row.names(weightSum)=paste("window", 1:21, sep="")
  weightSum[d,]=c(mean(weights), sd(weights), min(weights), max(weights)) ###summary of the weights
  

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
  
  
  ############# IMPUTE THE MISSING Intermediate outcome under control IN THE ORIGINAL DATA SET#####################################
  newData0=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                      WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1,
                      LEU3N2=rep(NA, dim(simdatFit)[1]), 
                      RBC2=rep(NA, dim(simdatFit)[1]),
                      college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
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
  predictedM0[which(predictedM0<0)]=0  ###CD4 counts should be at least 0
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
  ############# IMPUTE THE MISSING Intermediate outcome under treatment IN THE ORIGINAL DATA SET#####################################
  newData1=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                      WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1,
                      LEU3N2=rep(NA, dim(simdatFit)[1]), 
                      RBC2=rep(NA, dim(simdatFit)[1]),  
                      college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
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
  predictedM1[which(predictedM1<0)]=0  ### restrict CD4 counts to at least 0
  newData1$LEU3N2=predictedM1
  newData1$LEU3N2[which(!is.na(simdatFit$LEU3N2) & (simdatFit$A1==1))]=simdatFit$LEU3N2[which(!is.na(simdatFit$LEU3N2) & (simdatFit$A1==1))] ###
  
  mean(newData1$LEU3N2 - newData0$LEU3N2)
  
  #############################################################################################################################
  ####################IMPUTING THE MISSING VALUES IN THE ORIGINAL DATA SET################################
  newData00=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(0, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=1-predict(model2a, newData00, type="response")
  predict2b=1-predict(model2b, newData00, type="response")
  newData00$sw=predict2a * predict2b
  newData00$swlog=log(newData00$sw/(1-newData00$sw))
  newData00$id=1
  
  
  newData01=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(0, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=1-predict(model2a, newData01, type="response")    
  predict2b=predict(model2b, newData01, type="response")
  newData01$sw=predict2a * predict2b
  newData01$swlog=log(newData01$sw/(1-newData01$sw))
  newData01$id=1
  
  
  newData10=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, ### corrected 8/7/2017
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(1, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=predict(model2a, newData10, type="response")    
  predict2b=1-predict(model2b, newData10, type="response")
  newData10$sw=predict2a * predict2b
  newData10$swlog=log(newData10$sw/(1-newData10$sw))
  newData10$id=1
  
  
  newData11=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, ###corrected 8/7/2017
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age,yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(1, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]), A1_Orig=simdatFit$A1, A2_Orig=simdatFit$A2)
  predict2a=predict(model2a, newData11, type="response")    
  predict2b=predict(model2b, newData11, type="response")
  newData11$sw=predict2a * predict2b
  newData11$swlog=log(newData11$sw/(1-newData11$sw))
  newData11$id=1
  
  
  
  
  
  
  #################################################################################################
  ##FIND THE OVERLAPPING REGIONS AT FIRST TIME POINT #########################
  ####looking of probability of getting treated for both control and treated groups
  probTreat=predict(model2a, simdatFit, type="response")
  summary(probTreat)
  overlapTreat=c(min(probTreat[which(simdatFit$A1==1)]), max(probTreat[which(simdatFit$A1==1)]))

  # the input of the following function MUST be a numeric list
  plot(density(probTreat[simdatFit$A1==0], from=0, 
               to=1), lty=1, lwd=2, col="black", xlab="Propensity Score", main="")
  lines(density(probTreat[simdatFit$A1==1],from=0, to=1), lty=2, lwd=2, col="red")
  legend("topright", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  control=probTreat[simdatFit$A1==0]
  treat=probTreat[simdatFit$A1==1]
  group <- c("Control", "Treat")
  boxplot(control, treat, names=group, horizontal = TRUE,
          main = "Propensity Score Distribution", xlab = "Propensity Score") 
  
  ##############################
  ####looking of probability of getting treated for both control and treated groups
  probControl=1-predict(model2a, simdatFit, type="response")
  overlapControl=c(min(probControl[which(simdatFit$A1==0)]), max(probControl[which(simdatFit$A1==0)]))
  
  plot(density(probControl[simdatFit$A1==0], from=0, 
               to=1), lty=1, lwd=2, col="black", xlab="Propensity Score", main="")
  lines(density(probControl[simdatFit$A1==1],from=0, to=1),
        lty=2, lwd=2, col="red")
  legend("topleft", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  
  control=probControl[simdatFit$A1==0]
  treat=probControl[simdatFit$A1==1]
  group <- c("Control", "Treat")
  boxplot(control, treat, names=group, horizontal = TRUE,main = "Propensity Score Distribution", 
          xlab = "Propensity Score") 
  
  
  ####################################################################################################
  simdatFit$includedT1=(probTreat >= overlapTreat[1] &  probTreat <= overlapTreat[2]) *
    (probControl >= overlapControl[1] &  probControl <= overlapControl[2])
  sum(simdatFit$includedT1)  
  
  cutoff=quantile(probTreat[which(simdatFit$A1==1)], probs = c(0.05, 0.95))
  others=probTreat[which(simdatFit$A1==0)]
  overlapProp[d,1]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  ###########
  cutoff=quantile(probControl[which(simdatFit$A1==0)], probs = c(0.05, 0.95))
  others=probControl[which(simdatFit$A1==1)]
  overlapProp[d,2]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  
  pdf(paste(DIRECOUT, "overlaps_window", d, ".pdf", sep=""))  ###where overlap plots are stored
  
  
  par(mfrow = c(2,2),
      oma = c(2,2,3,2) + 0.1,
      mar = c(3,2,1.5,1.5) + 0.1, cex.main = 1.2)
  
  
  #################################################################################################
  ########based on estimated propensity of getting treatment pattern 11
  #######treat11
  group11=newData11$sw[simdatFit$A1==1 & simdatFit$A2==1]
  overlap=c(min(group11), max(group11))
  sum((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))/dim(newData11)[1]
  
  plot(density(newData11$sw[which(simdatFit$A1==1 & simdatFit$A2==1)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", ylab="Density Function", main="Propensity of (1, 1)", ylim=c(0,10))
  lines(density(newData11$sw[-c(which(simdatFit$A1==1 & simdatFit$A2==1))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("Observed (1,1)", "Observed not (1,1)"), cex=1.2, lty=1:2, col=c("black", "red"))
  mtext(expression(bold(paste("Density Function"))), line = 2, at=5, side=2)
  
  simdatFit$included11=0  ###included for 01
  simdatFit$included11[which((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))]=1
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  #######treat00
  group00=newData00$sw[simdatFit$A1==0 & simdatFit$A2==0]
  overlap=c(min(group00), max(group00))
  sum((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))/dim(newData00)[1]
  
  plot(density(newData00$sw[which(simdatFit$A1==0 & simdatFit$A2==0)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", ylab="Density Function", main="Propensity of (0, 0)", ylim=c(0,10))
  lines(density(newData00$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==0))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("Observed (0,0)", "Observed not (0,0)"), cex=1.2, lty=1:2, col=c("black", "red"))
  mtext(expression(bold(paste("Density Function"))), line = 2, at=5, side=2)
  
  simdatFit$included00=0  ###included for 01
  simdatFit$included00[which((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))]=1
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  #######treat01
  group01=newData01$sw[simdatFit$A1==0 & simdatFit$A2==1]
  overlap=c(min(group01), max(group01))
  sum((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))/dim(newData01)[1]
  
  plot(density(newData01$sw[which(simdatFit$A1==0 & simdatFit$A2==1)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", ylab="Density Function", main="Propensity of (0, 1)", ylim=c(0,10))
  lines(density(newData01$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==1))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("Observed (0,1)", "Observed not (0,1)"), cex=1.2, lty=1:2, col=c("black", "red"))
  mtext(expression(bold(paste("Density Function"))), line = 2, at=5, side=2)
  
  simdatFit$included01=0  ###included for 01
  simdatFit$included01[which((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))]=1
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  group10=newData10$sw[simdatFit$A1==1 & simdatFit$A2==0]
  overlap=c(min(group10), max(group10))
  sum((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))/dim(newData10)[1]
  
  plot(density(newData10$sw[which(simdatFit$A1==1 & simdatFit$A2==0)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", ylab="Density Function", main="Propensity of (1, 0)", ylim=c(0,35))
  lines(density(newData10$sw[-c(which(simdatFit$A1==1 & simdatFit$A2==0))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("Observed (1,0)", "Observed not (1,0)"), cex=1.2, lty=1:2, col=c("black", "red"))
  mtext(expression(bold(paste("Density Function"))), line = 2, at=5, side=2)
  
  simdatFit$included10=0  ###included for 01
  simdatFit$included10[which((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))]=1
  
  
  simdatFit$includeAll_01=as.numeric(simdatFit$included00==1 & simdatFit$included01==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_01)
  
  
  simdatFit$includeAll_11=as.numeric(simdatFit$included00==1 & simdatFit$included11==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_11)
  
  
  simdatFit$includeAll_10=as.numeric(simdatFit$included00==1 & simdatFit$included10==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_10)
  
  
  
  dev.off()
  
  
  ###################### this calculates the overlap proportion in Table 31 ##################
  cutoff=quantile(group11, probs = c(0.05, 0.95))
  others=newData11$sw[-c(which(simdatFit$A1==1 & simdatFit$A2==1))]
  overlapProp[d,3]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  cutoff=quantile(group00, probs = c(0.05, 0.95))
  others=newData00$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==0))]
  overlapProp[d,4]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  
  ##############################
  cutoff=quantile(group10, probs = c(0.05, 0.95))
  others=newData10$sw[-c(which(simdatFit$A1==1 & simdatFit$A2==0))]
  overlapProp[d,5]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  cutoff=quantile(group00, probs = c(0.05, 0.95))
  others=newData00$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==0))]
  overlapProp[d,6]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  ##############################
  cutoff=quantile(group01, probs = c(0.05, 0.95))
  others=newData01$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==1))]
  overlapProp[d,7]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
  cutoff=quantile(group00, probs = c(0.05, 0.95))
  others=newData00$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==0))]
  overlapProp[d,8]=c(sum(others >= cutoff[1] & others <= cutoff[2])/length(others))
  
}


#row.names(weightSum)=paste("window", 1:21, sep="")
colnames(weightSum)=c("mean", "sd", "min", "max")

write.csv(weightSum, paste(DIRECOUT, "weight_sample.csv", sep=""))  ###summary of weights across all the windows
write.csv(overlapProp, paste(DIRECOUT, "overlapProportion.csv", sep="")) ##summary of overlap proportions between treatment groups


########THis is to produce weight.txt and overlapProportion.txt, which can be copied and pasted into manuscript latex file to generate Table 30 and 31 in the Appendix

########################### to create Table 30 in the Appendix
weightSum=read.csv(paste(DIRECOUT, "weight_sample.csv", sep=""), header=T)
weightSum=format(weightSum, digits=3)

outPut=weightSum[6:21,]

outPut=cbind(paste("Window", 1:dim(outPut)[1], sep=""), rep("&", dim(outPut)[1]),
             outPut[,c(2)], rep("(", dim(outPut)[1]), outPut[,c(3)], rep(")", dim(outPut)[1]),rep("&", dim(outPut)[1]),
             outPut[,c(4)], rep("/", dim(outPut)[1]), outPut[,c(5)],
             rep("\\\\", dim(outPut)[1]))

write.table(outPut, paste(DIRECOUT, "weight.txt",sep=""),  quote=F, col.names=F, row.names=F)



############################to create Table 31 in the appendix
propSum=read.csv(paste(DIRECOUT, "overlapProportion.csv", sep=""), header=T)
propSum=propSum*100
propSum=propSum[,c(2,3,4,6,8,9)]
propSum=format(propSum, digits=0)
outPut=propSum[6:21,]


outPut=cbind(paste("Window", 1:dim(outPut)[1], sep=""),
             rep("&", dim(outPut)[1]), outPut[,c(1)], 
             rep("&", dim(outPut)[1]), outPut[,c(2)], 
             rep("&", dim(outPut)[1]), outPut[,c(3)], 
             rep("&", dim(outPut)[1]), outPut[,c(4)],
             rep("&", dim(outPut)[1]), outPut[,c(5)],
             rep("&", dim(outPut)[1]), outPut[,c(6)],
             rep("\\\\", dim(outPut)[1]))

write.table(outPut, paste(DIRECOUT, "overlapProportion.txt",sep=""),  quote=F, col.names=F, row.names=F)


