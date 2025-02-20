##############################################################################################
##############################################################################################
################PENCOMP #########################################################################
rm(list=ls())
library(nlme)

DIREC="M:/Private/Rpackage_v3/resubmission_round4/codes/Application/"  ###where the dataset and functions are stored

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

numRun=end


#######excluding nonoverlaps
estFinal_ps11=numeric(numRun)
sdFinal_ps11=numeric(numRun)
CIFinal_ps11=matrix(0, nrow=numRun,ncol=2)

estFinal_ps10=numeric(numRun)
sdFinal_ps10=numeric(numRun)
CIFinal_ps10=matrix(0, nrow=numRun,ncol=2)

estFinal_ps01=numeric(numRun)
sdFinal_ps01=numeric(numRun)
CIFinal_ps01=matrix(0, nrow=numRun,ncol=2)


sampleNum=matrix(0, nrow=numRun,ncol=8)  #### number of samples in each of the four treatment combinations
numNon=numeric(numRun)


##############without excluding nonoverlaps
estFinal_ps11_all=numeric(numRun)
sdFinal_ps11_all=numeric(numRun)
CIFinal_ps11_all=matrix(0, nrow=numRun,ncol=2)

estFinal_ps10_all=numeric(numRun)
sdFinal_ps10_all=numeric(numRun)
CIFinal_ps10_all=matrix(0, nrow=numRun,ncol=2)

estFinal_ps01_all=numeric(numRun)
sdFinal_ps01_all=numeric(numRun)
CIFinal_ps01_all=matrix(0, nrow=numRun,ncol=2)



for (d in start:end) {
  
  print(d)
  
  set.seed(d)
  
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
  
  
  
  simdat$yearPos=simdat2$yearPos
  simdat$yearPosInd=numeric(length=dim(simdat)[1])
  simdat$yearPosInd[which(simdat$yearPos==1984)]=1
  simdat$yearPosInd[which(simdat$yearPos>=1985 & simdat$yearPos<=1987)]=2
  simdat$yearPosInd[which(simdat$yearPos>=1988)]=3
  simdat$yearPosInd=as.factor(simdat$yearPosInd)
  table(simdat$yearPosInd)
  
  simdat$dosage=simdat2[,which(names(simdat2)==paste("dosageFill", (d), sep=""))]
  
  
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
  ######
  model2a=glm(A1 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + dosage, data=simdatFit, family="binomial") 
  temp2a=(simdatFit$A1)*model2a$fitted.values + (1-simdatFit$A1)*(1-model2a$fitted.values)
  summary(model2a)
  
  model2b=glm(A2 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + A1 + dosage, 
              data=simdatFit, family="binomial") ### correctly specified treatment mechanism 
  temp2b=(simdatFit$A2)*model2b$fitted.values + (1-simdatFit$A2)*(1-model2b$fitted.values)
  summary(model2b)
  
  simdatFit$sw=log((temp2a*temp2b)/(1-temp2a*temp2b))
  
  simdatFit$pslogit = log(temp2a/(1-temp2a))  ####here the name pslogit is probability of observed treatment at first time point
  
  
  ##############################################################
  ###use equally spaced fixed knots assuming K knots
  simdatFit0=simdatFit[simdatFit$A1==0,]
  K1=min(c(floor(0.25*dim(simdatFit0)[1]),35))
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
  
  
  ############# IMPUTE THE MISSING VALUES IN THE ORIGINAL DATA SET#####################################
  newData0=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                      WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1,
                      LEU3N2=rep(NA, dim(simdatFit)[1]), 
                      RBC2=rep(NA, dim(simdatFit)[1]),
                      college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                      dosage=simdatFit$dosage, 
                      A1=rep(0, dim(simdatFit)[1]))
  newData0$id=1
  predict2a=1-predict(model2a, newData0, type="response")
  newData0$pslogit=log(predict2a/(1-predict2a))
  
  
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
  K1=min(c(floor(0.25*dim(simdatFit1)[1]),35))
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
  ############# IMPUTE THE MISSING VALUES IN THE ORIGINAL DATA SET#####################################
  newData1=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                      WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1,
                      LEU3N2=rep(NA, dim(simdatFit)[1]), 
                      RBC2=rep(NA, dim(simdatFit)[1]),  
                      college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                      dosage=simdatFit$dosage, 
                      A1=rep(1, dim(simdatFit)[1]))
  newData1$id=1
  predict2a=predict(model2a, newData1, type="response")
  newData1$pslogit=log(predict2a/(1-predict2a))
  
  
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
  ####################IMPUTING THE MISSING VALUES IN THE ORIGINAL DATA SET################################
  newData00=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(0, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]))
  predict2a=1-predict(model2a, newData00, type="response")
  predict2b=1-predict(model2b, newData00, type="response")
  newData00$sw=predict2a * predict2b
  newData00$id=1
  
  
  newData01=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(0, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]))
  predict2a=1-predict(model2a, newData01, type="response")    
  predict2b=predict(model2b, newData01, type="response")
  newData01$sw=predict2a * predict2b
  newData01$id=1
  
  
  newData10=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(1, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]))
  predict2a=predict(model2a, newData10, type="response")    
  predict2b=1-predict(model2b, newData10, type="response")
  newData10$sw=predict2a * predict2b
  newData10$id=1
  
  
  newData11=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                       WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, 
                       college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                       dosage=simdatFit$dosage, 
                       A1=rep(1, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]))
  predict2a=predict(model2a, newData11, type="response")    
  predict2b=predict(model2b, newData11, type="response")
  newData11$sw=predict2a * predict2b
  newData11$id=1
  
  
  
  #################################################################################################
  ##FIND THE OVERLAPPING REGIONS AT FIRST TIME POINT #########################
  ####looking of probability of getting treated for both control and treated groups
  probTreat=predict(model2a, simdatFit, type="response")
  summary(probTreat)
  
  overlapTreat=c(max(min(probTreat[which(simdatFit$A1==1)]), min(probTreat[which(simdatFit$A1==0)])),
                 min(max(probTreat[which(simdatFit$A1==1)]), max(probTreat[which(simdatFit$A1==0)])))
  
  # the input of the following function MUST be a numeric list
  plot(density(probTreat[simdatFit$A1==0], from=0, 
               to=1), lty=1, lwd=2, col="black", xlab="Propensity Score", main="")
  lines(density(probTreat[simdatFit$A1==1],from=0, to=1), lty=2, lwd=2, col="red")
  legend("topright", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  ##############################
  ####looking of probability of getting treated for both control and treated groups
  probControl=1-predict(model2a, simdatFit, type="response")
  overlapControl=c(max(min(probControl[which(simdatFit$A1==1)]), min(probControl[which(simdatFit$A1==0)])),
                   min(max(probControl[which(simdatFit$A1==1)]), max(probControl[which(simdatFit$A1==0)])))
  
  plot(density(probControl[simdatFit$A1==0], from=0, 
               to=1), lty=1, lwd=2, col="black", xlab="Propensity Score", main="")
  lines(density(probControl[simdatFit$A1==1],from=0, to=1),
        lty=2, lwd=2, col="red")
  legend("topleft", c("control","treated"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  
  simdatFit$includedT1=(probTreat >= overlapTreat[1] &  probTreat <= overlapTreat[2]) *
    (probControl >= overlapControl[1] &  probControl <= overlapControl[2])
  sum(simdatFit$includedT1)  
  
  
  #################################################################################################
  ########based on estimated propensity of getting treatment pattern 01
  group11=newData11$sw[simdatFit$A1==1 & simdatFit$A2==1]
  group10=newData11$sw[simdatFit$A1==1 & simdatFit$A2==0]
  group01=newData11$sw[simdatFit$A1==0 & simdatFit$A2==1]
  group00=newData11$sw[simdatFit$A1==0 & simdatFit$A2==0]
  
  overlap=c(max(min(group11), min(group10), min(group01), min(group00)),
            min(max(group11), max(group10), max(group01), max(group00)))
  
  sum((group11>=overlap[1]) & (group11<=overlap[2]))/length(group11)
  sum((group10>=overlap[1]) & (group10<=overlap[2]))/length(group10)
  sum((group01>=overlap[1]) & (group01<=overlap[2]))/length(group01)
  sum((group00>=overlap[1]) & (group00<=overlap[2]))/length(group00)
  
  
  plot(density(newData11$sw[which(simdatFit$A1==1 & simdatFit$A2==1)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", main="", ylim=c(0,10))
  lines(density(newData01$sw[-c(which(simdatFit$A1==1 & simdatFit$A2==1))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("(A0,A1)=(1,1)", "(A0,A1) not (1,1)"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  
  simdatFit$included11=0  ###included for 01
  simdatFit$included11[which((newData11$sw >= overlap[1]) & (newData11$sw <= overlap[2]))]=1
  sum(simdatFit$included11)
  
  rm(overlap)
  rm(group11)
  rm(group10)
  rm(group01)
  rm(group00)
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  group11=newData00$sw[simdatFit$A1==1 & simdatFit$A2==1]
  group10=newData00$sw[simdatFit$A1==1 & simdatFit$A2==0]
  group01=newData00$sw[simdatFit$A1==0 & simdatFit$A2==1]
  group00=newData00$sw[simdatFit$A1==0 & simdatFit$A2==0]
  
  overlap=c(max(min(group11), min(group10), min(group01), min(group00)),
            min(max(group11), max(group10), max(group01), max(group00)))
  
  sum((group11>=overlap[1]) & (group11<=overlap[2]))/length(group11)
  sum((group10>=overlap[1]) & (group10<=overlap[2]))/length(group10)
  sum((group01>=overlap[1]) & (group01<=overlap[2]))/length(group01)
  sum((group00>=overlap[1]) & (group00<=overlap[2]))/length(group00)
  
  
  plot(density(newData00$sw[which(simdatFit$A1==0 & simdatFit$A2==0)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", main="", ylim=c(0,10))
  lines(density(newData00$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==0))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("(A0,A1)=(1,1)", "(A0,A1) not (1,1)"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  
  simdatFit$included00=0  ###included for 00
  simdatFit$included00[which((newData00$sw >= overlap[1]) & (newData00$sw <= overlap[2]))]=1
  sum(simdatFit$included00)
  
  rm(overlap)
  rm(group11)
  rm(group10)
  rm(group01)
  rm(group00)
  
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  group11=newData01$sw[simdatFit$A1==1 & simdatFit$A2==1]
  group10=newData01$sw[simdatFit$A1==1 & simdatFit$A2==0]
  group01=newData01$sw[simdatFit$A1==0 & simdatFit$A2==1]
  group00=newData01$sw[simdatFit$A1==0 & simdatFit$A2==0]
  
  overlap=c(max(min(group11), min(group10), min(group01), min(group00)),
            min(max(group11), max(group10), max(group01), max(group00)))
  
  sum((group11>=overlap[1]) & (group11<=overlap[2]))/length(group11)
  sum((group10>=overlap[1]) & (group10<=overlap[2]))/length(group10)
  sum((group01>=overlap[1]) & (group01<=overlap[2]))/length(group01)
  sum((group00>=overlap[1]) & (group00<=overlap[2]))/length(group00)
  
  
  plot(density(newData01$sw[which(simdatFit$A1==0 & simdatFit$A2==1)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", main="", ylim=c(0,10))
  lines(density(newData01$sw[-c(which(simdatFit$A1==0 & simdatFit$A2==1))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("(A0,A1)=(0,1)", "(A0,A1) not (0,1)"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  
  simdatFit$included01=0  ###included for 00
  simdatFit$included01[which((newData01$sw >= overlap[1]) & (newData01$sw <= overlap[2]))]=1
  sum(simdatFit$included01)
  
  
  ####################################################################################
  ########based on estimated propensity of getting treatment pattern 00
  group11=newData10$sw[simdatFit$A1==1 & simdatFit$A2==1]
  group10=newData10$sw[simdatFit$A1==1 & simdatFit$A2==0]
  group01=newData10$sw[simdatFit$A1==0 & simdatFit$A2==1]
  group00=newData10$sw[simdatFit$A1==0 & simdatFit$A2==0]
  
  overlap=c(max(min(group11), min(group10), min(group01), min(group00)),
            min(max(group11), max(group10), max(group01), max(group00)))
  
  sum((group11>=overlap[1]) & (group11<=overlap[2]))/length(group11)
  sum((group10>=overlap[1]) & (group10<=overlap[2]))/length(group10)
  sum((group01>=overlap[1]) & (group01<=overlap[2]))/length(group01)
  sum((group00>=overlap[1]) & (group00<=overlap[2]))/length(group00)
  
  
  plot(density(newData10$sw[which(simdatFit$A1==1 & simdatFit$A2==0)], from=0, to=1), lty=1, lwd=2, col="black",
       xlab="Propensity Score", main="", ylim=c(0,30))
  lines(density(newData10$sw[-c(which(simdatFit$A1==1 & simdatFit$A2==0))], from=0, to=1),lty=2, lwd=2, col="red")
  legend("topright", c("(A0,A1)=(1,0)", "(A0,A1) not (1,0)"), cex=1.2, lty=1:2, col=c("black", "red"))
  
  
  simdatFit$included10=0  ###included for 00
  simdatFit$included10[which((newData10$sw >= overlap[1]) & (newData10$sw <= overlap[2]))]=1
  
  rm(overlap)
  rm(group11)
  rm(group10)
  rm(group01)
  rm(group00)
  
  
  
  simdatFit$includeAll_01=as.numeric(simdatFit$included00==1 & simdatFit$included01==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_01)
  
  
  simdatFit$includeAll_11=as.numeric(simdatFit$included00==1 & simdatFit$included11==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_11)
  
  
  simdatFit$includeAll_10=as.numeric(simdatFit$included00==1 & simdatFit$included10==1 & simdatFit$includedT1==1)
  sum(simdatFit$includeAll_10)
  
  
  
  sampleNum[d,]=c(sum(simdatFit$A1==1 & simdatFit$A2==1),
                  sum(simdatFit$A1==1 & simdatFit$A2==0),
                  sum(simdatFit$A1==0 & simdatFit$A2==1),
                  sum(simdatFit$A1==0 & simdatFit$A2==0),dim(simdatFit)[1],
                  sum(simdatFit$includeAll_11), sum(simdatFit$includeAll_10), sum(simdatFit$includeAll_01))
  
  
  
  numLoop=200
  estimate11=numeric(numLoop)
  estimate10=numeric(numLoop)
  estimate01=numeric(numLoop)
  
  ### within imputation variance ###
  estimate11Wd=numeric(numLoop)
  estimate10Wd=numeric(numLoop)
  estimate01Wd=numeric(numLoop)
  
  n=dim(simdatFit)[1]
  
  
  ########without excluding overlaps
  estimate11_all=numeric(numLoop)
  estimate10_all=numeric(numLoop)
  estimate01_all=numeric(numLoop)
  
  
  ### within imputation variance ###
  estimate11Wd_all=numeric(numLoop)
  estimate10Wd_all=numeric(numLoop)
  estimate01Wd_all=numeric(numLoop)

  
  ########################################################################################################################
  ########################################################################################################################  
  ########################################################################################################################
  ########################################################################################################################  
  
  for (h in 1:numLoop) {
    
    tryCatch ( 
      {
        
        set.seed(h)
        print(h)
        
        
        num00=simdatFit$id2[simdatFit$A1==0 & simdatFit$A2==0]
        num01=simdatFit$id2[simdatFit$A1==0 & simdatFit$A2==1]
        num10=simdatFit$id2[simdatFit$A1==1 & simdatFit$A2==0]
        num11=simdatFit$id2[simdatFit$A1==1 & simdatFit$A2==1]
        
        bootSample=simdatFit[c(sample(x=num00, size=length(num00),replace=T),sample(x=num01, size=length(num01),replace=T),
                               sample(x=num10, size=length(num10),replace=T),sample(x=num11, size=length(num11),replace=T)),]
        
        
        ####################################
        model2a=glm(A1 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + dosage, data=bootSample, family="binomial") 
        temp2a=(bootSample$A1)*model2a$fitted.values + (1-bootSample$A1)*(1-model2a$fitted.values)
        summary(model2a)
        
        model2b=glm(A2 ~ LEU2N1 + WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + A1 + dosage, 
                    data=bootSample, family="binomial") ### correctly specified treatment mechanism 
        temp2b=(bootSample$A2)*model2b$fitted.values + (1-bootSample$A2)*(1-model2b$fitted.values)
        summary(model2b)
        
    
        bootSample$sw=log((temp2a*temp2b)/(1-temp2a*temp2b))
        
        bootSample$pslogit = log(temp2a/(1-temp2a))  ####here the name pslogit is probability of observed treatment at first time point
        
        
        ##############################################################
        ###use equally spaced fixed knots assuming K knots
        bootSample0=bootSample[bootSample$A1==0,]
        K1=min(c(floor(0.25*length(unique(bootSample0$pslogit))),35))
        space0=(max(bootSample0$pslogit)-min(bootSample0$pslogit))/(K1+1)
        knots0=(min(bootSample0$pslogit)+space0*(1:K1))
        
        ###assume a truncated linear basis
        linear0=NULL
        for (var in 1:K1) {
          temp=(bootSample0$pslogit-knots0[var])
          temp2=(as.numeric(bootSample0$pslogit<knots0[var]))*0 + (as.numeric(bootSample0$pslogit>=knots0[var]))*temp
          linear0=cbind(linear0, temp2)
        }
        
        colnames(linear0)=paste("basis", 1:K1, sep="")
        
        
        pspp0 <- lme(LEU3N2 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + pslogit, random=list(id=pdIdent(~0+linear0)), data=bootSample0)
        summary(pspp0)
        
        
        ############# IMPUTE THE MISSING VALUES IN THE ORIGINAL DATA SET#####################################
        newData0=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                            WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=rep(NA, dim(simdatFit)[1]), 
                            RBC2=rep(NA, dim(simdatFit)[1]),
                            college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                            dosage=simdatFit$dosage, 
                            A1=rep(0, dim(simdatFit)[1]))
        newData0$id=1
        predict2a=1-predict(model2a, newData0, type="response")
        newData0$pslogit=log(predict2a/(1-predict2a))
        
        
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
        bootSample1=bootSample[bootSample$A1==1,]
        K1=min(c(floor(0.25*length(unique(bootSample1$pslogit))),35))
        space1=(max(bootSample1$pslogit)-min(bootSample1$pslogit))/(K1+1)
        knots1=(min(bootSample1$pslogit)+space1*(1:K1))
        
        ###assume a truncated linear basis
        linear1=NULL
        for (var in 1:K1) {
          temp=(bootSample1$pslogit-knots1[var])
          temp2=(as.numeric(bootSample1$pslogit<knots1[var]))*0 + (as.numeric(bootSample1$pslogit>=knots1[var]))*temp
          linear1=cbind(linear1, temp2)
        }
        
        colnames(linear1)=paste("basis", 1:K1, sep="")
        
        
        pspp1 <- lme(LEU3N2 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + pslogit, random=list(id=pdIdent(~0+linear1)), data=bootSample1)
        summary(pspp1)
        
        
        ##################################################
        ############# IMPUTE THE MISSING VALUES IN THE ORIGINAL DATA SET#####################################
        newData1=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                            WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=rep(NA, dim(simdatFit)[1]), 
                            RBC2=rep(NA, dim(simdatFit)[1]),  
                            college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                            dosage=simdatFit$dosage, 
                            A1=rep(1, dim(simdatFit)[1]))
        newData1$id=1
        predict2a=predict(model2a, newData1, type="response")
        newData1$pslogit=log(predict2a/(1-predict2a))
        
        
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
        ####################IMPUTING THE MISSING VALUES IN THE ORIGINAL DATA SET################################
        newData00=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                             WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                             college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                             dosage=simdatFit$dosage, 
                             A1=rep(0, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]))
        predict2a=1-predict(model2a, newData00, type="response")
        predict2b=1-predict(model2b, newData00, type="response")
        newData00$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
        newData00$id=1
        
        
        newData01=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                             WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData0$LEU3N2,  RBC2=newData0$RBC2, 
                             college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                             dosage=simdatFit$dosage, 
                             A1=rep(0, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]))
        predict2a=1-predict(model2a, newData01, type="response")    
        predict2b=predict(model2b, newData01, type="response")
        newData01$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
        newData01$id=1
        
        
        newData10=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                             WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, 
                             college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                             dosage=simdatFit$dosage, 
                             A1=rep(1, dim(simdatFit)[1]), A2=rep(0, dim(simdatFit)[1]))
        predict2a=predict(model2a, newData10, type="response")    
        predict2b=1-predict(model2b, newData10, type="response")
        newData10$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
        newData10$id=1
        
        
        newData11=data.frame(RBC1=simdatFit$RBC1,LEU3N1=simdatFit$LEU3N1, 
                             WBC1=simdatFit$WBC1,LEU2N1=simdatFit$LEU2N1, PLATE1=simdatFit$PLATE1, LEU3N2=newData1$LEU3N2,  RBC2=newData1$RBC2, 
                             college=simdatFit$college, white=simdatFit$white, age=simdatFit$age, yearPosInd=simdatFit$yearPosInd,
                             dosage=simdatFit$dosage, 
                             A1=rep(1, dim(simdatFit)[1]), A2=rep(1, dim(simdatFit)[1]))
        predict2a=predict(model2a, newData11, type="response")    
        predict2b=predict(model2b, newData11, type="response")
        newData11$sw=log((predict2a * predict2b)/(1-predict2a * predict2b))
        newData11$id=1
        
        
        #######################################################################################################
        ###use equally spaced fixed knots assuming K knots
        ###use equally spaced fixed knots assuming K knots
        bootSample00=bootSample[bootSample$A1==0 & bootSample$A2==0,]
        K1=min(c(floor(0.25*length(unique(bootSample00$sw))),35))
        space00=(max(bootSample00$sw)-min(bootSample00$sw))/(K1+1)
        knots00=(min(bootSample00$sw)+space00*(1:K1))
        
        ###assume a truncated linear basis
        linear00=NULL
        for (var in 1:K1) {
          temp=(bootSample00$sw-knots00[var])
          temp2=(as.numeric(bootSample00$sw<knots00[var]))*0 + (as.numeric(bootSample00$sw>=knots00[var]))*temp
          linear00=cbind(linear00, temp2)
        }
        
        colnames(linear00)=paste("basis00_", 1:K1, sep="")
        
        
        pspp00=lme(LEU3N3 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + sw, random=list(id=pdIdent(~0+linear00)), data=bootSample00)
        summary(pspp00)
        
        ########################################################################################
        linear00=NULL
        for (var in 1:K1) {
          temp=(newData00$sw-knots00[var])
          temp2=(as.numeric(newData00$sw<knots00[var]))*0 + (as.numeric(newData00$sw>=knots00[var]))*temp
          linear00=cbind(linear00, temp2)
        }
        
        colnames(linear00)=paste("basis00_", 1:K1, sep="")
        
        imputed00=predict(pspp00, newData00) + rnorm(dim(newData00)[1], 0, summary(pspp00)$sigma)
        
        ############################################################################################
        ############################################################################################
        #######################################################################################################
        ###use equally spaced fixed knots assuming K knots
        bootSample01=bootSample[bootSample$A1==0 & bootSample$A2==1,]
        K1=min(c(floor(0.25*length(unique(bootSample01$sw))),35))
        space01=(max(bootSample01$sw)-min(bootSample01$sw))/(K1+1)
        knots01=(min(bootSample01$sw)+space01*(1:K1))
        
        ###assume a truncated linear basis
        linear01=NULL
        for (var in 1:K1) {
          temp=(bootSample01$sw-knots01[var])
          temp2=(as.numeric(bootSample01$sw<knots01[var]))*0 + (as.numeric(bootSample01$sw>=knots01[var]))*temp
          linear01=cbind(linear01, temp2)
        }
        
        colnames(linear01)=paste("basis01_", 1:K1, sep="")
        
        if(length(unique(bootSample01$sw)) <= 50){
          
          pspp01=lme(LEU3N3 ~ LEU3N2 + sw, random=list(id=pdIdent(~0+linear01)),  data=bootSample01)
          
        } else {
          
          pspp01=lme(LEU3N3 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + sw, random=list(id=pdIdent(~0+linear01)),  data=bootSample01)
          
        }
        
        summary(pspp01)
        
        ########################################################################################
        linear01=NULL
        for (var in 1:K1) {
          temp=(newData01$sw-knots01[var])
          temp2=(as.numeric(newData01$sw<knots01[var]))*0 + (as.numeric(newData01$sw>=knots01[var]))*temp
          linear01=cbind(linear01, temp2)
        }
        
        colnames(linear01)=paste("basis01_", 1:K1, sep="")
        
        imputed01=predict(pspp01, newData01) + rnorm(dim(newData01)[1], 0, summary(pspp01)$sigma)
        
        
        ############################################################################################
        ############################################################################################
        #######################################################################################################
        ###use equally spaced fixed knots assuming K knots
        bootSample10=bootSample[bootSample$A1==1 & bootSample$A2==0,]
        K1=min(c(floor(0.25*length(unique(bootSample10$sw))),35))
        space10=(max(bootSample10$sw)-min(bootSample10$sw))/(K1+1)
        knots10=(min(bootSample10$sw)+space10*(1:K1))
        
        ###assume a truncated linear basis
        linear10=NULL
        for (var in 1:K1) {
          temp=(bootSample10$sw-knots10[var])
          temp2=(as.numeric(bootSample10$sw<knots10[var]))*0 + (as.numeric(bootSample10$sw>=knots10[var]))*temp
          linear10=cbind(linear10, temp2)
        }
        
        colnames(linear10)=paste("basis10_", 1:K1, sep="")
        
        if(length(unique(bootSample10$sw)) <= 50) {
          
          pspp10=lme(LEU3N3 ~ LEU3N2 + sw, random=list(id=pdIdent(~0+linear10)),  data=bootSample10) 
          
        } else {
          
          pspp10=lme(LEU3N3 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + sw, random=list(id=pdIdent(~0+linear10)),  data=bootSample10) 
        }
        
        ########################################################################################
        linear10=NULL
        for (var in 1:K1) {
          temp=(newData10$sw-knots10[var])
          temp2=(as.numeric(newData10$sw<knots10[var]))*0 + (as.numeric(newData10$sw>=knots10[var]))*temp
          linear10=cbind(linear10, temp2)
        }
        
        colnames(linear10)=paste("basis10_", 1:K1, sep="")
        
        imputed10=predict(pspp10, newData10) + rnorm(dim(newData10)[1], 0, summary(pspp10)$sigma)
        
        
        ############################################################################################
        #######################################################################################################
        ###use equally spaced fixed knots assuming K knots
        bootSample11=bootSample[bootSample$A1==1 & bootSample$A2==1,]
        K1=min(c(floor(0.25*length(unique(bootSample11$sw))),35))
        space11=(max(bootSample11$sw)-min(bootSample11$sw))/(K1+1)
        knots11=(min(bootSample11$sw)+space11*(1:K1))
        
        ###assume a truncated linear basis
        linear11=NULL
        for (var in 1:K1) {
          temp=(bootSample11$sw-knots11[var])
          temp2=(as.numeric(bootSample11$sw<knots11[var]))*0 + (as.numeric(bootSample11$sw>=knots11[var]))*temp
          linear11=cbind(linear11, temp2)
        }
        
        colnames(linear11)=paste("basis11_", 1:K1, sep="")
        
        if(length(unique(bootSample11$sw)<= 50)){
          
          pspp11=lme(LEU3N3 ~ LEU3N2 + sw, random=list(id=pdIdent(~0+linear11)),  data=bootSample11)
          
        } else {
          
          pspp11=lme(LEU3N3 ~ WBC1 + PLATE1 + RBC1 + LEU3N1 + LEU3N2 + sw, random=list(id=pdIdent(~0+linear11)),  data=bootSample11)
          
        }
        
        
        summary(pspp11)
        
        ########################################################################################
        linear11=NULL
        for (var in 1:K1) {
          temp=(newData11$sw-knots11[var])
          temp2=(as.numeric(newData11$sw<knots11[var]))*0 + (as.numeric(newData11$sw>=knots11[var]))*temp
          linear11=cbind(linear11, temp2)
        }
        
        colnames(linear11)=paste("basis11_", 1:K1, sep="")
        
        imputed11=predict(pspp11, newData11) + rnorm(dim(newData11)[1], 0, summary(pspp11)$sigma)
        
        
        imputed00[which(imputed00<0)]=0
        imputed01[which(imputed01<0)]=0
        imputed10[which(imputed10<0)]=0
        imputed11[which(imputed11<0)]=0
        
        ####ORIGINAL DATASET  
        y_00=imputed00  
        y_01=imputed01
        y_10=imputed10  
        y_11=imputed11
        
        ###predict y when A0=0 and A1=0 
        y_00[which(simdatFit$A1==0 & simdatFit$A2==0)] = simdatFit$LEU3N3[which(simdatFit$A1==0 & simdatFit$A2==0)]
        y_01[which(simdatFit$A1==0 & simdatFit$A2==1)] = simdatFit$LEU3N3[which(simdatFit$A1==0 & simdatFit$A2==1)]
        y_10[which(simdatFit$A1==1 & simdatFit$A2==0)] = simdatFit$LEU3N3[which(simdatFit$A1==1 & simdatFit$A2==0)]
        y_11[which(simdatFit$A1==1 & simdatFit$A2==1)] = simdatFit$LEU3N3[which(simdatFit$A1==1 & simdatFit$A2==1)]
        
        
        mean(y_11)
        mean(y_10)
        mean(y_01)
        mean(y_00)
        
        
        #################################################################################################
        ### estimate from each imputation ####
        estimate11[h] = mean((y_11-y_00)[which(simdatFit$includeAll_11==1)])
        estimate10[h] = mean((y_10-y_00)[which(simdatFit$includeAll_10==1)])
        estimate01[h] = mean((y_01-y_00)[which(simdatFit$includeAll_01==1)])
        
        ### within impuation variance #####
        estimate11Wd[h] = var((y_11-y_00)[which(simdatFit$includeAll_11==1)])/sum(simdatFit$includeAll_11==1)
        estimate10Wd[h] = var((y_10-y_00)[which(simdatFit$includeAll_10==1)])/sum(simdatFit$includeAll_10==1)
        estimate01Wd[h] = var((y_01-y_00)[which(simdatFit$includeAll_01==1)])/sum(simdatFit$includeAll_01==1)
        
        
        
        
        
        #################################################################################################
        ### Without excluding nonoverlaps  estimate from each imputation ####
        estimate11_all[h] = mean(y_11-y_00)
        estimate10_all[h] = mean(y_10-y_00)
        estimate01_all[h] = mean(y_01-y_00)
        
        ### within impuation variance #####
        estimate11Wd_all[h] = var(y_11-y_00)/dim(simdatFit)[1]
        estimate10Wd_all[h] = var(y_10-y_00)/dim(simdatFit)[1]
        estimate01Wd_all[h] = var(y_01-y_00)/dim(simdatFit)[1]
        
        
        
      }
      ,
      error=function(e) { },
      warning=function(w) {return(c(d, h,w))} 
    )
  }
  
  numImput=sum(estimate01!=0)
  
  theta_d=c(mean(estimate11[estimate11!=0]), mean(estimate10[estimate10!=0]), mean(estimate01[estimate01!=0])) 
  ### within imputation variance
  Wd=c(mean(estimate11Wd[estimate11Wd!=0]), mean(estimate10Wd[estimate10Wd!=0]), mean(estimate01Wd[estimate01Wd!=0]))
  
  ###between imputation variance
  Bd=(1/(numImput-1))*c(sum((estimate11[estimate11!=0]-mean(estimate11[estimate11!=0]))^2), 
                        sum((estimate10[estimate10!=0]-mean(estimate10[estimate10!=0]))^2),
                        sum((estimate01[estimate01!=0]-mean(estimate01[estimate01!=0]))^2))
  Bd
  
  ###total variability associated with mean
  Td=Wd+(1+1/numImput)*Bd
  Td
  ##degree of freedom
  v=(numImput-1)*(1+(1/(1/numImput+1))*(Wd/Bd))^2  ### corrected this as well 8/7/2017
  estFinal_ps11[d]=theta_d[1]  ###estimate
  estFinal_ps10[d]=theta_d[2]  ###estimate
  estFinal_ps01[d]=theta_d[3]  ###estimate
  
  CIFinal_ps11[d,]=theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1])  ###
  CIFinal_ps10[d,]=theta_d[2] + c(-1,1)*qt(0.975,v[2])*sqrt(Td[2])  ###
  CIFinal_ps01[d,]=theta_d[3] + c(-1,1)*qt(0.975,v[3])*sqrt(Td[3])  ###
  
  sdFinal_ps11[d]=sqrt(Td[1])  ###standard error
  sdFinal_ps10[d]=sqrt(Td[2])  ###standard error
  sdFinal_ps01[d]=sqrt(Td[3])  ###standard error
  

  ##################################################################################################
  ##################################################################################################
  ########################Excluding the overlaps ###################################################
  theta_d=NULL
  Wd=NULL
  Bd=NULL
  Td=NULL
  v=NULL
  
  theta_d=c(mean(estimate11_all[estimate11_all!=0]), mean(estimate10_all[estimate10_all!=0]), mean(estimate01_all[estimate01_all!=0])) 
  ### within imputation variance
  Wd=c(mean(estimate11Wd_all[estimate11Wd_all!=0]), mean(estimate10Wd_all[estimate10Wd_all!=0]), mean(estimate01Wd_all[estimate01Wd_all!=0]))
  
  ###between imputation variance
  Bd=(1/(numImput-1))*c(sum((estimate11_all[estimate11_all!=0]-mean(estimate11_all[estimate11_all!=0]))^2), 
                        sum((estimate10_all[estimate10_all!=0]-mean(estimate10_all[estimate10_all!=0]))^2),
                        sum((estimate01_all[estimate01_all!=0]-mean(estimate01_all[estimate01_all!=0]))^2))
  Bd
  
  ###total variability associated with mean
  Td=Wd+(1+1/numImput)*Bd
  Td
  ##degree of freedom
  v=(numImput-1)*(1+(1/(1/numImput+1))*(Wd/Bd))^2
  estFinal_ps11_all[d]=theta_d[1]  ###estimate
  estFinal_ps10_all[d]=theta_d[2]  ###estimate
  estFinal_ps01_all[d]=theta_d[3]  ###estimate
  
  CIFinal_ps11_all[d,]=theta_d[1] + c(-1,1)*qt(0.975,v[1])*sqrt(Td[1])  ###
  CIFinal_ps10_all[d,]=theta_d[2] + c(-1,1)*qt(0.975,v[2])*sqrt(Td[2])  ###
  CIFinal_ps01_all[d,]=theta_d[3] + c(-1,1)*qt(0.975,v[3])*sqrt(Td[3])  ###
  
  sdFinal_ps11_all[d]=sqrt(Td[1])  ###standard error
  sdFinal_ps10_all[d]=sqrt(Td[2])  ###standard error
  sdFinal_ps01_all[d]=sqrt(Td[3])  ###standard error
  
  
}

############store in the result folders###########################
write.table(sampleNum[7:21,], paste0(DIREC, "Results/sampleSizeSum.txt"), sep="\t", row.names = F, 
            quote = F)

#############################################################

resultTable=data.frame(estFinal_ps11, sdFinal_ps11,  CIFinal_ps11, estFinal_ps10,  sdFinal_ps10,  CIFinal_ps10, 
                       estFinal_ps01,  sdFinal_ps01, CIFinal_ps01)


row.names(resultTable)=c(paste("window", 1:21, sep=""))
write.table(resultTable[7:21,], paste(DIREC, "/Results/tablePENCOMP_truncate.txt", sep=""), sep="\t", quote=F)



#################
resultTable_all=data.frame(estFinal_ps11_all, sdFinal_ps11_all, CIFinal_ps11_all, estFinal_ps10_all, sdFinal_ps10_all,CIFinal_ps10_all,
                           estFinal_ps01_all, sdFinal_ps01_all, CIFinal_ps01_all)


row.names(resultTable_all)=c(paste("window", 1:21, sep=""))
write.table(resultTable_all[7:21,], paste(DIREC, "/Results/tablePENCOMP.txt", sep=""), sep="\t", quote=F)

