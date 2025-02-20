##############################################################################################
##############################################################################################
rm(list=ls())
library(splines)
library(nlme)
require(stats)
require(graphics)
library("rootSolve")

DIREC="codes/Application/"  ###where the dataset and functions are stored

source(paste0(DIREC, "addFun.R") )  ###additional funcs for processing
source(paste0(DIREC,"AIPTW.R") )  ###function for obtaining AIPTW estimate
source(paste0(DIREC,"gcompute.R") )  ##function for obtaining g computation estimate
source(paste0(DIREC,"gcomputeFunc.R") ) ##addition function for g computation
source(paste0(DIREC,"IPTW.R") )###function for obtaining IPTW estimate
source(paste0(DIREC,"naive.R") ) ###function for obtaining naive estimate
source(paste0(DIREC,"pencomp.R") )  ###function for obtaining pencomp estimate
source(paste0(DIREC,"pencompFit.R") )


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
###the start and the end of the visits considered in this analysis
start= 7
end= 21

numRun=end

tablePENCOMP = matrix(NA, nrow =  numRun, ncol = 24 )
tableIPTW = matrix(NA, nrow =  numRun, ncol = 24)
tableGcompute = matrix(NA, nrow =  numRun, ncol = 12)
tableNaive = matrix(NA, nrow =  numRun, ncol = 12)
tableAIPTW = matrix(NA, nrow =  numRun, ncol = 12)
tableAIPTW_truncate = matrix(NA, nrow =  numRun, ncol = 12)

sampleSizeSum = matrix(NA, nrow=numRun, ncol=8)  ###sample size by treatment regime for every 3-visit moving window

for (k in start:end) {
  
  print(k)
  
  set.seed(k)
  
  ############for every 3-visit moving window, run the following script
  ########################################################
  simdat=data.frame(CASEID=simdat2$CASEID)
  
  ###treatment indicator at first time point A1, at second time point A2
  simdat$A1=simdat2[,which(names(simdat2)==paste("TreatVisit", (k+1)*10, sep=""))] ###treatment at first time point
  simdat$A2=simdat2[,which(names(simdat2)==paste("TreatVisit", (k+2)*10, sep=""))]  ##treatment at second time point
  
  ###CD4 count, LEU3N1 at first time point, LEU3N2 at second time point, LEU3N3-final outcome of interest after the second treatment A2
  simdat$LEU3N1=simdat2[,which(names(simdat2)==paste("LEU3N.", (k)*10, sep=""))]  ###
  simdat$LEU3N2=simdat2[,which(names(simdat2)==paste("LEU3N.", (k+1)*10, sep=""))]
  simdat$LEU3N3=simdat2[,which(names(simdat2)==paste("LEU3N.", (k+2)*10, sep=""))]
  
  ##CD8 counts
  simdat$LEU2N1=simdat2[,which(names(simdat2)==paste("LEU2N.", (k)*10, sep=""))]
  simdat$LEU2N2=simdat2[,which(names(simdat2)==paste("LEU2N.",(k+1)*10, sep=""))]
  simdat$LEU2N3=simdat2[,which(names(simdat2)==paste("LEU2N.", (k+2)*10, sep=""))]
  
  ###white blood cell counts
  simdat$WBC1=simdat2[,which(names(simdat2)==paste("WBC.", (k)*10, sep=""))]
  simdat$WBC2=simdat2[,which(names(simdat2)==paste("WBC.", (k+1)*10, sep=""))]
  simdat$WBC3=simdat2[,which(names(simdat2)==paste("WBC.", (k+2)*10, sep=""))]
  
  
  ###Red blood cell counts
  simdat$RBC1=simdat2[,which(names(simdat2)==paste("RBC.", (k)*10, sep=""))]
  simdat$RBC2=simdat2[,which(names(simdat2)==paste("RBC.", (k+1)*10, sep=""))]
  simdat$RBC3=simdat2[,which(names(simdat2)==paste("RBC.", (k+2)*10, sep=""))]
  
  ###platelet counts
  simdat$PLATE1=simdat2[,which(names(simdat2)==paste("PLATE.", (k)*10, sep=""))]
  simdat$PLATE2=simdat2[,which(names(simdat2)==paste("PLATE.", (k+1)*10, sep=""))]
  simdat$PLATE3=simdat2[,which(names(simdat2)==paste("PLATE.",(k+2)*10, sep=""))]
  
  ##viral load
  simdat$VLOAD1=simdat2[,which(names(simdat2)==paste("VLOAD.", (k)*10, sep=""))]
  simdat$VLOAD2=simdat2[,which(names(simdat2)==paste("VLOAD.", (k+1)*10, sep=""))]
  simdat$VLOAD3=simdat2[,which(names(simdat2)==paste("VLOAD.",(k+2)*10, sep=""))]
  
  ####demographic information
  simdat$age=simdat2[,which(names(simdat2)=="age")]
  simdat$white=simdat2[,which(names(simdat2)=="white")]
  simdat$college=simdat2[,which(names(simdat2)=="college")]
  simdat$deathYear=simdat2[,which(names(simdat2)=="DTHDATEyy")]
  
  ###HIV status
  simdat$hiv1=simdat2[,which(names(simdat2)==paste("hivVisit", (k)*10, sep=""))]
  
  ######dosage at the start of each 3-visit window
  simdat$dosage=simdat2[,which(names(simdat2)==paste("dosageFill", (k), sep=""))]
  
  
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
  
  
  numCarlo=4000 ###number of runs in g computations and estimating augmentation terms in AIPTW
  baselineVar=c("LEU3N1", "LEU2N1" , "WBC1", "RBC1", "PLATE1", "dosage") 
  

  numT=500

  ############################# g computation #################################################
  gcomputeEst=NULL
  gcomputeEst=gcompute(dataInput=simdatFit, original=1, numCarlo=numCarlo, visit=k)
  
  ########bootstrap samples
  mulResult=NULL
  mulResult=replicate(numT, gcompute(dataInput=simdatFit, original=0, numCarlo=numCarlo, visit=k) )
  
  
  for(ind in c(0, 1, 2) ){
    tableGcompute[k, (1:4)+(4*ind)] = c( gcomputeEst[ind+1], sd(mulResult[ind+1,], na.rm = T), gcomputeEst[ind+1] + c(-1,1)*1.96*sd(mulResult[ind+1, ], na.rm = T) )
  }
  
  

  
  ############store in the result folders###########################
  write.table(tableGcompute, paste0(DIREC, "Results/tableGcompute.txt"), sep="\t", row.names = F, 
              quote = F)
  

  
}

