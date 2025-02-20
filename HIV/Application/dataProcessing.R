#### focus on the first cohort only, 1984-1985 cohorts
#### CREATED 6/19/2017
###including all visits between 7-21
### for survey questions or more informations about the study, visit this website https://statepi.jhsph.edu/macs/pdt.html


rm(list=ls())
DIREC="" ### location of all the raw data files
DIRECOUT="codes/Application/dataset/" ##location of where generated datasets will be stored

################MACS ID PARTICIPANTS#################### 
###macsid.csv contains Participant IDs with Recruit status and DOB year
macsid=read.csv(paste(DIREC, "macsid.csv", sep=""), header=T)
names(macsid)
dim(macsid)  ###7343 subjects

####checking the codes
originalCohort=macsid$CASEID[macsid$MACSCODE==20 | macsid$MACSCODE==21]  ###1984-1985 cohorts, our analysis for the paper is restricted to this cohort only
newCohort1988=macsid$CASEID[macsid$MACSCODE==30 | macsid$MACSCODE==31 |  macsid$MACSCODE==36]  ###1988 new cohort
newCohort2001=macsid$CASEID[macsid$MACSCODE==40]  ###2001 new cohort
newCohort2010=macsid$CASEID[macsid$MACSCODE==50 | macsid$MACSCODE==55]  ###2010 new cohort

#originalCohort=c(originalCohort, newCohort1988)  ###combining the first two cohorts

length(originalCohort)  ###4954
length(newCohort1988)  ###668
length(newCohort2001)  ###1350
length(newCohort2010)  ###371


####################################################################################
############HIV status##############################################################
#########obtain HIV status from section 2 questionnaire
section2=read.csv(paste(DIREC, "section2.csv", sep=""), header=T)
section2=section2[which(section2$CASEID %in% originalCohort),]
section2=section2[which(section2$VISIT==10),]  ###some error in the birth year variable
dim(section2)
length(unique(section2$CASEID))  
table(section2$DAT2Y)  ###year of first visit 1984 (4172), 1985(782)
sum(is.na(section2$DAT2Y))  ###year of each visit

section2$college=NA
section2$college[which(section2$EDUCA==5 | section2$EDUCA==6 | section2$EDUCA==7)]=1  ###have at least a college degree
section2$college[which(section2$EDUCA==1 | section2$EDUCA==2 | section2$EDUCA==3 | section2$EDUCA==4)]=0

section2$white=NA
section2$white[which(section2$RACE==1 | section2$RACE==2)]=1
section2$white[which(section2$RACE==3 | section2$RACE==4 | section2$RACE==5 | section2$RACE==6 | section2$RACE==7)]=0

section2$age=1984-section2$BORNY ###age at enrollment use 1984 as reference

###checking simple statistics
hist(section2$age)
summary(section2$age)

table(section2$BORNY)
section2$CASEID[which(section2$age<0)]  

length(unique(section2$CASEID[which(section2$white==0)]))   ###264
length(unique(section2$CASEID[which(section2$white==1)]))  ###4689

length(unique(section2$CASEID[which(section2$college==0)]))   ###2068
length(unique(section2$CASEID[which(section2$college==1)]))  ###2831
unique(section2$VISIT)

####################################################################################
####################################################################################
####outcome.csv contains events of clinical system and endpoint
outcome=read.csv(paste(DIREC, "outcome.csv", sep=""), header=T)  ###3472 subjects
outcome=outcome[which(outcome$CASEID %in% originalCohort),]
dim(outcome)  ###2624 subjects for the first cohort

table(outcome$DATE1yy)  ###Year of first AIDS diagnosis 
table(outcome$DATE2yy)  ### year of second AIDs diagnosis
outcome[1:50, which(names(outcome) %in% c("CASEID", "DATE1yy", "DATE2yy", "DTHDATEyy", "DEATH", "AIDSCASE","TAIDS", "DCAUSE1"))]


outcome[which(outcome$CASEID == 1050),]
table(outcome$DCAUSE1)
table(outcome$DTHDATEyy)  ###Date of death, year
table(outcome$DTHREPDTyy) ###Date death first reported, year
outcome[1:50, which(names(outcome) %in% c("DTHDATEyy", "DTHREPDTyy"))]

table(outcome$DEATH) 
#1= AIDS-prior dx
#2= AIDS-no prior dx, ###AIDS after death
#3= Not AIDS
#4= Unknown
#Blank= Missing   ###734 in the missing category 

table(outcome$AIDSCASE)
#1= No CDC AIDS
#2= CDC AIDS diagnosis
#3= AIDS by death only

table(outcome$TAIDS)  ##type of AIDS diagnosis
#1= Definite
#2= Presumptive
#3= Probable
#8= Not applicable,no AIDS


#############################################################################
#######HIV status
hivstats=read.csv(paste(DIREC, "hivstats.csv", sep=""), header=T)
names(hivstats)
hivstats=hivstats[which(hivstats$CASEID %in% originalCohort),]
dim(hivstats)  ###4954
table(hivstats$STATUS)
hivstats[1:10, which(names(hivstats) %in% c("V1DATY", "NEGVIS", "POSVIS", "CASEID", "STATUS"))]
#1= Negative status                   
#2= Positive
#3= Positive, based on late update
#4= Converter
#5= Converter without known date of conversion
#6= Prevalent with known date of seroconversion

#####################################################################################
### for each subject find the the first 
#POSVIS, V2DATY-visit and year converted hiv+
unique(hivstats$POSVIS)
#[1]  NA  80 160 300  20 110  90  30 430  50 190 480 180  70  40 150 270 440 140 100 460 390  60 481 400 130 210  71 220 120 310 320 360 340 200
#[36] 250 240 170  21 221 230  41 290 191 530 370 420 410 590 350 599 201  11 380  81 211 330 500 241 510
hivstats$POSVIS[which(hivstats$POSVIS==481)]=480
hivstats$POSVIS[which(hivstats$POSVIS==71)]=70
hivstats$POSVIS[which(hivstats$POSVIS==21)]=20
hivstats$POSVIS[which(hivstats$POSVIS==221)]=220
hivstats$POSVIS[which(hivstats$POSVIS==41)]=40
hivstats$POSVIS[which(hivstats$POSVIS==191)]=190
hivstats$POSVIS[which(hivstats$POSVIS==201)]=200
hivstats$POSVIS[which(hivstats$POSVIS==11)]=10
hivstats$POSVIS[which(hivstats$POSVIS==81)]=80
hivstats$POSVIS[which(hivstats$POSVIS==211)]=210
hivstats$POSVIS[which(hivstats$POSVIS==241)]=240
hivstats$POSVIS[which(hivstats$POSVIS==599)]=590

table(hivstats$POSVIS)

unique(hivstats$V2DATY[which(hivstats$POSVIS==50)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==80)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==110)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==481)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==71)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==21)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==221)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==41)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==191)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==201)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==11)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==81)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==211)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==241)])
unique(hivstats$V2DATY[which(hivstats$POSVIS==599)])


hivstats$visitPos=hivstats$POSVIS   ###year with HIV+
hivstats$visitPos[is.na(hivstats$visitPos) & hivstats$STATUS!=1 & (hivstats$CASEID %in% originalCohort)]=10
table(hivstats$visitPos)
table(hivstats$POSVIS)
table(hivstats$STATUS)
#1    2    4    5 
#2579 1814  522   39 

table(hivstats$V2DATY)  ###year first HIV+
hivstats$yearPos=hivstats$V2DATY   ###year with HIV+
hivstats$yearPos[which(is.na(hivstats$V2DATY) & (hivstats$STATUS!=1) & (hivstats$CASEID %in% originalCohort))]=1984 ###setting the first year hiv+ at first visit
table(hivstats$yearPos)

table(hivstats$V2DATY, hivstats$STATUS)
sum(macsid$CASEID %in% hivstats$CASEID)  ###all the subjects in hivstatus file are in the macsid file 

indices=c(10*(1:30))
for (i in 1:length(indices)) {
  temp=as.numeric(indices[i]>=hivstats$visitPos)
  temp[which(hivstats$STATUS==1)]=0  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("hivVisit", indices[i], sep="")
}
summary(hivstats$visitPos)
summary(hivstats$STATUS)


hivstats=merge(hivstats, outcome[, which(names(outcome) %in% c("CASEID", "DATE1yy", "DATE2yy", "DTHDATEyy", "DEATH", "AIDSCASE","TAIDS", "DCAUSE1"))], by="CASEID", all.x=T)
hivstats=merge(hivstats, section2[, which(names(section2) %in% c("CASEID", "DAT2Y", "age", "college", "white", "RACE", "EDUCA"))], by="CASEID", all.x=T)
dim(hivstats)

table(hivstats$visitPos)
table(hivstats$hivVisit60, hivstats$visitPos)

hivPos60=unique(hivstats$CASEID[which(hivstats$hivVisit60==1)])
length(hivPos60)

hivPos70=unique(hivstats$CASEID[which(hivstats$hivVisit70==1)])
length(hivPos70)


#######################################################################################
#######################################################################################
### obtain whether a subject is taking antiretrovial treatment or not at each visit from section 4 questionnaires
section4=read.csv(paste(DIREC, "section4.csv", sep=""), header=T)
section4=section4[which(section4$CASEID %in% originalCohort),]
#####looking for the last visit for each subject

names(section4)
dim(section4)
length(unique(section4$CASEID))  ###2105 unique subjects
unique(section4$VISIT)


sum(is.na(section4$MAIDS[section4$VISIT==60]))
sum(is.na(section4$MAIDS[section4$VISIT==60]))


sum(is.na(section4$MAIDS[section4$VISIT==70]))
sum(is.na(section4$MAIDS[section4$VISIT==70]))


sum(is.na(section4$MAIDS[section4$VISIT==80]))
sum(is.na(section4$MAIDS[section4$VISIT==80]))


sum(is.na(section4$MAIDS[section4$VISIT==90]))
sum(is.na(section4$MAIDS[section4$VISIT==90]))


uniqueID=unique(section4$CASEID)
lastVisit=numeric(length(uniqueID))

for (i in 1:length(uniqueID)){
  lastVisit[i]=max(section4$VISIT[which(section4$CASEID %in% uniqueID[i])])
}

hist(lastVisit)
summary(lastVisit)

lastVisit=data.frame(lastVisit, CASEID=uniqueID)


############# symptoms #############
################################
indices=NULL
indices=c(10*(1:30))
for (i in 1:length(indices)) {
  print(indices[i])
  
  tempD=section4[which(section4$VISIT==indices[i]),]
  
  soreMouth=as.numeric(tempD$THR2W==2) ###sore mouth for >= 2 weeks
  thrush=as.numeric(tempD$TRS2W==2) ##thrush for >= 2weeks
  weightLoss=as.numeric(tempD$WTLOS==2) ###weight loss more than 10lbs
  shortBreath=as.numeric(tempD$BREAT==2) ##shortness of breath for 2 weeks
  bruise=as.numeric(tempD$BRUIS==2) ###unsual bruises for 2 weeks
  cough=as.numeric(tempD$COUGH==2) 
  diarreha=as.numeric(tempD$DIA2W==2)  ##diarreha
  fatig=as.numeric(tempD$FAT2W==2)  ##fatigue
  fever=as.numeric(tempD$FEV2W==2)  ##fever
  gland=as.numeric(tempD$GLN2W==2)  ###Tender/enlarged glands/
  headache=as.numeric(tempD$HED2W==2) ##headache
  muscle=as.numeric(tempD$MUS2W==2) ##muscle pain
  rash=as.numeric(tempD$RAS2W==2)  ##skin rash
  sweat=as.numeric(tempD$SWT2W==2) ##night swearts
  
  sum(soreMouth, na.rm=T)
  sum(thrush, na.rm=T)
  sum(weightLoss, na.rm=T)
  sum(shortBreath, na.rm=T)
  sum(bruise, na.rm=T)
  sum(cough, na.rm=T)
  sum(diarreha, na.rm=T)
  sum(fatig, na.rm=T)
  sum(fever, na.rm=T)
  sum(gland, na.rm=T)
  sum(headache, na.rm=T)
  sum(muscle, na.rm=T)
  sum(rash, na.rm=T)
  sum(sweat, na.rm=T)
  
  
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(soreMouth==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(soreMouth==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("soreMouth", indices[i], sep="")
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(thrush==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(thrush==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("thrush", indices[i], sep="")
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(weightLoss==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(weightLoss==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("weightLoss", indices[i], sep="")
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(shortBreath==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(shortBreath==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("shortBreath", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(bruise==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(bruise==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("bruise", indices[i], sep="")
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(cough==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(cough==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("cough", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(diarreha==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(diarreha==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("diarreha", indices[i], sep="")
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(fatig==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(fatig==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("fatig", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(fever==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(fever==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("fever", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(gland==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(gland==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("gland", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(headache==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(headache==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("headache", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(muscle==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(muscle==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("muscle", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(rash==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(rash==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("rash", indices[i], sep="")
  
  
  #########
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(sweat==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(sweat==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("sweat", indices[i], sep="")
  
  
  ###any of the symptoms
  
  anySymptom=as.numeric((soreMouth==1) | (thrush==1) | (weightLoss==1) | (shortBreath==1) | (bruise==1) | (cough==1) | (diarreha==1) |
                          (fatig==1) | (fever==1) | (gland==1) | (headache==1) | (muscle==1) | (rash==1) | (sweat==1))
 
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(anySymptom==1)])]=1  ### symptoms
  temp[which(hivstats$CASEID %in% tempD$CASEID[which(anySymptom==0)])]=0  ### no symptoms
  
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("anySymptom", indices[i], sep="")
  
}



###no questions on drugs before visit 6 since there were no drugs available
### drug variable MAIDS,OMAID for visits6-11
#### so I set them 0, treatment indicator at each time point 
indices=NULL
indices=c(10*(1:5))
for (i in 1:length(indices)) {
  temp=rep(0, dim(hivstats)[1])
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("TreatVisit", indices[i], sep="")
}

#############
################################
indices=NULL
indices=c(10*(6:11))
for (i in 1:length(indices)) {
  print(indices[i])
  
  tempD=section4[which(section4$VISIT==indices[i]),]
  
  tempTreat1=unique(tempD$CASEID[which((tempD$M1AID==92 | tempD$M1AID==94) & (tempD$M1ADV!=1))])   ###subjects who took AIDs drugs and who were in visit indices[i] 
  tempTreat2=unique(tempD$CASEID[which((tempD$M2AID==92 | tempD$M2AID==94) & (tempD$M2ADV!=1))])    ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat3=unique(tempD$CASEID[which((tempD$M3AID==92 | tempD$M3AID==94) & (tempD$M3ADV!=1))]) 
  tempTreat4=unique(tempD$CASEID[which((tempD$M4AID==92 | tempD$M4AID==94) & (tempD$M4ADV!=1))]) 
  tempTreat5=unique(tempD$CASEID[which((tempD$M5AID==92 | tempD$M5AID==94) & (tempD$M5ADV!=1))]) 
  
  tempTreat=unique(c(tempTreat1, tempTreat2, tempTreat3, tempTreat4, tempTreat5))
  
  ############################## TREATED BUT WITH OTHER DRUGS################################
  tempNoTreatAll=unique(tempD$CASEID[which(!is.na(tempD$M1AID) | !is.na(tempD$M2AID) | is.na(tempD$M3AID) | is.na(tempD$M4AID) | is.na(tempD$M5AID))]) 
  
  if(sum(tempTreat %in% tempNoTreatAll)!=0){
    tempNoTreatAll=tempNoTreatAll[-which(tempNoTreatAll %in% tempTreat)]  ###excluding the treated 92 or 94 
  }


  ######################################################  
  tempD2=tempD[which(tempD$M1ADV==1 | tempD$M2ADV==1 | tempD$M3ADV==1 | tempD$M4ADV==1 | tempD$M5ADV==1), ###for those who said treated but not since last visit
       which(names(tempD) %in% c("M1ADV", "M2ADV", "M3ADV", "M4ADV","M5ADV", "CASEID"))]
  
  tempNoTreatAll2=NULL
  for (subj in 1:dim(tempD2)[1]){
    tempVal=tempD2[subj, which(names(tempD2) %in% c("M1ADV", "M2ADV", "M3ADV", "M4ADV","M5ADV"))]
    tempVal=tempVal[which(!is.na(tempVal))]
    if(sum(tempVal==1) == length(tempVal)){
      tempNoTreatAll2=c(tempNoTreatAll2, tempD2$CASEID[subj])
    }
  }
  
  tempNoTreatAll3=unique(tempD$CASEID[which(tempD$MAIDS==1)])   ###subjects who said no
  tempNoTreat=unique(c(tempNoTreatAll, tempNoTreatAll2, tempNoTreatAll3))
  
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempTreat)]=1  ### receive treatment
  temp[which(hivstats$CASEID %in% tempNoTreat)]=0  ### did not receive treatment but HIV+
    
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("TreatVisit", indices[i], sep="")

  tempTreat1=NULL
  tempTreat2=NULL
  tempTreat3=NULL
  tempTreat4=NULL
  
  tempNoTreat1=NULL
  tempNoTreat2=NULL
  tempNoTreat3=NULL
  tempNoTreat4=NULL
  
  tempD=NULL
  tempD2=NULL
  

}


table(section4$M1AID[which(section4$VISIT==60)])
table(section4$M2AID[which(section4$VISIT==60)])

table(section4$M1AID[which(section4$VISIT==60)])
table(section4$M2AID[which(section4$VISIT==60)])

table(section4$M1AID[which(section4$VISIT==60)])
table(section4$M2AID[which(section4$VISIT==60)])


table(section4$M1AID[which(section4$VISIT==60)])
table(section4$M2AID[which(section4$VISIT==60)])


table(section4$M1ADV, section4$VISIT) ####have taken drugs to help with AIDS since last visit
table(section4$M1AID, section4$VISIT)  ###092=AZT, what drugs

table(section4$M2ADV, section4$VISIT)  
table(section4$M2AID, section4$VISIT)

table(section4$M3ADV, section4$VISIT)
table(section4$M3AID, section4$VISIT)

table(section4$M4ADV, section4$VISIT)
table(section4$M4AID, section4$VISIT)

table(section4$M5ADV, section4$VISIT)
table(section4$M5AID, section4$VISIT)

table(section4$M1AID==92, section4$M1ADS)

#####what about other drugs not on the list
table(section4$O1ADV, section4$VISIT)
table(section4$O1AID, section4$VISIT)  ###I don't know what the codes mean

table(section4$M1ADS, section4$VISIT)  ###take drugs in a research study, 1=no, 2=yes
table(section4$M2ADS, section4$VISIT)  ###take drugs in a research study
table(section4$M3ADS, section4$VISIT)  ###take drugs in a research study
table(section4$M4ADS, section4$VISIT)  ###take drugs in a research study
table(section4$M5ADS, section4$VISIT)  ###take drugs in a research study

table(section4$MAIDS, section4$VISIT)#####subjects who took AIDs drugs on list 1
table(section4$OMAID, section4$VISIT)#####subjects who took other drugs not on list 1

table(hivstats$TreatVisit110[hivstats$hivVisit110==1])
table(hivstats$TreatVisit60[hivstats$hivVisit60==1])
table(hivstats$TreatVisit70[hivstats$hivVisit70==1])
table(hivstats$TreatVisit80[hivstats$hivVisit80==1])
table(hivstats$TreatVisit90[hivstats$hivVisit90==1])
table(hivstats$TreatVisit100[hivstats$hivVisit100==1])
table(hivstats$TreatVisit110[hivstats$hivVisit110==1])



#write.csv(section4[which((section4$CASEID %in% hivPos60) & (section4$VISIT==60)), which(names(section4) %in% c("CASEID", "MAIDS", "OMAID", "M1ADV", "M1AID", "M1ADS", "M2ADV", "M2AID", "M2ADS", 
#                                                  "M3ADV", "M3AID", "M3ADS", "M4ADV", "M4AID", "M4ADS", "M5ADV", "M5AID", "M5ADS", "VISIT"))], 
#          paste(DIRECOUT, "drug_Pos6.csv", sep=""), row.names=F)


table(section4$PLCT1) ###controlled placebo
table(section4$CTDR1) ##drugname
table(section4$CTDR2) ##drugname
table(section4$C1STY)##year stopped taking drug
table(section4$CT1YR)  ### start date for drug 1
table(section4$C1NWY)  ##restarted taking drug1
table(section4$CLTRL)  ###involved in trial 
table(section4$DRGNS)  ###involved in nonresearch drug therapy
table(section4$PLCT1,section4$CTDR1)

############################################################################
#### for visit 120
indices=NULL
indices=c(10*12)
for (i in 1:length(indices)) {
  print(indices[i])
  
  tempD=section4[which(section4$VISIT==indices[i]),]
  
  tempTreat1=unique(tempD$CASEID[which(tempD$D1NAM==92 & ((tempD$D1YER <= tempD$DAT4Y) | (tempD$RSD1Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat2=unique(tempD$CASEID[which(tempD$D2NAM==92 & ((tempD$D2YER <= tempD$DAT4Y) | (tempD$RSD2Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat3=unique(tempD$CASEID[which(tempD$D3NAM==92 & ((tempD$D3YER <= tempD$DAT4Y) | (tempD$RSD3Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat4=unique(tempD$CASEID[which(tempD$D4NAM==92 & ((tempD$D4YER <= tempD$DAT4Y) | (tempD$RSD4Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  ###########
  tempTreat5=unique(tempD$CASEID[which(tempD$D1NAM==94 & ((tempD$D1YER <= tempD$DAT4Y) | (tempD$RSD1Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat6=unique(tempD$CASEID[which(tempD$D2NAM==94 & ((tempD$D2YER <= tempD$DAT4Y) | (tempD$RSD2Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat7=unique(tempD$CASEID[which(tempD$D3NAM==94 & ((tempD$D3YER <= tempD$DAT4Y) | (tempD$RSD3Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat8=unique(tempD$CASEID[which(tempD$D4NAM==94 & ((tempD$D4YER <= tempD$DAT4Y) | (tempD$RSD4Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  
  ###########
  tempTreat9=unique(tempD$CASEID[which(tempD$D1NAM==147 & ((tempD$D1YER <= tempD$DAT4Y) | (tempD$RSD1Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat10=unique(tempD$CASEID[which(tempD$D2NAM==147 & ((tempD$D2YER <= tempD$DAT4Y) | (tempD$RSD2Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat11=unique(tempD$CASEID[which(tempD$D3NAM==147 & ((tempD$D3YER <= tempD$DAT4Y) | (tempD$RSD3Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat12=unique(tempD$CASEID[which(tempD$D4NAM==147 & ((tempD$D4YER <= tempD$DAT4Y) | (tempD$RSD4Y<= tempD$DAT4Y)))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  #########
  tempTreat1b=unique(tempD$CASEID[which((tempD$CTDR1==92) & (((tempD$CT1YR <= tempD$DAT4Y) | (tempD$C1NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat2b=unique(tempD$CASEID[which((tempD$CTDR2==92) & (((tempD$CT2YR <= tempD$DAT4Y) | (tempD$C2NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat3b=unique(tempD$CASEID[which((tempD$CTDR3==92) & (((tempD$CT3YR <= tempD$DAT4Y) | (tempD$C3NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  #########
  tempTreat4b=unique(tempD$CASEID[which((tempD$CTDR1==94) & (((tempD$CT1YR <= tempD$DAT4Y) | (tempD$C1NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat5b=unique(tempD$CASEID[which((tempD$CTDR2==94) & (((tempD$CT2YR <= tempD$DAT4Y) | (tempD$C2NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat6b=unique(tempD$CASEID[which((tempD$CTDR3==94) & (((tempD$CT3YR <= tempD$DAT4Y) | (tempD$C3NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  
  #########
  tempTreat7b=unique(tempD$CASEID[which((tempD$CTDR1==147) & (((tempD$CT1YR <= tempD$DAT4Y) | (tempD$C1NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat8b=unique(tempD$CASEID[which((tempD$CTDR2==147) & (((tempD$CT2YR <= tempD$DAT4Y) | (tempD$C2NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempTreat9b=unique(tempD$CASEID[which((tempD$CTDR3==147) & (((tempD$CT3YR <= tempD$DAT4Y) | (tempD$C3NWY<= tempD$DAT4Y))))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  
  tempTreat=unique(tempTreat1, tempTreat2, tempTreat3, tempTreat4, tempTreat5, tempTreat6, tempTreat7, tempTreat8, tempTreat9,
                   tempTreat10, tempTreat11, tempTreat11, tempTreat12, 
                   tempTreat1b, tempTreat2b, tempTreat3b, tempTreat4b, tempTreat5b, tempTreat6b, tempTreat7b, tempTreat8b, tempTreat9b)
  
  
  ################################################################################
  #################################################################################
  
  tempNoTreat1=unique(tempD$CASEID[which(tempD$D1NAM!=92 & tempD$D1NAM!=94 & tempD$D1NAM!=147)])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempNoTreat2=unique(tempD$CASEID[which(tempD$D2NAM!=92 & tempD$D2NAM!=94 & tempD$D2NAM!=147)])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempNoTreat3=unique(tempD$CASEID[which(tempD$D3NAM!=92 & tempD$D3NAM!=94 & tempD$D3NAM!=147)])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempNoTreat4=unique(tempD$CASEID[which(tempD$D4NAM!=92 & tempD$D4NAM!=94 & tempD$D4NAM!=147)])   ###subjects who took AIDs drugs and who were in visit indices[i]
  

  tempNoTreat1b=unique(tempD$CASEID[which((tempD$CTDR1!=92) & (tempD$CTDR1!=94) & (tempD$CTDR1!=147))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempNoTreat2b=unique(tempD$CASEID[which((tempD$CTDR2!=92) & (tempD$CTDR2!=94) & (tempD$CTDR2!=147))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  tempNoTreat3b=unique(tempD$CASEID[which((tempD$CTDR3!=92) & (tempD$CTDR3!=94) & (tempD$CTDR3!=147))])   ###subjects who took AIDs drugs and who were in visit indices[i]

  
  tempNoTreatAll=unique(tempNoTreat1, tempNoTreat2, tempNoTreat3, tempNoTreat4, 
                        tempNoTreat1b, tempNoTreat2b, tempNoTreat3b)
  
  

  tempNoTreatAll2=unique(tempD$CASEID[which((tempD$DRGNS==1) & (tempD$CLTRL==1))])   ###subjects who answered no 
  
  tempNoTreat=unique(c(tempNoTreatAll, tempNoTreatAll2))
  
  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempTreat)]=1  ### receive treatment
  temp[which(hivstats$CASEID %in% tempNoTreat)]=0  ### did not receive treatment but HIV+
    
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("TreatVisit", indices[i], sep="")

  tempTreat1=NULL
  tempTreat2=NULL
  tempTreat3=NULL
  tempTreat4=NULL
  
}

table(section4$D1NAM)
table(section4$D2NAM)
table(section4$D3NAM)
table(section4$D4NAM)

table(hivstats$TreatVisit120[hivstats$hivVisit120==1])






####checking the drug patterns of those who took drugs
#write.csv(section4[which(section4$VISIT==120), 
#                   which(names(section4) %in% c("CASEID","CLTRL","PLCT1","CTDR1","C1STY","CT1YR","C1NWY","DRGNS",
#                                                "D1YER", "STD1Y", "RSD1Y", "STD2Y","STD3Y","STD4Y", "AID17","AID27", "AID37", "AID47", "D1NAM",
#                                                "D2NAM", "D3NAM", "D4NAM",   "D2YER", "D3YER", "D4YER",
#                                                "VISIT", "DAT4Y"))], 
#          paste(DIRECOUT, "drug_visit12.csv", sep=""), row.names=F)

#STD1Y-stopped taking drug in year
#section4$D2NAM-drug names
#D2YER drug start year
#DRGNS in non-research aids drug yes/no
#147=Dideoxyinosine (ddI)
#094=2', 3' -dideoxycytidine (ddC)
#092=AZT(Azidothymidi-ne, Compound S,Retrovir,Zidovudine, ZDV)
#180=AZT/ddI trial, didnot include this treatment



drug=read.csv(paste(DIREC, "drugf1.csv", sep=""), header=T)
dim(drug)
names(drug)

table(drug$DRGAV, drug$VISIT)
table(drug$DRGAV[drug$VISIT==130])
table(drug$DRGAV[drug$VISIT==140])
table(drug$DRGAV[drug$VISIT==150])
table(drug$DRGAV[drug$VISIT==160])
table(drug$DRGAV[drug$VISIT==170])
table(drug$DRGAV[drug$VISIT==180])
table(drug$DRGAV[drug$VISIT==190])
table(drug$DRGAV[drug$VISIT==200])
table(drug$DRGAV[drug$VISIT==210])
table(drug$DRGAV[drug$VISIT==220])
table(drug$DRGAV[drug$VISIT==230])
table(drug$DRGAV[drug$VISIT==610])

###############################################################################
indices=NULL
indices=c(10*c(13:30))
for (i in 1:length(indices)) {
  
  print(indices[i])
  
  ################## patients taking that specific drugs ################
  tempD=drug[which(drug$VISIT==indices[i]),]
  table(tempD$DRGAV)
  
  
  tempTreat=unique(c(tempD$CASEID[which((tempD$DRGAV==92) | (tempD$DRGAV==94) | (tempD$DRGAV==147) | (tempD$DRGAV==159) | (tempD$DRGAV==180)
                                      | (tempD$DRGAV==185) | (tempD$DRGAV==186) | (tempD$DRGAV==187) | (tempD$DRGAV==191)  | (tempD$DRGAV==193) 
                                      | (tempD$DRGAV==194) | (tempD$DRGAV==201) | (tempD$DRGAV==204)
                                      | (tempD$DRGAV==205) | (tempD$DRGAV==206) | (tempD$DRGAV==208) | (tempD$DRGAV==209)
                                      | (tempD$DRGAV==210) | (tempD$DRGAV==211) | (tempD$DRGAV==212) | (tempD$DRGAV==214)
                                      | (tempD$DRGAV==216) | (tempD$DRGAV==217) | (tempD$DRGAV==218) | (tempD$DRGAV==219)
                                      | (tempD$DRGAV==220) | (tempD$DRGAV==221) | (tempD$DRGAV==227) | (tempD$DRGAV==233)
                                      | (tempD$DRGAV==238) | (tempD$DRGAV==239))]))   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  
  tempNoTreat=unique(tempD$CASEID[which(!is.na(tempD$DRGAV))])   ###subjects who took AIDs drugs and who were in visit indices[i]
  
  if(sum(tempTreat %in% tempNoTreat) != 0){
    tempNoTreat = tempNoTreat[-which(tempNoTreat %in% tempTreat)]
  }
  
  
  #tempD[which(tempD$CASEID==1011), which(names(tempD) == "DRGAV")]
  #tempD[, which(names(tempD)) == c("CASEID", "DRGAV")]
  
  ##################
  tempD2=section4[which(section4$VISIT==indices[i]),]
  tempNoTreat2=unique(tempD2$CASEID[which((tempD2$ML1AD==1)  | (tempD2$MAIDS==1))])   ###subjects who didn't fill the question
  tempNoTreat=unique(c(tempNoTreat, tempNoTreat2))
  
  #####if they have filled in drug info assumed they have taken drugs although they said no in section4

  temp=rep(NA, dim(hivstats)[1]) ###NA for HIV- for NA for not answering the questionnaire
  temp[which(hivstats$CASEID %in% tempTreat)]=1  ### receive treatment
  temp[which(hivstats$CASEID %in% tempNoTreat)]=0  ### did not receive treatment but HIV+
    
  hivstats=data.frame(hivstats, temp)
  names(hivstats)[dim(hivstats)[2]]=paste("TreatVisit", indices[i], sep="")

  tempTreat1=NULL
  tempTreat2=NULL
  tempTreat3=NULL
  tempTreat4=NULL
  
}

table(section4$ML1A1, section4$VISIT)
table(section4$ML1A2, section4$VISIT)
table(section4$ML1A3, section4$VISIT)
table(section4$ML1A4, section4$VISIT)
table(section4$ML1A5, section4$VISIT)
table(section4$ML1A6, section4$VISIT)
table(section4$ML1A7, section4$VISIT)
table(section4$ML1A8, section4$VISIT)
table(section4$ML1A9, section4$VISIT)
table(section4$ML110, section4$VISIT)
table(section4$ML111, section4$VISIT)
table(section4$ML112, section4$VISIT)


table(hivstats$TreatVisit130[hivstats$hivVisit130==1])
table(hivstats$TreatVisit140[hivstats$hivVisit140==1])
table(hivstats$TreatVisit150[hivstats$hivVisit150==1])
table(hivstats$TreatVisit160[hivstats$hivVisit160==1])
table(hivstats$TreatVisit170[hivstats$hivVisit170==1])


table(section4$ML1AD, section4$VISIT)
table(section4$ML2AD, section4$VISIT)
table(section4$ML1AD, section4$VISIT)

############################################################################
#######LAB TEST RESULTS#####################################################
#### obtain blood count measures from lab_rslt.csv
labTest=read.csv(paste(DIREC, "lab_rslt.csv", sep=""), header=T)
#labTest=labTest[which(labTest$CASEID %in% hivPos60),]
names(labTest)
dim(labTest)


summary(labTest$LEU3N) ###CD4 counts
labTest$WBC  ### white blood cell counts
labTest$RBC   ###red blood cell counts
labTest$PLATE   ###platelets
labTest$LEU2N ###CD8 counts
labTest$LEU3N ###CD4 counts
labTest$VLOAD



labTest=labTest[,which(names(labTest) %in% c("CASEID","LDATY", "VISIT", "WBC", "RBC", "PLATE", 
                                             "LEU3N", "LEU2N","VLOAD", "GLUC2", "HB"))]
labTest_wide=reshape(labTest, timevar="VISIT", idvar=c("CASEID"), direction="wide")  ### reshape the long data to wide data set


#write.csv(labTest_wide[, which(names(labTest_wide) %in% c("CASEID",  "LEU3N.50", "LEU3N.60",  "LEU3N.70",  "LEU3N.80",  "LEU3N.90",  "LEU3N.100",  "LEU3N.110"))], 
#          paste(DIRECOUT, "labTestTemp.csv", sep=""), row.names=F)


hivstats=merge(hivstats, labTest_wide, by="CASEID", all.x=T)
dim(hivstats)

hivstats=merge(hivstats, lastVisit, by="CASEID", all.x=T)
dim(hivstats)
names(hivstats)

###get the year of visit from section4 questionnaire
dataSec4=section4[,which(names(section4) %in% c("CASEID","DAT4Y", "VISIT", "MAIDS"))]
dataSec4_wide=reshape(dataSec4, timevar="VISIT", idvar=c("CASEID"), direction="wide")  ### reshape the long data to wide data set
dataSec4_wide[1:10,]
sum(is.na(dataSec4_wide$MAIDS.70))


hivstats=merge(hivstats, dataSec4_wide, by="CASEID", all.x=T)
dim(hivstats)
names(hivstats)



###################################################################
########getting only the HIV positive participants only ##########
hivstatsPos=hivstats[-c(which(hivstats$STATUS==1)),]
hivstatsPos=hivstatsPos[-c(which(hivstatsPos$lastVisit <= 60)),]  ###excluding subjects who's last visit is before AZT came out
dim(hivstatsPos)  ###2375
table(hivstatsPos$hivVisit10)
#0    1 
#523 1852 
table(hivstatsPos$lastVisit)
unique(hivstatsPos$visitPos)  ###first HIV+ visits in the data
unique(hivstatsPos$yearPos)  ### first HIV+ year in the data


table(hivstatsPos$visitPos[hivstatsPos$TreatVisit60==1])
table(hivstatsPos$yearPos[hivstatsPos$TreatVisit60==1])   ### the subjects with 

sum(is.na(hivstatsPos$MAIDS.60[hivstatsPos$hivVisit60==1]))
sum(is.na(hivstatsPos$MAIDS.60[hivstatsPos$hivVisit60==1]))

table(hivstatsPos$TreatVisit70)
table(hivstatsPos$visitPos)



####################################################
####use last observation carried forward assumption to fill in missing treatment 
treatPattern=matrix(NA, nrow=dim(hivstatsPos)[1], ncol=30)  ###starting with visit 6 when ART became available

index=NULL
indexOut=NULL
for (i in 1:30) {
  indexOut=c(indexOut, which(names(hivstatsPos)==paste("TreatVisit", 10*i, sep="")))
}

for (i in 1:dim(hivstatsPos)[1]) {
  print(i)
  
  tempVal=as.numeric(hivstatsPos[i,indexOut])
  missingIndex=which(is.na(tempVal))
  notMissingIndex=which(!is.na(tempVal))  
  
  if (length(missingIndex)>=1 & length(notMissingIndex)!=0) {
    for (boot in 1:length(missingIndex)) {
      selectIndex=notMissingIndex[which(notMissingIndex <= missingIndex[boot])]
      tempVal[missingIndex[boot]]=tempVal[selectIndex[length(selectIndex)]]
    } 
  } 
  
  treatPattern[i,]=tempVal
  tempVal=NULL
} 

treatPattern=data.frame(treatPattern)
names(treatPattern)=paste("treatFill", 1:dim(treatPattern)[2], sep="")

hivstatsPos=data.frame(hivstatsPos, treatPattern)


############################################################################
####dosage as time varying covariate
#### doage = total number of visits that a subject answered yes to taking antiretrovival treatments since becoming HIV+
dosageFill=matrix(NA, nrow=dim(hivstatsPos)[1], ncol=30)  ###including before first HIV+ visit

index=NULL
indexOut=NULL
for (i in 1:30) {
  indexOut=c(indexOut, which(names(hivstatsPos)==paste("treatFill", i, sep="")))
}

for (i in 1:dim(hivstatsPos)[1]) {
  print(i)
  tempVal=as.numeric(hivstatsPos[i,indexOut])
  for (j in 1:length(tempVal)) {
    dosageFill[i,j]=sum(tempVal[1:j])
  }
  
}

dosageFill=data.frame(dosageFill)
names(dosageFill)=paste("dosageFill", 1:dim(dosageFill)[2], sep="")
hivstatsPos=data.frame(hivstatsPos, dosageFill)



########## the final dataset that we used in the analysis 
write.csv(hivstatsPos,paste(DIRECOUT, "hivPos_All_ApprovedDrug.csv", sep=""), row.names=F)




gnames=c("CASEID", "STATUS", "visitPos", "yearPos", paste("TreatVisit", (1:11)*10, sep=""),"hivTreat",
         "startTreat", "restartTreat","restart" ,  paste("TreatVisit", (12:30)*10, sep=""), "DEATH","DTHDATEyy","lastVisit",
         paste("LEU3N.", (1:30)*10, sep=""), paste("LEU2N.", (1:30)*10, sep=""), paste("RBC.", (1:30)*10, sep=""), 
         paste("WBC.", (1:30)*10, sep=""), paste("PLATE.", (1:30)*10, sep=""), paste("VLOAD.", (1:30)*10, sep=""), 
         paste("LDATY.", (1:30)*10, sep=""),
         "age", "white", "college")

indNum=NULL

for (gnum in 1:length(gnames)){
  indNum=c(indNum, which(names(hivstats) == gnames[gnum]))
}


### same as before, just order the variables in some specific order
write.csv(hivstatsPos[,indNum],paste(DIRECOUT, "data.csv", sep=""), row.names=F)



###checking some summary statistics
sum(hivstatsPos$TreatVisit60==1 & hivstatsPos$TreatVisit70==1, na.rm=T)
sum(hivstatsPos$TreatVisit60==1 & hivstatsPos$TreatVisit70==0, na.rm=T)
sum(hivstatsPos$TreatVisit60==0 & hivstatsPos$TreatVisit70==1, na.rm=T)
sum(hivstatsPos$TreatVisit60==0 & hivstatsPos$TreatVisit70==0, na.rm=T)


sum(hivstatsPos$TreatVisit70==1 & hivstatsPos$TreatVisit80==1, na.rm=T)
sum(hivstatsPos$TreatVisit70==1 & hivstatsPos$TreatVisit80==0, na.rm=T)
sum(hivstatsPos$TreatVisit70==0 & hivstatsPos$TreatVisit80==1, na.rm=T)
sum(hivstatsPos$TreatVisit70==0 & hivstatsPos$TreatVisit80==0, na.rm=T)



sum(hivstatsPos$TreatVisit80==1 & hivstatsPos$TreatVisit90==1, na.rm=T)
sum(hivstatsPos$TreatVisit80==1 & hivstatsPos$TreatVisit90==0, na.rm=T)
sum(hivstatsPos$TreatVisit80==0 & hivstatsPos$TreatVisit90==1, na.rm=T)
sum(hivstatsPos$TreatVisit80==0 & hivstatsPos$TreatVisit90==0, na.rm=T)


sum(hivstatsPos$TreatVisit90==1 & hivstatsPos$TreatVisit100==1, na.rm=T)
sum(hivstatsPos$TreatVisit90==1 & hivstatsPos$TreatVisit100==0, na.rm=T)
sum(hivstatsPos$TreatVisit90==0 & hivstatsPos$TreatVisit100==1, na.rm=T)
sum(hivstatsPos$TreatVisit90==0 & hivstatsPos$TreatVisit100==0, na.rm=T)


sum(hivstatsPos$TreatVisit100==1 & hivstatsPos$TreatVisit110==1, na.rm=T)
sum(hivstatsPos$TreatVisit100==1 & hivstatsPos$TreatVisit110==0, na.rm=T)
sum(hivstatsPos$TreatVisit100==0 & hivstatsPos$TreatVisit110==1, na.rm=T)
sum(hivstatsPos$TreatVisit100==0 & hivstatsPos$TreatVisit110==0, na.rm=T)


sum(hivstatsPos$TreatVisit110==1 & hivstatsPos$TreatVisit120==1, na.rm=T)
sum(hivstatsPos$TreatVisit110==1 & hivstatsPos$TreatVisit120==0, na.rm=T)
sum(hivstatsPos$TreatVisit110==0 & hivstatsPos$TreatVisit120==1, na.rm=T)
sum(hivstatsPos$TreatVisit110==0 & hivstatsPos$TreatVisit120==0, na.rm=T)


sum(hivstatsPos$TreatVisit120==1 & hivstatsPos$TreatVisit130==1, na.rm=T)
sum(hivstatsPos$TreatVisit120==1 & hivstatsPos$TreatVisit130==0, na.rm=T)
sum(hivstatsPos$TreatVisit120==0 & hivstatsPos$TreatVisit130==1, na.rm=T)
sum(hivstatsPos$TreatVisit120==0 & hivstatsPos$TreatVisit130==0, na.rm=T)



sum(hivstatsPos$TreatVisit130==1 & hivstatsPos$TreatVisit140==1, na.rm=T)
sum(hivstatsPos$TreatVisit130==1 & hivstatsPos$TreatVisit140==0, na.rm=T)
sum(hivstatsPos$TreatVisit130==0 & hivstatsPos$TreatVisit140==1, na.rm=T)
sum(hivstatsPos$TreatVisit130==0 & hivstatsPos$TreatVisit140==0, na.rm=T)




sum(hivstatsPos$TreatVisit140==1 & hivstatsPos$TreatVisit150==1, na.rm=T)
sum(hivstatsPos$TreatVisit140==1 & hivstatsPos$TreatVisit150==0, na.rm=T)
sum(hivstatsPos$TreatVisit140==0 & hivstatsPos$TreatVisit150==1, na.rm=T)
sum(hivstatsPos$TreatVisit140==0 & hivstatsPos$TreatVisit150==0, na.rm=T)



sum(hivstatsPos$TreatVisit150==1 & hivstatsPos$TreatVisit160==1, na.rm=T)
sum(hivstatsPos$TreatVisit150==1 & hivstatsPos$TreatVisit160==0, na.rm=T)
sum(hivstatsPos$TreatVisit150==0 & hivstatsPos$TreatVisit160==1, na.rm=T)
sum(hivstatsPos$TreatVisit150==0 & hivstatsPos$TreatVisit160==0, na.rm=T)


sum(hivstatsPos$TreatVisit130==1 & hivstatsPos$TreatVisit140==1 & hivstatsPos$TreatVisit150==1, na.rm=T)
sum(hivstatsPos$TreatVisit130==0 & hivstatsPos$TreatVisit140==1 & hivstatsPos$TreatVisit150==1, na.rm=T)
sum(hivstatsPos$TreatVisit130==0 & hivstatsPos$TreatVisit140==0 & hivstatsPos$TreatVisit150==1, na.rm=T)
sum(hivstatsPos$TreatVisit130==0 & hivstatsPos$TreatVisit140==0 & hivstatsPos$TreatVisit150==0, na.rm=T)


