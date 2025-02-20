

all_simdatFit <- list()
DIREC="Application/"  ###where dataset is


###import the dataset
simdat2=read.csv(paste(DIREC, "dataset/data.csv", sep=""), header=T)
start= 7
end= 21
# Iterate over each d value, generate the corresponding simdatFit and add a Visit column
for (d in start:end) {
  print(d)
  set.seed(d)
  
  # Construct the simdat data frame, which contains the three follow-up window data corresponding to the current d
  simdat <- data.frame(CASEID = simdat2$CASEID)
  
  # Extract treatment status, CD4/CD8 counts and other biomarkers
  simdat$A1 <- simdat2[, which(names(simdat2) == paste("TreatVisit", (d+1)*10, sep=""))]
  ###CD4 count, LEU3N1 at first time point, LEU3N2 at second time point, LEU3N3-final outcome of interest after the second treatment A2
  simdat$LEU3N1 <- simdat2[, which(names(simdat2) == paste("LEU3N.", (d)*10, sep=""))]
  simdat$LEU3N2 <- simdat2[, which(names(simdat2) == paste("LEU3N.", (d+1)*10, sep=""))]
  ##CD8 counts
  simdat$LEU2N1 <- simdat2[, which(names(simdat2) == paste("LEU2N.", (d)*10, sep=""))]
  simdat$LEU2N2 <- simdat2[, which(names(simdat2) == paste("LEU2N.", (d+1)*10, sep=""))]
  ###white blood cell counts
  simdat$WBC1 <- simdat2[, which(names(simdat2) == paste("WBC.", (d)*10, sep=""))]
  ###Red blood cell counts
  simdat$RBC1 <- simdat2[, which(names(simdat2) == paste("RBC.", (d)*10, sep=""))]
  ## hemoglobin
  simdat$HB1 <- simdat2[, which(names(simdat2) == paste("HB.", (d)*10, sep=""))]
  ###platelet counts
  simdat$PLATE1 <- simdat2[, which(names(simdat2) == paste("PLATE.", (d)*10, sep=""))]
  ##viral load
  simdat$VLOAD1 <- simdat2[, which(names(simdat2) == paste("VLOAD.", (d)*10, sep=""))]
  ## GLUCOSE
  simdat$GLUCOSE1 <- simdat2[, which(names(simdat2) == paste("GLUC2.", (d)*10, sep=""))]
  
  simdat$age <- simdat2$age
  simdat$white <- simdat2$white
  simdat$college <- simdat2$college
  simdat$deathYear <- simdat2$DTHDATEyy
  simdat$dosage <- simdat2[, which(names(simdat2) == paste("dosageFill", d, sep=""))]
  simdat$hiv1 <- simdat2[, which(names(simdat2) == paste("hivVisit", (d)*10, sep=""))]
  
  # Filter the data, remove the missing value rows, and generate the simdatFit of the current d
  simdatFit <- simdat[which(!is.na(simdat$LEU3N1) & !is.na(simdat$LEU2N1) & !is.na(simdat$WBC1) & 
                              !is.na(simdat$RBC1) & !is.na(simdat$PLATE1) & 
                              !is.na(simdat$LEU3N2) &    
                              !is.na(simdat$A1)  & !is.na(simdat$dosage)), ]
  
  # Add a column Visit to record the current d value
  simdatFit$Visit <- d
  
  # Add the current d's simdatFit to the all_simdatFit list
  all_simdatFit[[paste0("d_", d)]] <- simdatFit
}

# Combine all d's simdatFit into one data frame
final_simdatFit <- do.call(rbind, all_simdatFit)
final_simdatFit$LEU3N1=sqrt(final_simdatFit$LEU3N1)
final_simdatFit$LEU2N1=sqrt(final_simdatFit$LEU2N1) 
final_simdatFit$WBC1=sqrt(final_simdatFit$WBC1)  
final_simdatFit$RBC1=sqrt(final_simdatFit$RBC1)
final_simdatFit$PLATE1=sqrt(final_simdatFit$PLATE1)

final_simdatFit$LEU3N2=sqrt(final_simdatFit$LEU3N2)
final_simdatFit$LEU2N2=sqrt(final_simdatFit$LEU2N2)   

head(final_simdatFit)

#### save final_simdatFit
write.csv(final_simdatFit, paste(DIREC, "dataset/final_simdatFit.csv", sep=""), row.names=FALSE)
