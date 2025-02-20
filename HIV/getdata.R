library('CPMA' )

#out <- CPlane(data.tr=CPdata , data.te=NULL, yind = 1, xind = 2:6, zind = 2:6,
              #ini.theta = c(sqrt(0.5),-sqrt(0.5), 0, 0, 0))

# 初始化一个空列表，用于存储不同 d 值的 simdatFit 数据
all_simdatFit <- list()
DIREC="Application/"  ###where dataset is


###import the dataset
simdat2=read.csv(paste(DIREC, "dataset/data.csv", sep=""), header=T)
start= 7
end= 21
# 遍历每个 d 值，生成对应的 simdatFit 并添加 Visit 列
for (d in start:end) {
  print(d)
  set.seed(d)
  
  # 构建 simdat 数据框，包含当前 d 对应的3次随访窗口数据
  simdat <- data.frame(CASEID = simdat2$CASEID)
  
  # 提取治疗状态、CD4/CD8计数和其他生物标志物
  simdat$A1 <- simdat2[, which(names(simdat2) == paste("TreatVisit", (d+1)*10, sep=""))]
  simdat$A2 <- simdat2[, which(names(simdat2) == paste("TreatVisit", (d+2)*10, sep=""))]
  ###CD4 count, LEU3N1 at first time point, LEU3N2 at second time point, LEU3N3-final outcome of interest after the second treatment A2
  simdat$LEU3N1 <- simdat2[, which(names(simdat2) == paste("LEU3N.", (d)*10, sep=""))]
  simdat$LEU3N2 <- simdat2[, which(names(simdat2) == paste("LEU3N.", (d+1)*10, sep=""))]
  simdat$LEU3N3 <- simdat2[, which(names(simdat2) == paste("LEU3N.", (d+2)*10, sep=""))]
  ##CD8 counts
  simdat$LEU2N1 <- simdat2[, which(names(simdat2) == paste("LEU2N.", (d)*10, sep=""))]
  simdat$LEU2N2 <- simdat2[, which(names(simdat2) == paste("LEU2N.", (d+1)*10, sep=""))]
  simdat$LEU2N3 <- simdat2[, which(names(simdat2) == paste("LEU2N.", (d+2)*10, sep=""))]
  ###white blood cell counts
  simdat$WBC1 <- simdat2[, which(names(simdat2) == paste("WBC.", (d)*10, sep=""))]
  simdat$WBC2 <- simdat2[, which(names(simdat2) == paste("WBC.", (d+1)*10, sep=""))]
  simdat$WBC3 <- simdat2[, which(names(simdat2) == paste("WBC.", (d+2)*10, sep=""))]
  ###Red blood cell counts
  simdat$RBC1 <- simdat2[, which(names(simdat2) == paste("RBC.", (d)*10, sep=""))]
  simdat$RBC2 <- simdat2[, which(names(simdat2) == paste("RBC.", (d+1)*10, sep=""))]
  simdat$RBC3 <- simdat2[, which(names(simdat2) == paste("RBC.", (d+2)*10, sep=""))]
  ###platelet counts
  simdat$PLATE1 <- simdat2[, which(names(simdat2) == paste("PLATE.", (d)*10, sep=""))]
  simdat$PLATE2 <- simdat2[, which(names(simdat2) == paste("PLATE.", (d+1)*10, sep=""))]
  simdat$PLATE3 <- simdat2[, which(names(simdat2) == paste("PLATE.", (d+2)*10, sep=""))]
  ##viral load
  simdat$VLOAD1 <- simdat2[, which(names(simdat2) == paste("VLOAD.", (d)*10, sep=""))]
  simdat$VLOAD2 <- simdat2[, which(names(simdat2) == paste("VLOAD.", (d+1)*10, sep=""))]
  simdat$VLOAD3 <- simdat2[, which(names(simdat2) == paste("VLOAD.", (d+2)*10, sep=""))]
  simdat$age <- simdat2$age
  simdat$white <- simdat2$white
  simdat$college <- simdat2$college
  simdat$deathYear <- simdat2$DTHDATEyy
  simdat$dosage <- simdat2[, which(names(simdat2) == paste("dosageFill", d, sep=""))]
  simdat$hiv1 <- simdat2[, which(names(simdat2) == paste("hivVisit", (d)*10, sep=""))]
  ## hemoglobin
  simdat$HB1 <- simdat2[, which(names(simdat2) == paste("HB.", (d)*10, sep=""))]
  simdat$dosage <- simdat2[, which(names(simdat2) == paste("dosageFill", d, sep=""))]
  # 筛选数据，去除缺失值行，生成当前 d 的 simdatFit
  simdatFit <- simdat[which(!is.na(simdat$LEU3N1) & !is.na(simdat$LEU2N1) & !is.na(simdat$WBC1) & 
                              !is.na(simdat$RBC1) & !is.na(simdat$PLATE1) & 
                              !is.na(simdat$LEU3N2) & !is.na(simdat$LEU3N3) &
                              !is.na(simdat$A1) & !is.na(simdat$A2) & !is.na(simdat$dosage)), ]
  
  # 添加一列 Visit，记录当前的 d 值
  simdatFit$Visit <- d
  
  # 将当前 d 的 simdatFit 添加到 all_simdatFit 列表中
  all_simdatFit[[paste0("d_", d)]] <- simdatFit
}

# 将所有 d 的 simdatFit 合并成一个数据框
final_simdatFit <- do.call(rbind, all_simdatFit)
final_simdatFit$LEU3N1=sqrt(final_simdatFit$LEU3N1)
final_simdatFit$LEU2N1=sqrt(final_simdatFit$LEU2N1) 
final_simdatFit$WBC1=sqrt(final_simdatFit$WBC1)  
final_simdatFit$RBC1=sqrt(final_simdatFit$RBC1)
final_simdatFit$PLATE1=sqrt(final_simdatFit$PLATE1)

final_simdatFit$LEU3N2=sqrt(final_simdatFit$LEU3N2)
final_simdatFit$LEU2N2=sqrt(final_simdatFit$LEU2N2) 
final_simdatFit$WBC2=sqrt(final_simdatFit$WBC2)  
final_simdatFit$RBC2=sqrt(final_simdatFit$RBC2)
final_simdatFit$PLATE2=sqrt(final_simdatFit$PLATE2)

final_simdatFit$LEU3N3=sqrt(final_simdatFit$LEU3N3)
# 查看最终合并的结果
head(final_simdatFit)

#### save final_simdatFit
write.csv(final_simdatFit, paste(DIREC, "dataset/final_simdatFit.csv", sep=""), row.names=FALSE)
