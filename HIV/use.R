# read dataset/final_simdatFit.csv in R
library('CPMA' )
final_simdatFit <- read.csv("Application/dataset/final_simdatFit.csv")
head(final_simdatFit)
final_simdatFit = final_simdatFit[final_simdatFit$hiv1 == 1,]

# select col of LEU3N1   LEU3N2   LEU3N3   LEU2N1       WBC1       RBC1     PLATE1  VLOAD1 age white college  hiv1 Visit
  three_visit_data = final_simdatFit[,c("LEU3N2","LEU3N1" ,"LEU2N1","RBC1","PLATE1","VLOAD1", "WBC1","dosage"  , "A1","college", "HB1")]#"A2","white", "age","PLATE1"
# remove HB1 >50的
  three_visit_data = three_visit_data[complete.cases(three_visit_data),]
three_visit_data = three_visit_data[three_visit_data$HB1 < 30,]
  

hist( three_visit_data$RBC1 )  
three_visit_data$VLOAD1 = log(three_visit_data$VLOAD1)

cor(three_visit_data)

# remove NA
three_visit_data = three_visit_data[complete.cases(three_visit_data),]
dim(three_visit_data)
xind =   2:10
zind =   2:8
three_visit_data[,1:8] = scale(three_visit_data[,1:8])

three_visit_data[,1:10] 
write.csv(three_visit_data[,1:10], "final.csv", row.names = FALSE)

#shape of three_visit_data
dim(three_visit_data)

# Prepare multiple ini.theta candidate parameters
ini_theta_candidates <- list(
  c(1, -1,0,0, -1, 0,0),
  c(1, 0, 0, 0, -1),
  #c( 1, 1, 1, 1, 1),#
  c(-1,0,0,0,1),
  c(0,0,0,1,0),
  #c(-0.1, -0.1, -0.1, -0.1, -0.1),
  c(0, 1, 0, 0, 0),
  ####
  c(0,0,0,1,0),
  c(1,1,0,0,0),
  c(1,0,1,0,0),
  c(1,0,0,1,0),
  c(1,0,0,0,1),
  c(0,1,1,0,0),
  c(0,1,0,1,0),
  c(0,1,0,0,1),
  c(0,0,1,0,0),
  c(0,0,1,0,1),
  c(0,0,0,1,1),
  c(1,1,1,0,0),
  c(1,1,0,1,1),
  c(0,1,1,1,1),
  c(1,0,1,1,1),
  c(1,1,1,0,1),
  c(1,1,1,1,0)
)
# Iterate through and try different ini.theta values
for (ini_theta in ini_theta_candidates) {
  cat("Try ini.theta =", ini_theta, "\n")
  
  result <- tryCatch({
    # Try running CPlane
    full_out <- CPlane(
      data.tr = three_visit_data,
      data.te = NULL,
      yind = 1,
      xind = xind,
      zind = zind,
      ini.theta = ini_theta
    )
    cat(" success, ini.theta =", ini_theta, "\n")
    full_out  # Return results
  }, error = function(e) {
    cat("fail:", conditionMessage(e), "\n")
    NULL
  })
  
  # If successful, exit the loop
  if (!is.null(result)) {
    break
  }
}

if (is.null(result)) {
  cat("All ini.theta parameter candidates were tried and failed.\n")
} else {
  cat("A usable ini.theta is found and the result is stored in full_out.\n")
}

full_out[["train.res"]][["coefficient"]][["beta"]]

full_out[["train.res"]][["coefficient"]][["delta"]]

full_out[["train.res"]][["threshold"]]

full_out[["train.res"]][["theta"]]

table(full_out[["train.res"]][["subgroup"]])


y = three_visit_data$LEU3N2
three_visit_data <- as.matrix(three_visit_data) 
# X is the data frame without LEU3N2, not a list
X = three_visit_data[, xind]
ols_model <- lm(y ~ X)
summary(ols_model)

