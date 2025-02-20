
calculate_gap <- function(row) {
  indices <- which(row == 1) 
  if (length(indices) < 2) {
    return(NA) # 
  }
  return(max(indices) - min(indices)) # 
}

hivVisit_columns <- grep("^hivVisit", names(data), value = TRUE) 
hivVisit_data <- data[, hivVisit_columns]
data$hivVisit_gap <- apply(hivVisit_data, 1, calculate_gap)

# 
table(data$hivVisit_gap)

return_first_time <- function(row) {
  indices <- which(row == 1) #
  if (length(indices) < 1) {
    return(NA) # 
  }
  return(indices[1]) # 

data$hivVisit_first_time <- apply(hivVisit_data, 1, return_first_time)

TreatVisit_columns <- grep("^TreatVisit", names(data), value = TRUE) # TreatVisit
TreatVisit_data <- data[, TreatVisit_columns]
data$TreatVisit_gap <- apply(TreatVisit_data, 1, calculate_gap)
data$TreatVisit_first_time = apply(TreatVisit_data, 1, return_first_time)
table(data$TreatVisit_first_time)
table(data$TreatVisit_gap)

#data[, c("hivVisit_gap", "hivVisit_first_time", "TreatVisit_gap", "TreatVisit_first_time")]

data$first_treat_last_visit_gap = data$TreatVisit_first_time - data$hivVisit_first_time
table(data$first_treat_last_visit_gap)
head(data[, c("hivVisit_gap", "hivVisit_first_time", "TreatVisit_gap", "TreatVisit_first_time", "first_treat_last_visit_gap")])
