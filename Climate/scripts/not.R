library(not)
library(ggplot2)
library(dplyr)
library(readr)

cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")

cleaned_data <- cleaned_data %>%
  arrange(Year)

numeric_data <- cleaned_data %>%
  select(-Year) %>%
  mutate(across(everything(), as.numeric))  
numeric_matrix <- as.matrix(numeric_data)

changepoints_list <- lapply(1:2, function(i) {
  w <- not(numeric_matrix[, i])  
  fo <- features(w)  #
  changepoints <- fo$cpt  #
  if (length(changepoints) == 0) {
    changepoints <- NA  
  }
  list(variable = colnames(numeric_matrix)[i], changepoints = changepoints)
})

changepoints_Years <- lapply(1:length(changepoints_list), function(i) {
  changepoints <- changepoints_list[[i]]$changepoints
  if (all(is.na(changepoints))) {
    Year_points <- NA  
  } else {
    Year_points <- cleaned_data$Year[changepoints] 
  }
  list(variable = changepoints_list[[i]]$variable, Years = Year_points)
})

for (i in 1:length(changepoints_Years)) {
  if (all(is.na(changepoints_Years[[i]]$Years))) {
    cat("\n variable:", changepoints_Years[[i]]$variable, "No change points detected\n")
  } else {
    cat("\n variable:", changepoints_Years[[i]]$variable, "\n")
    cat("Month corresponding to the change point:", changepoints_Years[[i]]$Years, "\n")
  }
}

for (i in 1:2) {
  w <- not(numeric_matrix[, i])  
  plot(w, main = paste("Changepoint Detection for", colnames(numeric_matrix)[i])) 
}
