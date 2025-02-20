library("readr")
library(dplyr)
library(changepoint)
cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")
cleaned_data <- cleaned_data %>%
  arrange(Year)

numeric_data <- cleaned_data %>%
  select(-Year) %>%
  mutate(across(everything(), as.numeric))  #
numeric_matrix <- as.matrix(numeric_data)

result <- cpt.meanvar(t(numeric_matrix[,1:2]), method = "BinSeg", penalty = "MBIC", Q = 5)

changepoints_list <- lapply(1:length(result), function(i) {
  changepoints <- cpts(result[[i]])  
  Year_points <- cleaned_data$Year[changepoints]  
  list(variable = colnames(numeric_matrix)[i], changepoints = changepoints, Years = Year_points)
})


for (i in 1:length(changepoints_list)) {
  cat("\n Change point detection results - variables:", changepoints_list[[i]]$variable, "\n")
  cat("Change point line number:", changepoints_list[[i]]$changepoints, "\n")
  cat("Corresponding month:", changepoints_list[[i]]$Years , "\n")
}


for (i in 1:length(result)) {
  plot(result[[i]], main = paste("Changepoint Detection for", colnames(numeric_matrix)[i]))
}

