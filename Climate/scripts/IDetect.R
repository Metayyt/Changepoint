library(IDetect)
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
  result <- ID(numeric_matrix[, i])  
  changepoints <- result$cpt 
  list(variable = colnames(numeric_matrix)[i], changepoints = changepoints)
})

changepoints_Years <- lapply(1:length(changepoints_list), function(i) {
  changepoints <- changepoints_list[[i]]$changepoints
  if (length(changepoints) == 0) {
    Year_points <- NA  
  } else {
    Year_points <- cleaned_data$Year[changepoints]  
  }
  list(variable = changepoints_list[[i]]$variable, Years = Year_points)
})

for (i in 1:length(changepoints_Years)) {
  if (all(is.na(changepoints_Years[[i]]$Years))) {
    cat("\n Variable:", changepoints_Years[[i]]$variable, "No change points detected\n")
  } else {
    cat("\n Variable:", changepoints_Years[[i]]$variable, "\n")
    cat("Month corresponding to the change point:",  changepoints_Years[[i]]$Years , "\n")
  }
}


result <- ID(numeric_matrix[, 1]) 
changepoints <- result$cpt 

Year_points <- cleaned_data$Year[changepoints]

ggplot(cleaned_data, aes(x = Year, y = numeric_matrix[, 1])) +
  geom_line() +  
  geom_vline(xintercept = as.numeric(Year_points), color = "red", linetype = "dashed") +  
  labs(title = "Change point detection results", x = "month", y = "Variable Value") +
  theme_minimal()

