library(mosum)
library(dplyr)
library(readr)
library(ggplot2)

cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")

cleaned_data <- cleaned_data %>%
  arrange(Year)

numeric_data <- cleaned_data %>%
  select(-Year) %>%
  mutate(across(everything(), as.numeric))
numeric_matrix <- as.matrix(numeric_data)

all_results <- list()

for (i in 1:ncol(numeric_matrix)) {
  column_name <- colnames(numeric_matrix)[i]  #
  result <- mosum(numeric_matrix[, i], G = 10)  #
  
  changepoints <- result$cpts
  
  Year_points <- cleaned_data$Year[changepoints]
  
  all_results[[i]] <- list(
    variable = column_name,
    result = result,
    Year_points = Year_points
  )
  
  cat("\n Variable:", column_name, "\n")
  cat("Detected change point locations:", changepoints, "\n")
  cat("Month corresponding to the change point:", Year_points , "\n")
}

# Visualize each column 
column_i_to_plot = 1# for example

column_name <- all_results[[column_i_to_plot]]$variable
result <- all_results[[column_i_to_plot]]$result
Year_points <- all_results[[column_i_to_plot]]$Year_points

data_to_plot <- data.frame(
  Year = cleaned_data$Year,   
  Value = numeric_matrix[, column_i_to_plot]  
)



