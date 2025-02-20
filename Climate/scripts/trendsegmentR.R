library(trendsegmentR)
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
  column_name <- colnames(numeric_matrix)[i]  
  result <- trendsegment(numeric_matrix[, i])  
  
  changepoints <- result$cpt  
  Year_points <- cleaned_data$Year[changepoints]  
  
  all_results[[i]] <- list(
    variable = column_name,
    changepoints = changepoints,
    Year_points = Year_points,
    result = result
  )
  
  cat("\n variable:", column_name, "\n")
  cat("The number of change points detected:", result$no.of.cpt, "\n")
  cat("Month corresponding to the change point:", Year_points , "\n")
}


column_i_to_plot = 4 # for example

column_name <- all_results[[column_i_to_plot]]$variable
result <- all_results[[column_i_to_plot]]$result
Year_points <- all_results[[column_i_to_plot]]$Year_points

data_to_plot <- data.frame(
  Year = cleaned_data$Year,
  Original = result$x,
  Estimated = result$est
)

ggplot(data_to_plot, aes(x = Year)) +
  geom_line(aes(y = Original, color = "Original Data")) + 
  geom_line(aes(y = Estimated, color = "Estimated Trend")) + 
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") + 
  labs(title = paste("Trend Segmentation for", column_name), x = "Year", y = column_name) +
  scale_color_manual(values = c("Original Data" = "blue", "Estimated Trend" = "green")) +
  theme_minimal() 

