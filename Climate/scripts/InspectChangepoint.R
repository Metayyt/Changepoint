library(InspectChangepoint)
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
numeric_matrix[,c(1,2,5,6,7,8,10)]
n <- nrow(numeric_matrix)
p <- ncol(numeric_matrix)

threshold <- compute.threshold(n = n, p = p)

result <- inspect(t(numeric_matrix), threshold = threshold)

changepoints_matrix <- result[["changepoints"]]  

changepoints_index <- changepoints_matrix[, 1]

Year_points <- cleaned_data$Year[changepoints_index]

cat("Month corresponding to the change point:",  Year_points , "\n")

data_to_plot <- data.frame(
  Year = cleaned_data$Year,
  Value = numeric_matrix[, 1]  
)

ggplot(data_to_plot, aes(x = Year, y = Value)) +
  geom_line(color = "blue") +  # 
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") + 
  labs(title = "Changepoint Detection", x = "Year", y = "Value") +
  theme_minimal()

