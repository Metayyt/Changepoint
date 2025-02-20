library(dplyr)
library(readr)

cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")

cleaned_data <- cleaned_data %>%
  arrange(Year)

numeric_data <- cleaned_data %>%
  select(-Year) %>%
  mutate(across(everything(), as.numeric))
numeric_matrix <- as.matrix(numeric_data)
library(changepointsVar)

changepoints_list <- lapply(1:2, function(i) {
  numeric_vector_scaled <- scale(numeric_matrix[, i])
  
  result <- jumpointsVar(y = numeric_vector_scaled, k = 30, y.res = FALSE)
  
  changepoints <- result$psi
  
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
    cat("Month corresponding to the change point:", format(changepoints_Years[[i]]$Years, "%Y-%m-%d"), "\n")
  }
}

###### Regression
y <- cleaned_data$temp_max_daily  
x <- cleaned_data[, c("rainfall_total", "rainy_days", "sunshine_mean_daily")]
model <- lm(y ~ ., data = x)

residuals_squared <- resid(model)^2

result <- jumpointsVar(y = residuals_squared, k = 30, y.res = TRUE)

changepoints <- result$psi
print(changepoints)

Year_points <- cleaned_data$Year[changepoints]
cat("Month corresponding to the change point:", Year_points, "\n")

library(ggplot2)

data_to_plot <- data.frame(
  Year = cleaned_data$Year,
  ResidualsSquared = residuals_squared
)

ggplot(data_to_plot, aes(x = Year, y = ResidualsSquared)) +
  geom_line(color = "blue") + 
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +
  labs(title = "Variance Changepoint Detection on Squared Residuals", x = "Year", y = "Squared Residuals") +
  theme_minimal()


