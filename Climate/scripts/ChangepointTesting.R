library(ChangepointTesting)
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

all_change_points <- list()

for (i in 1:ncol(numeric_matrix)) {
  column_name <- colnames(numeric_matrix)[i] 
  
  standardized_data <- scale(numeric_matrix[, i]) 
  
  z_scores <- standardized_data
  p_values <- 2.0 * (1.0 - pnorm(abs(z_scores)))
  p_values <- as.numeric(p_values) 
  
  result <- tryCatch({
    changePoint(pvalues = p_values, alpha = 0.05, km = log(length(p_values))^2, lm = length(p_values)^(1/4), compare = "Both")
  }, error = function(e) NULL)
  
  if (is.null(result)) {
    cat("variable:", column_name, "Change point detection failed\n")
    next
  }
  
  # 提取检测到的变点位置
  change_pts <- changePts(result)
  
  # 检查是否检测到了变点
  if (is.null(change_pts)) {
    cat("variable:", column_name, "No change points detected\n")
    all_change_points[[i]] <- list(
      variable = column_name,
      Year_points = NULL
    )
  } else {
    Year_points <- cleaned_data$Year[change_pts]
    cat("variable:", column_name, "Detected change point locations:", change_pts, "\n")
    all_change_points[[i]] <- list(
      variable = column_name,
      Year_points = Year_points
    )
  }
}

for (i in 1:length(all_change_points)) {
  if (is.null(all_change_points[[i]]$Year_points)) {
    cat("\n variable:", all_change_points[[i]]$variable, "No change points detected\n")
  } else {
    cat("\n variable:", all_change_points[[i]]$variable, "\n")
    cat("Month corresponding to the change point:", all_change_points[[i]]$Year_points, "\n")
  }
}
