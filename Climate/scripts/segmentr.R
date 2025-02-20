library(segmentr)
library(dplyr)
library(readr)

##########Super slow. It seems to be related to the mean_lik I defined. 


cleaned_data <- read_csv("data/cleaned_monthly_climate_data.csv")

cleaned_data <- cleaned_data %>%
  arrange(Month)

numeric_data <- cleaned_data %>%
  select(-Month) %>%
  mutate(across(everything(), as.numeric))

numeric_matrix <- as.matrix(scale(numeric_data))

mean_lik <- function(X) abs(mean(X))
all_results <- list()

for (i in 1:ncol(numeric_matrix)) {
  print(i)
  numeric_vector <- numeric_matrix[, i]
  
  result <- segment(t(numeric_vector), likelihood = mean_lik)
  
  changepoints <- result$changepoints
  
  month_points <- cleaned_data$Month[changepoints]
  
  all_results[[i]] <- list(
    variable = colnames(numeric_matrix)[i],
    changepoints = changepoints,
    month_points = month_points
  )
}

for (i in 1:length(all_results)) {
  cat("\n variable:", all_results[[i]]$variable, "\n")
  if (is.null(all_results[[i]]$month_points)) {
    cat("No change points detected\n")
  } else {
    cat("No change points detected:", all_results[[i]]$month_points, "\n")
  }
}


result <- segment(t(numeric_matrix), likelihood = mean_lik)

changepoints <- result$changepoints

month_points <- cleaned_data$Month[changepoints]

month_points
