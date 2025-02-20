library(breakpoint)
library(dplyr)
library(readr)

cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")
cleaned_data <- cleaned_data %>%
  arrange(Year)

numeric_data <- cleaned_data %>%
  select(-Year) %>%
  mutate(across(everything(), as.numeric))
numeric_matrix <- as.matrix(numeric_data)

all_results <- list()

for (i in 1:2) {
  print(i)
  numeric_df <- as.data.frame(numeric_matrix[, i])
  
  result <- CE.Normal.Mean(numeric_df, Nmax = 10, penalty = "BIC")
  
  changepoints <- result$BP.Loc
  
  Year_points <- cleaned_data$Year[changepoints]
  
  all_results[[i]] <- list(
    variable = colnames(numeric_matrix)[i],
    changepoints = changepoints,
    Year_points = Year_points
  )
}

for (i in 1:length(all_results)) {
  cat("\n variable:", all_results[[i]]$variable, "\n")
  if (all(is.null(all_results[[i]]$Year_points))) {
    cat("No change points detected\n")
  } else {
    cat("Month corresponding to the change point:", all_results[[i]]$Year_points , "\n")
  }
}

