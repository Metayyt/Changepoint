library(Segmentor3IsBack)
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
  numeric_vector <- numeric_matrix[, i]
  
  result <- Segmentor(data = numeric_vector, Kmax = 5, model =2)
  #"Choose model=1 (Poisson), 2 (normal), 3 (Negative Binomial), (Normal-Variance) or 5 (Exponential)"
  
  breaks_matrix <- result@breaks
  
  Year_breaks_matrix <- matrix(NA, nrow = nrow(breaks_matrix), ncol = ncol(breaks_matrix))
  
  for (j in 1:nrow(breaks_matrix)) {
    for (k in 1:ncol(breaks_matrix)) {
      if (breaks_matrix[j, k] != 0) {
        Year_breaks_matrix[j, k] <- as.character(cleaned_data$Year[breaks_matrix[j, k]])
      } else {
        Year_breaks_matrix[j, k] <- NA 
      }
    }
  }
  
  all_results[[i]] <- list(
    variable = colnames(numeric_data)[i],  
    result = result,  
    Year_breaks_matrix = Year_breaks_matrix  
  )
}

for (i in 1:length(all_results)) {
  cat("\n variable:", all_results[[i]]$variable, "\n")
  cat("Month matrix corresponding to the change point (the i-th row indicates that if we want to keep i segments, the change point is these months):\n")
  print(all_results[[i]]$Year_breaks_matrix)
}
