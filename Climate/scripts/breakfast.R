library(breakfast)
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
  result <- breakfast(numeric_matrix[, i])
  changepoints <- result$cpt  #
  list(variable = colnames(numeric_matrix)[i], changepoints = changepoints)
})
final_results <- lapply(1:length(changepoints_list), function(i) {
  variable_name <- changepoints_list[[i]][["variable"]]  
  changepoints_info <- changepoints_list[[i]][["changepoints"]]
  
  lapply(1:7, function(j) {
    solution_path <- changepoints_info[[j]][["solution.path"]] 
    model_selection <- changepoints_info[[j]][["model.selection"]]  
    cpts <- changepoints_info[[j]][["cpts"]]  
    
    if (length(cpts) == 0) {
      Year_points <- NA  
    } else {
      Year_points <- cleaned_data$Year[cpts]  
    }
    
    list(
      variable = variable_name,
      method = solution_path,
      model = model_selection,
      changepoints = cpts,
      Years = Year_points
    )
  })
})

for (i in 1:length(final_results)) {
  cat("\n variable:", final_results[[i]][[1]]$variable, "\n")
  for (j in 1:7) {
    if (all(is.na(final_results[[i]][[j]]$Years))) {
      cat("Method:", final_results[[i]][[j]]$method, "\n",
          "| Model:", final_results[[i]][[j]]$model, "\n",
          "| No change points detected\n")
    } else {
      cat("Method:", final_results[[i]][[j]]$method, "\n",
          "| Model:", final_results[[i]][[j]]$model, "\n",
          "| Corresponding month:",  final_results[[i]][[j]]$Years , "\n")
    }
  }
}

changepoints_df <- do.call(rbind, lapply(1:length(final_results), function(i) {
  do.call(rbind, lapply(1:7, function(j) {
    data.frame(
      variable = final_results[[i]][[j]]$variable,
      method = final_results[[i]][[j]]$method,
      model = final_results[[i]][[j]]$model,
      changepoint_year = final_results[[i]][[j]]$Years
    )
  }))
}))

changepoints_df <- changepoints_df[!is.na(changepoints_df$changepoint_year), ]

changepoints_df = changepoints_df[1:4,]
library(ggplot2)
library(ggplot2)

if (!"temp_max_daily" %in% colnames(cleaned_data)) {
  stop("Make sure the temp_max_daily column exists in cleaned_data")
}

changepoints_df <- changepoints_df %>%
  left_join(cleaned_data, by = c("changepoint_year" = "Year"))  

if (any(is.na(changepoints_df$temp_max_daily))) {
  warning("There are unmatched change point years")
}

ggplot(cleaned_data, aes(x = Year)) +
  geom_line(aes(y = temp_max_daily), color = "red") +   
  labs(title = "Maximum Daily Temperature Trend with Changepoints", 
       x = "Year", 
       y = "Max Daily Temperature (°C)") +
  theme_minimal() +
  
  geom_point(data = changepoints_df, aes(x = changepoint_year, y = temp_max_daily, color = interaction(method, model)),
             shape = 8, size = 3) + 
  
  scale_color_manual(name = "Changepoint Detection", values = rainbow(length(unique(interaction(changepoints_df$method, changepoints_df$model)))))
