library(changepointsHD)
library(dplyr)
library(readr)

cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")

cleaned_data <- cleaned_data %>%
  arrange(Year)

numeric_data <- cleaned_data %>%
  select(-Year) %>%
  mutate(across(everything(), as.numeric))  
numeric_matrix <- as.matrix(numeric_data)
numeric_matrix = numeric_matrix[,c(1,2,5,6,7,8,10)]
cov_est <- cov(numeric_matrix)
init <- solve(cov_est)       

prox_gradient_params <- list(
  update_w = 0.1,
  update_change = 0.99,
  regularizer = 0.1,
  max_iter = 1,
  tol = 1e-5
)

prox_gradient_ll_params <- list(
  regularizer = 0.1
)

changepoints_mod <- changepointsMod(
  bbmod = prox_gradient_mapping,        
  log_likelihood = prox_gradient_ll,     
  bbmod_params = prox_gradient_params,  
  ll_params = prox_gradient_ll_params,   
  part_values = list(init, init),        
  data = list(numeric_matrix)         

changepoints_mod_result <- simulated_annealing(changepoints_mod, buff = 10)
simulated_annealing_Year_points=cleaned_data$Year[changepoints_mod_result@changepoints]
cat("Change point locations detected by simulated annealing:",  simulated_annealing_Year_points , "\n")

changepoints_mod@part_values <- list(init, init)  
changepoints_mod <- brute_force(changepoints_mod, buff = 10)
cat("Change point locations detected by Brute Force:", changepoints_mod@changepoints, "\n")

res_ro <- rank_one(numeric_matrix, init, update_w = 0.1, regularizer = 0.1)
rank_one_Year_points=cleaned_data$Year[res_ro$tau]
cat("Rank-one:", rank_one_Year_points , "\n")

changepoints_mod@data <- list(numeric_matrix)             
changepoints_mod@part_values <- list(init, init)        

changepoints_mod <- binary_segmentation(
  changepoints_mod,
  method = simulated_annealing,
  thresh = 0,
  buff = 10,
)

binary_segmentation_Year_points=cleaned_data$Year[changepoints_mod@changepoints]

cat("The position of the change points detected by binary segmentation:", binary_segmentation_Year_points , "\n")

#统一输出变点：
cat("Change point locations detected by simulated annealing:",  simulated_annealing_Year_points , "\n")
cat("Rank-one:",  rank_one_Year_points , "\n")
cat("The position of the change points detected by binary segmentation:",  binary_segmentation_Year_points , "\n")
