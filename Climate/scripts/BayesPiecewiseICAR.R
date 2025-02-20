library(BayesPiecewiseICAR) 
library(dplyr)
library(readr)
library(ggplot2)
cleaned_data <- read_csv("data/cleaned_monthly_climate_data.csv")

cleaned_data <- cleaned_data %>%
  arrange(Month)

Y = cleaned_data$temp_max_daily

I = rep(1, length(Y))
a1 = 0.7
b1 = 0.7
phi = 3
Jmax = 20
cl1 = 0.25
clam1 = 0.5
J1 = 0

hyper = c(a1, b1, phi, Jmax, cl1, J1, clam1)

B = 100

result = ICARBHSampler(Y, I, B, hyper)

print(result)
