
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
############################################### ANNUAL

climate_data <- read_csv("data/cleaned_annual_climate_data.csv")


points_data <- data.frame(
  Year = c(
    1976, 1998, # CE.Normal.Mean
    1969,  # simulated_annealing
    1977,  # brute_force
    1980, 1996, 1998, 2013,  # InspectChangepoint
    1975, 1997, 2018, 2019, 2023  # Segmentor
  ),
  
  
  position = c(
    140, 140, # CE.Normal.Mean
    160,  # simulated_annealing
    150,  # brute_force
    130, 130, 130, 130,  # InspectChangepoint
    120, 120, 120, 120, 120  # Segmentor
  ),
  
  
  Method = c(
    "CE.Normal.Mean", "CE.Normal.Mean",  # CE.Normal.Mean
    "changepointsHD(simulated_annealing)",  # simulated_annealing
    "changepointsHD(brute_force)",  # brute_force
    "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", "InspectChangepoint",  # InspectChangepoint
    "Segmentor", "Segmentor", "Segmentor","Segmentor","Segmentor"  # Segmentor
  ),
  
  shape = c(
    12, 12, # CE.Normal.Mean
    13,  # simulated_annealing
    14,  # brute_force
    15, 15, 15, 15,  # InspectChangepoint
    16, 16, 16, 16, 16
  ),
  
  color = rep(c("red", "blue", "green"), length.out = 13)
)



#Year_points = set(climate_data['Year'])
Year_points = unique(points_data$Year)
p <- ggplot(climate_data, aes(x = Year, y = rainy_days)) +
  geom_line(color = "blue") +  
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +  
  

  geom_point(data = points_data, aes(x = Year, y = position, color = Method, shape = Method), size = 4) +
  

  scale_color_manual(values = setNames(points_data$color, points_data$Method)) +
  scale_shape_manual(values = setNames(points_data$shape, points_data$Method)) +
  

  labs(
    x = "Year",
    y = 'Number of Rainy Days (days)',
    color = "Method",  
    shape = "Method"   
  ) +
  theme_minimal()


print(p) 
