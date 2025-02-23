
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
############################################### ANNUAL

climate_data <- read_csv("data/cleaned_annual_climate_data.csv")


points_data <- data.frame(
  Year = c(
    1969,  # simulated_annealing
    1977,  # brute_force
    1980, 1996, 1998, 2013,  # InspectChangepoint
    1996, 1997, 2013, 2020, 2023  # Segmentor
  ),
  

  position = c(
    1500,  # simulated_annealing
    1525,  # brute_force
    1550, 1550, 1550, 1550,  # InspectChangepoint
    1475, 1475, 1475, 1475, 1475  # Segmentor
  ),
  
  
  Method = c(
    "changepointsHD(simulated_annealing)",  # simulated_annealing
    "changepointsHD(brute_force)",  # brute_force
    "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", "InspectChangepoint",  # InspectChangepoint
    "Segmentor", "Segmentor", "Segmentor","Segmentor","Segmentor"  # Segmentor
  ),
  
  shape = c(
    13,  # simulated_annealing
    14,  # brute_force
    15, 15, 15, 15,  # InspectChangepoint
    16, 16, 16, 16, 16
  ),
  
  color = rep(c("red", "blue", "green"), length.out = 11)
)



#Year_points = set(climate_data['Year'])
Year_points = unique(points_data$Year)
p <- ggplot(climate_data, aes(x = Year, y = rainfall_total)) +
  geom_line(color = "blue") +  
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +  
  

  geom_point(data = points_data, aes(x = Year, y = position, color = Method, shape = Method), size = 4) +
  

  scale_color_manual(values = setNames(points_data$color, points_data$Method)) +
  scale_shape_manual(values = setNames(points_data$shape, points_data$Method)) +
  

  labs(
    x = "Year",
    y = 'Total Rainfall (mm)',
    color = "Method",  
    shape = "Method"  
  ) +
  theme_minimal()


print(p) 
