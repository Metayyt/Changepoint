
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
############################################### ANNUAL

climate_data <- read_csv("data/cleaned_annual_climate_data.csv")


points_data <- data.frame(
  Year = c(
    2013,2013,2013,2013,2013,2013,2013,2013,2013,  # not
    2001,  # breakfast (wcm+gsc)
    1975, 1997, 2018, 2019, 2023,  # Segmentor
    1969,  # simulated_annealing
    1977,  # brute_force
    1980, 1996, 1998, 2013   # InspectChangepoint
  ),
  

  position = c(
    4.8,4.9,5,5.1,5.2,5.3,5.4,5.5,5.6,  # not
    5,  # breakfast (wcm+gsc)
    5, 5, 5, 5, 5,  # Segmentor
    6,    # simulated_annealing
    6.2,  # brute_force
    5.2, 5.2, 5.2, 5.2   # InspectChangepoint
  ),
  

  Method = c(
    "not", "breakfast(idetect+ic)", "ID", "mosum",
"breakfast(idetect_seq+thresh)",
"breakfast(wbs+ic)", "breakfast(wbs2+sdll)", 
"breakfast(not+ic)", "breakfast(tguh+lp)",  # breakfast (tguh+lp)
    "breakfast(wcm+gsc)",  # breakfast (wcm+gsc)
    "Segmentor", "Segmentor", "Segmentor", "Segmentor", "Segmentor",  # Segmentor
    "changepointsHD(simulated_annealing)",   # simulated_annealing
    "changepointsHD(brute_force)",  # brute_force
    "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", "InspectChangepoint" 
  ),
  
  shape = c(
    1,2,3,4,5,6,7,9,12,  # not
    8,  # breakfast (wcm+gsc)
    11, 11, 11, 11, 11,  # Segmentor
    8,   # simulated_annealing
    9,  # brute_force
    10, 10, 10, 10   # InspectChangepoint
  ),
  
  color = rep(c("red", "blue", "green"), length.out = 21)
)





#Year_points = set(climate_data['Year'])
Year_points = unique(points_data$Year)
p <- ggplot(climate_data, aes(x = Year, y = sunshine_mean_daily)) +
  geom_line(color = "blue") +  
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +  
  

  geom_point(data = points_data, aes(x = Year, y = position, color = Method, shape = Method), size = 4) +
  

  scale_color_manual(values = setNames(points_data$color, points_data$Method)) +
  scale_shape_manual(values = setNames(points_data$shape, points_data$Method)) +

  labs(
    x = "Year",
    y = 'Bright Sunshine Daily Mean (hours)',
    color = "Method",  
    shape = "Method" 
  ) +
  theme_minimal()

print(p) 
