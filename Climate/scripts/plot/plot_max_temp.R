
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

############################################### ANNUAL

climate_data <- read_csv("data/cleaned_annual_climate_data.csv")

Year_points <- c(1969, 1977, 1980, 1981, 1996, 1998, 2013, 2014, 2018)


points_data <- data.frame(
  Year = c(
    1980, 1996,  # not
    1980, 1996, 1998,  # breakfast (idetect+ic)
    1980, 1996, 1998, 2018,  # breakfast (idetect_seq+thresh)
    1980, 1996, 1998,  # breakfast (wbs+ic)
    1980, 1996, 1998,  # breakfast (wbs2+sdll)
    1980, 1996, 1998,  # breakfast (not+ic)
    1996,  # breakfast (tguh+lp)
    1996,  # breakfast (wcm+gsc)
    1980, 1996, 1998,  # ID
    1980,  # mosum
    1977, 1980,  # cpt.meanvar
    1981,  # CE.Normal.Mean
    1969,  # simulated_annealing
    1977,  # brute_force
    1980, 1996, 1998, 2013,  # InspectChangepoint
    1980, 1996, 1998, 2013, 2023  # Segmentor
  ),
  
 
  temp_max_daily = c(
    30, 30,  # not
    30.2, 30.2, 30.2,  # breakfast (idetect+ic)
    30.3, 30.3, 30.3, 30.3,  # breakfast (idetect_seq+thresh)
    30.4, 30.4, 30.4,  # breakfast (wbs+ic)
    30.5, 30.5, 30.5,  # breakfast (wbs2+sdll)
    30.6, 30.6, 30.6,  # breakfast (not+ic)
    30.7,  # breakfast (tguh+lp)
    30.8,  # breakfast (wcm+gsc)
    30.9, 30.9, 30.9,  # ID
    31,  # mosum
    31.2, 31.2,  # cpt.meanvar
    31.3,  # CE.Normal.Mean
    31.4,  # simulated_annealing
    31.5,  # brute_force
    31.6, 31.6, 31.6, 31.6,  # InspectChangepoint
    31.1, 31.1, 31.1, 31.1, 31.1  # Segmentor
  ),
  
  
  Method = c(
    "not", "not",  # not
    "breakfast(idetect+ic)", "breakfast(idetect+ic)", "breakfast(idetect+ic)",  # breakfast (idetect+ic)
    "breakfast(idetect_seq+thresh)", "breakfast(idetect_seq+thresh)", "breakfast idetect_seq+thresh)", "breakfast (idetect_seq+thresh)",  # breakfast (idetect_seq+thresh)
    "breakfast(wbs+ic)", "breakfast(wbs+ic)", "breakfast(wbs+ic)",  # breakfast (wbs+ic)
    "breakfast(wbs2+sdll)", "breakfast(wbs2+sdll)", "breakfast(wbs2+sdll)",  # breakfast (wbs2+sdll)
    "breakfast(not+ic)", "breakfast(not+ic)", "breakfast(not+ic)",  # breakfast (not+ic)
    "breakfast(tguh+lp)",  # breakfast (tguh+lp)
    "breakfast(wcm+gsc)",  # breakfast (wcm+gsc)
    "ID", "ID", "ID",  # ID
    "mosum",  # mosum
    "cpt.meanvar", "cpt.meanvar",  # cpt.meanvar
    "CE.Normal.Mean",  # CE.Normal.Mean
    "changepointsHD(simulated_annealing)",  # simulated_annealing
    "changepointsHD(brute_force)",  # brute_force
    "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", "InspectChangepoint",  # InspectChangepoint
    "Segmentor", "Segmentor", "Segmentor", "Segmentor", "Segmentor"  # Segmentor
  ),
  
  shape = c(
    1, 1,  # not
    2, 2, 2,  # breakfast (idetect+ic)
    3, 3, 3, 3,  # breakfast (idetect_seq+thresh)
    4, 4, 4,  # breakfast (wbs+ic)
    5, 5, 5,  # breakfast (wbs2+sdll)
    6, 6, 6,  # breakfast (not+ic)
    7,  # breakfast (tguh+lp)
    8,  # breakfast (wcm+gsc)
    9, 9, 9,  # ID
    10,  # mosum
    11, 11,  # cpt.meanvar
    12,  # CE.Normal.Mean
    13,  # simulated_annealing
    14,  # brute_force
    15, 15, 15, 15,  # InspectChangepoint
    16, 16, 16, 16, 16  # Segmentor
  ),
  
  color = rep(c("red", "blue", "green"), length.out = 38)
)

Year_points = unique(points_data$Year)

p <- ggplot(climate_data, aes(x = Year, y = temp_max_daily)) +
  geom_line(color = "blue") +  
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +  
  

  geom_point(data = points_data, aes(x = Year, y = temp_max_daily, color = Method, shape = Method), size = 4) +
  

  scale_color_manual(values = setNames(points_data$color, points_data$Method)) +
  scale_shape_manual(values = setNames(points_data$shape, points_data$Method)) +
  

  labs(
    x = "Year",
    y = 'Air Temperature Means Daily Maximum (°C)',
    color = "Method",  
    shape = "Method"   
  ) +
  theme_minimal()

print(p)
