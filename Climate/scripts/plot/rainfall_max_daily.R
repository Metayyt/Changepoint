
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
############################################### ANNUAL

climate_data <- read_csv("data/cleaned_annual_climate_data.csv")


points_data <- data.frame(
  Year = c(
    1977, 1978,  # breakfast (idetect+ic)
    1977, 1978,  # breakfast (idetect_seq+thresh)
    1968, 1969, 1977, 1978,  # breakfast (wbs+ic)
    1968, 1969, 1977, 1978, 2013,  # breakfast (wbs2+sdll)
    1968, 1969, 1977, 1978,  # breakfast (not+ic)
    1969,  # trendsegment
    1977,  # cpt.meanvar
    1969,  # simulated_annealing
    1977,  # brute_force
    1980, 1996, 1998, 2013,   # InspectChangepoint
    1968, 1969, 1977, 1978, 2023# Segmentor
  ),
  
  
  position = c(
    350, 350,  # breakfast (idetect+ic)
    310, 310,  # breakfast (idetect_seq+thresh)
    330, 330, 330, 330,  # breakfast (wbs+ic)
    290, 290, 290, 290, 290,  # breakfast (wbs2+sdll)
    200, 200, 200, 200,  # breakfast (not+ic)
    250,  # trendsegment
    250,  # cpt.meanvar
    210,   # simulated_annealing
    230,  # brute_force
    370, 370, 370, 370,  # InspectChangepoint
    270, 270, 270, 270, 270  # Segmentor
  ),
  
  
  Method = c(
    "breakfast(idetect+ic)", "breakfast(idetect+ic)",  # breakfast (idetect+ic)
    "breakfast(idetect_seq+thresh)", "breakfast(idetect_seq+thresh)",  # breakfast (idetect_seq+thresh)
    "breakfast(wbs+ic)", "breakfast(wbs+ic)", "breakfast(wbs+ic)", "breakfast(wbs+ic)",  # breakfast (wbs+ic)
    "breakfast(wbs2+sdll)", "breakfast(wbs2+sdll)", "breakfast(wbs2+sdll)", "breakfast(wbs2+sdll)", "breakfast(wbs2+sdll)",  # breakfast (wbs2+sdll)
    "breakfast(not+ic)", "breakfast(not+ic)", "breakfast(not+ic)", "breakfast(not+ic)",  # breakfast (not+ic)
    "trendsegment",  # trendsegment
    "cpt.meanvar",  # cpt.meanvar
    "changepointsHD(simulated_annealing)" ,  # simulated_annealing
    "changepointsHD(brute_force)",  # brute_force
    "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", # "InspectChangepoint"  # InspectChangepoint
    "Segmentor", "Segmentor", "Segmentor","Segmentor","Segmentor"  # Segmentor 
     ),
  
  shape = c(
    1, 1,  # breakfast (idetect+ic)
    2, 2,  # breakfast (idetect_seq+thresh)
    3, 3, 3, 3,  # breakfast (wbs+ic)
    4, 4, 4, 4, 4,  # breakfast (wbs2+sdll)
    5, 5, 5, 5,  # breakfast (not+ic)
    6,  # trendsegment
    7,  # cpt.meanvar
    8,  # simulated_annealing
    9,  # brute_force
    10, 10, 10, 10, 10,  # InspectChangepoint
    11, 11, 11, 11
  ),
  
  color = rep(c("red", "blue", "green"), length.out = 30)
)




#Year_points = set(climate_data['Year'])
Year_points = unique(points_data$Year)
p <- ggplot(climate_data, aes(x = Year, y = rainfall_max_daily)) +
  geom_line(color = "blue") +  
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +  
  

  geom_point(data = points_data, aes(x = Year, y = position, color = Method, shape = Method), size = 4) +
  

  scale_color_manual(values = setNames(points_data$color, points_data$Method)) +
  scale_shape_manual(values = setNames(points_data$shape, points_data$Method)) +
  

  labs(
    x = "Year",
    y = 'Highest Daily Rainfall Total (mm)',
    color = "Method", 
    shape = "Method"   
  ) +
  theme_minimal()


print(p) 
