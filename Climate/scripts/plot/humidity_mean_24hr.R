
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
############################################### ANNUAL

climate_data <- read_csv("data/cleaned_annual_climate_data.csv")


points_data <- data.frame(
  Year = c(
    1980, 2013,  # not, breakfast (idetect+ic)  breakfast (tguh+lp)  ID cpt.meanvar
    1972, 1980, 2013, #breakfast(idetect_seq+thresh)
    1972, 1980, 1996, 1997, 2013, 2016, 2017,  # breakfast(wbs+ic)  breakfast(wbs2+sdll)  breakfast(not+ic)
    2010, 2016, # trendsegment
    1981, 2014, # CE.Normal.Mean
    1975, 1997, 2018, 2019, 2023,  # Segmentor
    1969,  # simulated_annealing
    1977,  # brute_force
    1980, 1996, 1998, 2013   # InspectChangepoint
  ),
  

  position = c(
    78, 78,  # not, breakfast (idetect+ic)  breakfast (tguh+lp)  ID cpt.meanvar
    77, 77, 77,  # breakfast(idetect_seq+thresh)
    76, 76, 76, 76, 76, 76, 76,   # breakfast(wbs+ic)  breakfast(wbs2+sdll)  breakfast(not+ic)
    80, 80, # trendsegment
    75.75, 75.75, # CE.Normal.Mean
    75, 75, 75, 75, 75,  # Segmentor
    81,  # simulated_annealing
    82,  # brute_force
    75.25, 75.25, 75.25, 75.25   # InspectChangepoint
  ),
  
 
  Method = c(
    "not, breakfast(idetect+ic), breakfast(tguh+lp), ID, cpt.meanvar", "not, breakfast(idetect+ic), breakfast(tguh+lp), ID, cpt.meanvar",  # breakfast (tguh+lp)
    "breakfast(idetect_seq+thresh)","breakfast(idetect_seq+thresh)","breakfast(idetect_seq+thresh)",  # breakfast (wcm+gsc)
    "breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)","breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)","breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)",
    "breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)","breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)","breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)",
    "breakfast(wbs+ic), breakfast(wbs2+sdll), breakfast(not+ic)",
    "trendsegment","trendsegment",
    "CE.Normal.Mean","CE.Normal.Mean",
    "Segmentor", "Segmentor", "Segmentor", "Segmentor", "Segmentor",  # Segmentor
    "changepointsHD(simulated_annealing)",   # simulated_annealing
    "changepointsHD(brute_force)",  # brute_force
    "InspectChangepoint", "InspectChangepoint", "InspectChangepoint", "InspectChangepoint" 
  ),
  
  shape = c(
    1, 1,  # not, breakfast (idetect+ic)  breakfast (tguh+lp)  ID cpt.meanvar
    2, 2, 2,  # breakfast(idetect_seq+thresh)
    3, 3, 3, 3, 3, 3, 3,   # breakfast(wbs+ic)  breakfast(wbs2+sdll)  breakfast(not+ic)
    4, 4, # trendsegment
    5, 5, # CE.Normal.Mean
    6, 6, 6, 6, 6,  # Segmentor
    7,  # simulated_annealing
    8,  # brute_force
    9, 9, 9, 9   # InspectChangepoint
  ),
  
  color = rep(c("red", "blue", "green"), length.out = 27)
)





#Year_points = set(climate_data['Year'])
Year_points = unique(points_data$Year)
p <- ggplot(climate_data, aes(x = Year, y = humidity_mean_24hr)) +
  geom_line(color = "blue") +  # 
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") +  # 
  

  geom_point(data = points_data, aes(x = Year, y = position, color = Method, shape = Method), size = 4) +
  

  scale_color_manual(values = setNames(points_data$color, points_data$Method)) +
  scale_shape_manual(values = setNames(points_data$shape, points_data$Method)) +
  

  labs(
    x = "Year",
    y = '24-Hour Mean Relative Humidity (%)',
    color = "Method",  
    shape = "Method"   #
  ) +
  theme_minimal()


print(p) 
