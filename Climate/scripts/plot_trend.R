library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
############################################### ANNUAL
climate_data <- read_csv("data/cleaned_annual_climate_data.csv")

Year_points <- c(1969, 1977, 1980, 1981, 1996, 1998, 2018, 2023)
p <- ggplot(climate_data, aes(x = Year, y = temp_max_daily)) +
  geom_line(color = "blue") + 
  geom_vline(xintercept = as.numeric(Year_points), linetype = "dashed", color = "red") + 
  
  labs(
    x = "Year",
    y = 'Air Temperature Means Daily Maximum (°C)',
  ) +
  theme_minimal()

p + 
  annotate("point", x = 1969, y = 30, color = "blue", size = 3, shape = 10) + 
  annotate("point", x = 1977, y = 30, color = "red", size = 3, shape = 4) + 
  annotate("point", x = 1977, y = 30.25, color = "green", size = 3, shape = 4) + 
  annotate("point", x = 1980, y = 30.25, color = "green", size = 3, shape = 2)


############################

