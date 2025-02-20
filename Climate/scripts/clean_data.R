library(dplyr)
library(readr)
library(lubridate)
climate_data <- read_csv("data/monthly_climate.csv")
climate_data <- t(climate_data)
colnames(climate_data) <- climate_data[1,]
climate_data <- climate_data[-1,]
cleaned_data <- data.frame(climate_data)

colnames(cleaned_data) <- c(
  "Month",                 # Month
  "temp_max_daily",        # Air.Temperature.Means.Daily.Maximum..Degree.Celsius.
  "temp_min_daily",        # Air.Temperature.Means.Daily.Minimum..Degree.Celsius.
  "temp_max_extreme",      # Air.Temperature.Absolute.Extremes.Maximum..Degree.Celsius.
  "temp_min_extreme",      # Air.Temperature.Absolute.Extremes.Minimum..Degree.Celsius.
  "rainfall_total",        # Total.Rainfall..Millimetre.
  "rainfall_max_daily",    # Highest.Daily.Rainfall.Total..Millimetre.
  "rainy_days",            # Number.Of.Rainy.Days..Number.
  "sunshine_mean_daily",   # Bright.Sunshine.Daily.Mean..Hour.
  "humidity_min",          # Minimum.Relative.Humidity..Per.Cent.
  "humidity_mean_24hr"     # X24.Hours.Mean.Relative.Humidity..Per.Cent.
)
cleaned_data <- cleaned_data %>%
  mutate(Month = ym(Month))
cleaned_data$humidity_min <- as.numeric(as.character(cleaned_data$humidity_min))
cleaned_data$humidity_mean_24hr <- as.numeric(as.character(cleaned_data$humidity_mean_24hr))

cleaned_data <- na.omit(cleaned_data)
write_csv(cleaned_data, "data/cleaned_monthly_climate_data.csv")


##########################################################Annual data

climate_data <- read_csv("data/annual_climate.csv")
climate_data <- t(climate_data)
colnames(climate_data) <- climate_data[1,]
climate_data <- climate_data[-1,]
cleaned_data <- data.frame(climate_data)

colnames(cleaned_data) <- c(
  "Year",                  # Year
  "temp_max_daily",        # Air.Temperature.Means.Daily.Maximum..Degree.Celsius.
  "temp_min_daily",        # Air.Temperature.Means.Daily.Minimum..Degree.Celsius.
  "temp_max_extreme",      # Air.Temperature.Absolute.Extremes.Maximum..Degree.Celsius.
  "temp_min_extreme",      # Air.Temperature.Absolute.Extremes.Minimum..Degree.Celsius.
  "rainfall_total",        # Total.Rainfall..Millimetre.
  "rainfall_max_daily",    # Highest.Daily.Rainfall.Total..Millimetre.
  "rainy_days",            # Number.Of.Rainy.Days..Number.
  "sunshine_mean_daily",   # Bright.Sunshine.Daily.Mean..Hour.
  "humidity_min",          # Minimum.Relative.Humidity..Per.Cent.
  "humidity_mean_24hr"     # X24.Hours.Mean.Relative.Humidity..Per.Cent.
)
cleaned_data <- cleaned_data %>%
  mutate(Year = as.numeric(Year))
cleaned_data$humidity_min <- as.numeric(as.character(cleaned_data$humidity_min))
cleaned_data$humidity_mean_24hr <- as.numeric(as.character(cleaned_data$humidity_mean_24hr))
#cleaned_data <- na.omit(cleaned_data)
write_csv(cleaned_data, "data/cleaned_annual_climate_data.csv")
