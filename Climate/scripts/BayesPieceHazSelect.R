# Load the BayesPieceHazSelect package
library(BayesPieceHazSelect)
library(dplyr)
library(readr)
library(ggplot2)
cleaned_data <- read_csv("data/cleaned_annual_climate_data.csv")
cleaned_data$humidity_min <- as.numeric(as.character(cleaned_data$humidity_min))
cleaned_data$humidity_mean_24hr <- as.numeric(as.character(cleaned_data$humidity_mean_24hr))
cleaned_data <- na.omit(cleaned_data)
cleaned_data <- cleaned_data %>%
  arrange(Year)

# Assume you have a column of weather data (e.g., daily temperatures)
Y <- cleaned_data$temp_max_daily  # your weather data
I <- rep(1, length(Y))  # no censoring, so all observations are fully observed
X <- cleaned_data %>%
  select(-Year) %>%
  select(-temp_max_daily) %>%
  mutate(across(everything(), as.numeric))
X = as.matrix(X)



# Define hyperparameters and other arguments #####
B <- 100  # number of MCMC iterations
####Read in Hyperparameters
##Swap Rate
psi=.5
c=20
###Eta Beta function probabilities
z1a=.4
z1b=1.6
####Hierarchical lam params
###Sigma^2 lambda_ hyperparameters
a1=.7
b1=.7
##Spacing dependence c in [0,1]
clam1=1
#####NumSplit
alpha1=3
J1max=10
####Split Point Starting Value ###
J1=3
##Tuning parameter for lambda
cl1=.25
###Beta Starting Values
beta1start=c(0,0,-1,0,0,0,1,1,1)
hyper=c(psi,c,z1a,z1b,a1,b1,clam1,alpha1,J1max,J1,cl1)
###Number of iterations and output location
B=200
Path=tempdir()
inc=2
burn=.4

# Run the model
result <- PiecewiseBayesSelect(Y, I=I, X=X, hyperparameters=hyper,beta1start= beta1start, B=B, inc=inc, Path=Path, burn = burn)


