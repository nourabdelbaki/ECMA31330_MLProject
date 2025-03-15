#     Nour Abdelbaki & Giuliana Triberti
## In this code, we are evaluating our random forests' models. We are using 
# random forests as a flexible way to get at the effect external monetary policy
# has on the US monetary policy decisions. We then use our model to recover 
# coefficients using the finite difference approach. To test whether our recovered
# betas (that come from the random forests) are robust against small noise, we
# will introduce some random noise and recover the betas again. Compare the betas
# recovered from the original data and the noisy data and see if there is a 
# statistical difference. 

# Import Libraries
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(purrr)

set.seed(123)

## Set working directory
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject")

## Read dataset
data <- read.csv("1998_G7_US.csv")

datab4_08 <- data[data$date < as.yearmon("Dec 2007", "%b %Y"),] 
dataAfter08 <- data[data$date >=  as.yearmon("Dec 2007", "%b %Y") &
                      data$date < as.yearmon("March 2020", "%b %Y"), ]

datab4_covid <- data[data$date >= as.yearmon("June 2009", "%b %Y") &
                       data$date < as.yearmon("March 2020", "%b %Y"),] 
dataAfterCovid <- data[data$date >=  as.yearmon("March 2020", "%b %Y"), ]
############################################################################
#  Functions:
############################################################################ 
## Partial Derivative Approximation through a finite difference approach:
partial_deriv_approx <- function(model, data, x_interest, delta) {
  data_plus <- data
  data_minus <- data
  
  data_plus[[x_interest]] <- data_plus[[x_interest]] + delta
  data_minus[[x_interest]] <- data_minus[[x_interest]] - delta
  
  # Get predictions
  y_plus <- predict(model, newdata = data_plus)
  y_minus <- predict(model, newdata = data_minus)
  
  # Compute finite difference approximation
  return(round((y_plus - y_minus) / (2 * delta), 10))
}

## Generate Autocorrelated (1) noise:
generate_ar1_noise <- function(n, phi = 0.9, sigma = 0.1) {
  # phi; how gradual the change of from period t to period t+1. The higher the
  # more gradual. 
  noise <- numeric(n)
  noise[1] <- rnorm(1, mean = 0, sd = sigma)
  
  for (t in 2:n) {
    noise[t] <- phi * noise[t - 1] + rnorm(1, mean = 0, sd = sigma)
  }
  
  return(noise)
}
############################################################################
#  Random Forests- Load in the different models
############################################################################ 
rf_model <- readRDS("Models/RF_model_ALL.rds")
RFmodel_b4_08 <- readRDS("Models/RF_model_b4_08.rds")
RFmodel_after_08 <- readRDS("Models/RFmodel_after_08.rds")
RFmodel_b4_covid <- readRDS("Models/RFmodel_b4_covid.rds")
RFmodel_after_covid <- readRDS("Models/RFmodel_after_covid.rds")
############################################################################
#  Evaluating Random Forest-generated Coefficients- Complete Model:
############################################################################ 
all_x_interest <- c("CA", "lag_GB", "FR", "JP") # "FR" to stand for "EU"
# Parameters:
delta_value = 1 

# Data:
data_noisy <- data[, -11] # All w/o US monetary policy
# Adding noise to all except the date column:
set.seed(123)
data_noisy[, -1] <- data_noisy[, -1] + rnorm(nrow(data_noisy), 
                                             mean = 0, sd = 0.01)

data_predict <- data[,-11] # Non-noisy data w/o US monetary policy

rf_all_comparison <- data.frame(data$date)

for(i in all_x_interest){
  rf_all_comparison[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
                                                  rf_model, data_predict,
                                                  i, delta_value)
  rf_all_comparison[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
                                                  rf_model, data_noisy,
                                                  i, delta_value)
}

# Test if there is statistical significant difference between the two sets of
# betas: 

# Loop through all_x_interest and perform normality test, then the 
# appropriate t-test
results <- list()

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison[[paste0("beta_", i, "_noisy")]]
  
  # Test for normality
  shapiro_test <- shapiro.test(beta_reg - beta_noisy)
  
  # Choose and run the appropriate test
  test_result <- if (shapiro_test$p.value > 0.05) {
    t.test(beta_reg, beta_noisy, paired = TRUE)  # Use t-test
  } else {
    wilcox.test(beta_reg, beta_noisy, paired = TRUE)  # Use Wilcoxon
  }
  
  # Store results
  results[[i]] <- list(shapiro_test = shapiro_test, test_result = test_result)
}

# Print results
print(results)

## CA: Reject H_{0} (there is no statistical significant difference)
#$CA$test_result; Wilcoxon signed rank test with continuity correction
# data:  beta_reg and beta_noisy
#V = 22160, p-value = 0.01716
#alternative hypothesis: true location shift is not equal to 0

## lag_GB: Do Not Reject H_{0} (there is no statistical significant difference)
# $lag_GB$test_result
# Wilcoxon signed rank test with continuity correction
# data:  beta_reg and beta_noisy
# V = 25600, p-value = 0.8844
# alternative hypothesis: true location shift is not equal to 0

## FR: Do Not Reject H_{0} (there is no statistical significant difference)
# $FR$test_result
# Wilcoxon signed rank test with continuity correction
# data:  beta_reg and beta_noisy
# V = 23818, p-value = 0.83
# alternative hypothesis: true location shift is not equal to 0

## JP: Reject H_{0} (there is no statistical significant difference)
# $JP$test_result
# Wilcoxon signed rank test with continuity correction
# data:  beta_reg and beta_noisy
# V = 26856, p-value = 0.00141
# alternative hypothesis: true location shift is not equal to 0

### CA & JP's coefficients fail the robustness check

############################################################################
#  Evaluating Random Forest-generated Coefficients- Before 2008:
############################################################################ 
# Data:
data_noisy_b408 <- datab4_08[, -11] # All w/o US monetary policy
# Adding noise to all except the date column:
data_noisy_b408[, -1] <- data_noisy_b408[, -1] + generate_ar1_noise(
  nrow(data_noisy_b408), phi = 0.9, sigma = 0.1)

data_predict_b408 <- datab4_08[,-11] # Non-noisy data w/o US monetary policy

rf_all_comparison_b408 <- data.frame(datab4_08$date)

for(i in all_x_interest){
  rf_all_comparison_b408[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
    RFmodel_b4_08, data_noisy_b408, i, delta_value)
  rf_all_comparison_b408[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
    RFmodel_b4_08, data_predict_b408, i, delta_value)
}

# To test whether the estimate of the betas stays consistent, we will run a 
# regression of noisy beta on regular beta

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison_b408[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison_b408[[paste0("beta_", i, "_noisy")]]
  

# Print results
print(results_b4_08)
############################################################################
#  Evaluating Random Forest-generated Coefficients- After 2008:
############################################################################ 

############################################################################
#  Evaluating Random Forest-generated Coefficients- Before COVID-19:
############################################################################

############################################################################
#  Evaluating Random Forest-generated Coefficients- After COVID-19:
############################################################################ 