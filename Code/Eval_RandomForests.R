#     Nour Abdelbaki & Giuliana Triberti
## In this code, we are evaluating our random forests' models. We are using 
# random forests as a flexible way to get at the effect external monetary policy
# has on the US monetary policy decisions. We then use our model to recover 
# coefficients using the finite difference approach. To test whether our recovered
# betas (that come from the random forests) are robust against small noise, we
# will introduce some random noise and recover the betas again. Compare the betas
# recovered from the original data and the noisy data and see if there is a 
# statistical difference. 

# Test if there the estimate of the betas stays stable/consistent even with
# the added noise, we will run a simple OLS regression of the noisy beta on 
# the beta derived from the data:
# if the intercept is close to zero, slope is close to 1, and the R^{2} is very 
# high (>0.8); it means that the model is not sensitive to noise.
# So, we are more confident the beta that's picked up is not one that's based
# on noise. 

# It is worth noting we tried adding completely normal noise with mean = 0, 
# sd = 0.1, and adding Autocorrelated(1) (AR1) noise with persistence of 0.5, they both
# give the same end result, but we preferred the AR1 noise because
# it makes more sense with time-series data. 

# Import Libraries
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(purrr)
library(knitr)
library(kableExtra)

set.seed(123)

## Set working directory
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject")

## Read dataset
data <- read.csv("1998_G7_US.csv")
data <- data %>%
  mutate(EU = rowMeans(select(., DE, FR, IT))) %>%
  select(-c(DE, FR, IT))

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
all_x_interest <- c("CA", "lag_GB", "EU", "JP")
# Parameters:
delta_value = 1 

# Data:
data_noisy <- data[, -8] # All w/o US monetary policy
# Adding noise to all except the date column:
set.seed(123)
data_noisy[, -1] <- data_noisy[, -1] + generate_ar1_noise(nrow(data_noisy),
                                                          phi = 0.5, sigma= 0.1)
#data_noisy[, -1] <- data_noisy[, -1] + rnorm(nrow(data_noisy), 
#                                             mean = 0, sd = 0.1)

data_predict <- data[,-8] # Non-noisy data w/o US monetary policy

rf_all_comparison <- data.frame(data$date)

for(i in all_x_interest){
  rf_all_comparison[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
                                                  rf_model, data_predict,
                                                  i, delta_value)
  rf_all_comparison[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
                                                  rf_model, data_noisy,
                                                  i, delta_value)
}

results <- list()

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison[[paste0("beta_", i, "_noisy")]]
  
  # Run OLS Regression to Check Stability
  lm_stability <- lm(beta_noisy ~ beta_reg)
  summary_lm <- summary(lm_stability)
  #print(summary_lm); all slopes are statistical significant 
  
  # Extract metrics
  intercept <- coef(summary_lm)[1, 1]  # Intercept estimate
  slope <- coef(summary_lm)[2, 1]      # Slope estimate
  r_squared <- summary_lm$r.squared    # R-squared value
  
  # Check stability conditions
  if (abs(round(intercept)) == 0 & abs(round(slope)) == 1 & r_squared >= 0.8) {
    status <- "stable"
  } else {
    status <- "unstable"
  }
  
  # Store results
  results[[i]] <- list(intercept = intercept, slope = slope,
                       r_squared = r_squared, status = status)
}

# Convert results into dataframe
results_df <- do.call(rbind, lapply(names(results), function(i) {
  cbind(Country = i, as.data.frame(t(results[[i]])))
})) %>%
  mutate(across(c(intercept, slope, r_squared), as.numeric)) 

# Generate LaTeX table
stargazer(results_df, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Regression Stability Test Results",
          digits = 3, align = TRUE,
          column.labels = c("Country", "Intercept", "Slope", "R^{2}", "Status"))
############################################################################
#  Evaluating Random Forest-generated Coefficients- Before 2008:
############################################################################ 
# Data:
data_noisy_b408 <- datab4_08[, -8] # All w/o US monetary policy

# Adding noise to all except the date column:
set.seed(123)
data_noisy_b408[, -1] <- data_noisy_b408[, -1] + generate_ar1_noise(
  nrow(data_noisy_b408), phi = 0.5, sigma = 0.1)

data_predict_b408 <- datab4_08[,-8] # Non-noisy data w/o US monetary policy

rf_all_comparison_b408 <- data.frame(datab4_08$date)

for(i in all_x_interest){
  rf_all_comparison_b408[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
    RFmodel_b4_08, data_predict_b408, i, delta_value)
  rf_all_comparison_b408[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
    RFmodel_b4_08, data_noisy_b408, i, delta_value)
}

results_b4_08 <- list()

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison_b408[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison_b408[[paste0("beta_", i, "_noisy")]]
  
  # Run OLS Regression to Check Stability
  lm_stability <- lm(beta_noisy ~ beta_reg)
  summary_lm <- summary(lm_stability)
  #print(summary_lm); all slopes are statistical significant 
  
  # Extract metrics
  intercept <- coef(summary_lm)[1, 1]  # Intercept estimate
  slope <- coef(summary_lm)[2, 1]      # Slope estimate
  r_squared <- summary_lm$r.squared    # R-squared value
  
  # Check stability conditions
  if (abs(round(intercept)) == 0 & abs(round(slope)) == 1 & r_squared >= 0.8) {
    status <- "stable"
  } else {
    status <- "unstable"
  }
  
  # Store results
  results_b4_08[[i]] <- list(intercept = intercept, slope = slope,
                       r_squared = r_squared, status = status)
}

# Convert results into dataframe
results_b4_08_df <- do.call(rbind, lapply(names(results_b4_08), function(i) {
  cbind(Country = i, as.data.frame(t(results_b4_08[[i]])))
})) %>%
  mutate(across(c(intercept, slope, r_squared), as.numeric)) 

# Generate LaTeX table
stargazer(results_b4_08_df, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Stability Test Results: Before 2008",
          digits = 3, align = TRUE,
          column.labels = c("Country", "Intercept", "Slope", "R^{2}", "Status"))
############################################################################
#  Evaluating Random Forest-generated Coefficients- After 2008:
############################################################################ 
# Data:
data_noisy_after_08 <- dataAfter08[, -8] # All w/o US monetary policy

# Adding noise to all except the date column:
set.seed(123)
data_noisy_after_08[, -1] <- data_noisy_after_08[, -1] + generate_ar1_noise(
  nrow(data_noisy_after_08), phi = 0.5, sigma = 0.1)

data_predict_After08 <- dataAfter08[,-8] # Non-noisy data w/o US monetary policy

rf_all_comparison_after_08 <- data.frame(dataAfter08$date)

for(i in all_x_interest){
  rf_all_comparison_after_08[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
    RFmodel_after_08, data_predict_After08, i, delta_value)
  rf_all_comparison_after_08[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
    RFmodel_after_08, data_noisy_after_08, i, delta_value)
}

results_after_08 <- list()

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison_after_08[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison_after_08[[paste0("beta_", i, "_noisy")]]
  
  # Run OLS Regression to Check Stability
  lm_stability <- lm(beta_noisy ~ beta_reg)
  summary_lm <- summary(lm_stability)
  #print(summary_lm); all slopes are statistical significant 
  
  # Extract metrics
  intercept <- coef(summary_lm)[1, 1]  # Intercept estimate
  slope <- coef(summary_lm)[2, 1]      # Slope estimate
  r_squared <- summary_lm$r.squared    # R-squared value
  
  # Check stability conditions
  if (abs(round(intercept)) == 0 & abs(round(slope)) == 1 & r_squared >= 0.8) {
    status <- "stable"
  } else {
    status <- "unstable"
  }
  
  # Store results
  results_after_08[[i]] <- list(intercept = intercept, slope = slope,
                             r_squared = r_squared, status = status)
}

# Convert results into dataframe
results_after_08_df <- do.call(rbind, lapply(names(results_after_08), function(i) {
  cbind(Country = i, as.data.frame(t(results_after_08[[i]])))
})) %>%
  mutate(across(c(intercept, slope, r_squared), as.numeric)) 

# Generate LaTeX table
stargazer(results_after_08_df, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Stability Test Results: After 2008",
          digits = 3, align = TRUE,
          column.labels = c("Country", "Intercept", "Slope", "R^{2}", "Status"))
############################################################################
#  Evaluating Random Forest-generated Coefficients- Before COVID-19:
############################################################################
# Data:
data_noisy_b4Covid <- datab4_covid[, -8] # All w/o US monetary policy

# Adding noise to all except the date column:
set.seed(123)
data_noisy_b4Covid[, -1] <- data_noisy_b4Covid[, -1] + generate_ar1_noise(
  nrow(data_noisy_b4Covid), phi = 0.5, sigma = 0.1)

data_predict_b4Covid <- datab4_covid[,-8] # Non-noisy data w/o US monetary policy

rf_all_comparison_before_covid <- data.frame(datab4_covid$date)

for(i in all_x_interest){
  rf_all_comparison_before_covid[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
    RFmodel_b4_covid, data_predict_b4Covid, i, delta_value)
  rf_all_comparison_before_covid[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
    RFmodel_b4_covid, data_noisy_b4Covid, i, delta_value)
}

results_before_covid <- list()

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison_before_covid[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison_before_covid[[paste0("beta_", i, "_noisy")]]
  
  # Run OLS Regression to Check Stability
  lm_stability <- lm(beta_noisy ~ beta_reg)
  summary_lm <- summary(lm_stability)
  #print(summary_lm); all slopes are statistical significant 
  
  # Extract metrics
  intercept <- coef(summary_lm)[1, 1]  # Intercept estimate
  slope <- coef(summary_lm)[2, 1]      # Slope estimate
  r_squared <- summary_lm$r.squared    # R-squared value
  
  # Check stability conditions
  if (abs(round(intercept)) == 0 & abs(round(slope)) == 1 & r_squared >= 0.8) {
    status <- "stable"
  } else {
    status <- "unstable"
  }
  
  # Store results
  results_before_covid[[i]] <- list(intercept = intercept, slope = slope,
                                r_squared = r_squared, status = status)
}

# Convert results into dataframe
results_before_covid_df <- do.call(rbind, lapply(names(results_before_covid),
                                                 function(i) {
  cbind(Country = i, as.data.frame(t(results_before_covid[[i]])))
})) %>%
  mutate(across(c(intercept, slope, r_squared), as.numeric)) 

# Generate LaTeX table
stargazer(results_before_covid_df, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Stability Test Results: Before COVID-19",
          digits = 3, align = TRUE,
          column.labels = c("Country", "Intercept", "Slope", "R^{2}", "Status"))
############################################################################
#  Evaluating Random Forest-generated Coefficients- After COVID-19:
############################################################################ 
# Data:
data_noisy_AfterCovid <- dataAfterCovid[, -8] # All w/o US monetary policy

# Adding noise to all except the date column:
set.seed(123)
data_noisy_AfterCovid[, -1] <- data_noisy_AfterCovid[, -1] + generate_ar1_noise(
  nrow(data_noisy_AfterCovid), phi = 0.5, sigma = 0.1)

data_predict_AfterCovid <- dataAfterCovid[,-8] # Non-noisy data w/o US monetary policy

rf_all_comparison_after_covid <- data.frame(dataAfterCovid$date)

for(i in all_x_interest){
  rf_all_comparison_after_covid[[paste0("beta_", i, "_reg")]] <- partial_deriv_approx(
    RFmodel_after_covid, data_predict_AfterCovid, i, delta_value)
  rf_all_comparison_after_covid[[paste0("beta_", i, "_noisy")]] <- partial_deriv_approx(
    RFmodel_after_covid, data_noisy_AfterCovid, i, delta_value)
}

results_after_covid <- list()

for (i in all_x_interest) {
  # Extract the generated columns
  beta_reg <- rf_all_comparison_after_covid[[paste0("beta_", i, "_reg")]]
  beta_noisy <- rf_all_comparison_after_covid[[paste0("beta_", i, "_noisy")]]
  
  # Run OLS Regression to Check Stability
  lm_stability <- lm(beta_noisy ~ beta_reg)
  summary_lm <- summary(lm_stability)
  #print(summary_lm); all slopes are statistical significant 
  
  # Extract metrics
  intercept <- coef(summary_lm)[1, 1]  # Intercept estimate
  slope <- coef(summary_lm)[2, 1]      # Slope estimate
  r_squared <- summary_lm$r.squared    # R-squared value
  
  # Check stability conditions
  if (abs(round(intercept)) == 0 & abs(round(slope)) == 1 & r_squared >= 0.8) {
    status <- "stable"
  } else {
    status <- "unstable"
  }
  
  # Store results
  results_after_covid[[i]] <- list(intercept = intercept, slope = slope,
                                    r_squared = r_squared, status = status)
}

# Convert results into dataframe
results_after_covid_df <- do.call(rbind, lapply(names(results_after_covid),
                                                 function(i) {
          cbind(Country = i, as.data.frame(t(results_after_covid[[i]])))})) %>%
  mutate(across(c(intercept, slope, r_squared), as.numeric)) 

# Generate LaTeX table
stargazer(results_after_covid_df, type = "latex", summary = FALSE, rownames = FALSE,
          title = "Stability Test Results: After COVID-19",
          digits = 3, align = TRUE,
          column.labels = c("Country", "Intercept", "Slope", "R^{2}", "Status"))