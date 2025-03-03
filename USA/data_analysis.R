#     Nour Abdelbaki & Giuliana Triberti


library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(purrr)

#Set path
setwd("C:/Users/HP/Downloads/UChicago/1. Courses/2. Winter Quarter 2025/2.3 MACSS 31330 Econometrics and Machine Learning/Final project/database/USA")

# Data cleaning

data <- fread("merged_data.csv", na.strings = "NA")

# Drop columns that are completely NA (some inflation sectors do not have values
# or are discontinued)
data_cleaned <- data[397:925,]
data_cleaned <- data[, Filter(function(x) any(!is.na(x)), .SD)]

# Imputed database (filling NAs with the mean of observations)
mat_data <- as.matrix(data_cleaned[,2:117])  # Exclude date column and lags
mat_data_imputed <- mat_data
mat_data_imputed[is.na(mat_data_imputed)] <- apply(mat_data_imputed, 2, mean, na.rm = TRUE)


###############################
# Analysis before and after pandemic (5 years before, 5 years after)

### OLS Before pandemic 5 year
possible_regressors <- data_cleaned %>% select(-US)
possible_regressors_pre <- possible_regressors[807:866,]
N <- nrow(data_cleaned)
k <- ncol(possible_regressors[,2:ncol(possible_regressors)])
S <- 50 #we start with 50 simulations

#function to run ols regression
ols_simulation <- function(data_cleaned, k, S, N){
  #betaj and R^2 under this model
  bj <- 0
  R_sq <- 0
  
  #best beta and j under each subset with higher R_squared
  bj_hat <- 0
  j_hat <- 0
  
  #store values for verification
  betasj <- matrix(NA, nrow=S, ncol=k)
  Rs <- matrix(NA, nrow=S, ncol=k)
  colnames(Rs) = colnames(possible_regressors[,2:ncol(possible_regressors)])
  
  # these are given SELECT PREPANDEMIC ROWS
  xs <- as.matrix(possible_regressors[,2:ncol(possible_regressors)])
  # gamma is the true value so I don't need a function for that anymore
  # SELECT PREPANDEMIC ROWS
  y <- data_cleaned %>% select(US)
  y <- y[807:866,]
  
  
  for (j in 1:k){
    for (i in 1:S){
      
      ols <- lm(y$US ~ 0 + xs[, j], na.action = na.exclude) #0 to remove intercept 
      
      #we take the only coefficient in the ols
      bj <- coef(ols)[1]
      
      #we also take the R squared of each simulation
      sum <- summary(ols)
      R_possible <- sum$r.squared
      js <- numeric(S)
      
      #storing the values
      betasj[i, j] <- bj
      Rs[i, j] <- R_possible
      
      if (R_possible > R_sq){
        R_sq <- R_possible
        bj_hat <- bj
        j_hat <- j
      }
    }
  }
  return(list(j_hat, bj_hat, R_sq, betasj, Rs))
}

result <- ols_simulation(data_cleaned, k, S, N)

best_reg <- result[[1]]
best_betaj <- result[[2]]
R2_best <- result[[3]]
Rs <- result[[5]]

print(best_reg)

colnames(Rs)[best_reg] #"inf_157"



