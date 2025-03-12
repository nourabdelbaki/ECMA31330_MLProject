#     Nour Abdelbaki & Giuliana Triberti

library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(purrr)
library(randomForest)
library(ggplot2)
library(corrplot)
library(reshape2)
library(factoextra)
library(FactoMineR)
library(stringr)
library(lfe)  # For fixed-effects regression
library(DALEX) # SHAP 
library(pdp) # Partial Dependence Plot
set.seed(123)

## Set working directory
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject")

## Read dataset
data <- read.csv("1998_G7_US.csv")

#### Lessons learned from preliminary_analysis file (if any):
## ADD HERE!!!!!!!

############################################################################
#  K-means clustering (if doing it)
###########################################################################

### FIX IT!!! 


###### Random Forests ###### 
# ref: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
############################################################################
#  Random Forests- Entire Dataset 
###########################################################################
# Turning it back scale from 0-100 because it may help beta approximation
data[, -c(1,5,12, 13, 107, 201)] <- data[, -c(1,5,12, 13, 107, 201)] * 100

rf_model <- randomForest(US ~ ., data = data, importance = TRUE,
                         ntree = 1000, replace = TRUE, 
                         keep.forest = TRUE)

# C.V. mtry and ntree? # TO DO/FIX!!!!!!  

# Extract feature importance
importance_plot <- importance(rf_model)

# Convert to data frame for easier manipulation
importance_df <- data.frame(Feature = rownames(importance_plot),
                            Importance = importance_plot[, 1])

# Order by importance (descending)
importance_df <- importance_df[order(-importance_df$Importance), ]

# Select top 50 most important features
top_50 <- head(importance_df, 50)

# Plot
barplot(top_50$Importance, names.arg = top_50$Feature, 
        las = 2, col = "steelblue", main = "Top 50 Feature Importance", 
        cex.names = 0.7) # Reduce text size for readability

############################################################################
#  Random Forests- Entire Dataset- "Coefficient" calculation
###########################################################################
### Method 1: +/- delta, calculate difference in predicted y; 
# finite difference approximation approach of the partial derivative of y wrt x

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

## Testing it for CA  and GB 
# try different values for delta? somehow validate the choice?
delta_value_CA <- 1 # 0.01*sd(data$CA)
delta_value_GB <- 1 # 0.01*sd(data$lag_GB)
data_predict <- data[, -11] #remove US from the data
effect_CA <- partial_deriv_approx(rf_model, data_predict, "CA", delta_value_CA)
effect_GB <- partial_deriv_approx(rf_model, data_predict, "lag_GB", delta_value)

### Method 2: SHAP breakdown (not really betas but rather game theoretic contribution)
explainer <- explain(rf_model, data = data[,-1], #data w/o date column 
                     y = data$US, label = "Random Forest")

# SHAP values to interpret feature importance at a local level
shap_values <- predict_parts(explainer, new_observation = data[10,],
                             type = "break_down")
plot(shap_values)


### Method 3: Partial Dependence Plots (again not really betas but)

# Partial dependence for "CA"
pdp_hp <- partial(rf_model, pred.var = "CA", train = X) #what should we put for train
plotPartial(pdp_hp, 
      main = "Partial Dependence of US Monetary Policy on CA Monetary policy")

############################################################################
#  Random Forests- Before and After 2008 
###########################################################################

############################################################################
#  Random Forests- Before and After COVID-19
###########################################################################


