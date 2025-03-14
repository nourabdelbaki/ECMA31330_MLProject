#     Nour Abdelbaki & Giuliana Triberti

library(data.table)
library(dplyr)
library(tidyr)
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
#  Random Forests- Entire Dataset- Cross Validation 
###########################################################################
# Cross-validation: 
# Ref: https://bradleyboehmke.github.io/HOML/random-forest.html

p <- dim(data)[2] - 1 #Total number of features w/o US data
n <- dim(data)[1]

hyper_grid <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 10 MINS TO RUN!! 
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = data, ntree = p * 10, 
                      mtry = hyper_grid$mtry[i], 
                      nodesize = hyper_grid$nodesize[i],
                      replace = hyper_grid$replace[i],
                      sampsize = hyper_grid$sampsize[i]
  )
  # save rmse
  hyper_grid$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_all <- hyper_grid %>%
  arrange(rmse)

head(hyper_grid_all, 10)
#     mtry nodesize replace sampsize      rmse
#1    80       15   FALSE      259    0.3479939
#2    66       15    TRUE      259    0.4057546
#3    30        3    TRUE      162    0.4061437
#4    30        3   FALSE      259    0.4272611
############################################################################
#  Random Forests- Entire Dataset
###########################################################################
rf_model <- randomForest(US ~ ., data = data, importance = TRUE,
                         ntree = p*10,
                         mtry = 80,
                         nodesize = 15,
                         replace = FALSE,
                         sampsize = 80,
                         keep.forest = TRUE)

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

# Coefficients: 
delta_value <- 1 #Note here in writeup why this delta; o/w too small to detect

data_predict <- data[, -11] #remove US from the data

effect_CA <- partial_deriv_approx(rf_model, data_predict, "CA",
                                  delta_value)
effect_GB <- partial_deriv_approx(rf_model, data_predict, "lag_GB", 
                                  delta_value)
effect_EU <- partial_deriv_approx(rf_model, data_predict, "FR", 
                                  delta_value)
effect_JP <- partial_deriv_approx(rf_model, data_predict, "JP", 
                                  delta_value)

### Method 2: SHAP breakdown (not really betas but rather game theoretic contribution)
explainer <- explain(rf_model, data = data,
                     y = data$US, label = "Random Forest")

# SHAP values to interpret feature importance at a local level
# Different rows for new_observation give different values of breakdown
# but relatively stays the same in terms of order and somewhat magnitude
shap_values <- predict_parts(explainer, new_observation = data[320,],
                             type = "break_down")
plot(shap_values)


### Method 3: Partial Dependence Plots (again not really betas but)
# Ref: https://cran.r-project.org/web/packages/pdp/pdp.pdf 
# train is the dataset we used for training the rfmodel
# computation takes a long time, so suggested maximum 3 pred.var() for the partial()
## Takes around 10 mins to run as well:

# Show marginal effect of each monetary policy, holding the rest fixed:
pdp_rf_all <- partial(rf_model, pred.var = c("CA", "lag_GB", "FR"), train = data) 
plotPartial(pdp_rf_all, 
      main = "Partial Dependence of US Monetary Policy on Foreign Monetary policy")

# Individual impact; not very intuitive, but:
pdp_gb_rf_all <- partial(rf_model, pred.var = "lag_GB", train = data) 
plotPartial(pdp_gb_rf_all, 
            main = "Partial Dependence of US Monetary Policy on GB Monetary policy")
############################################################################
#  Random Forests- Before and After 2008- Cross Validation 
###########################################################################
# Datasets:
# Officially the Great Recession/2008 Financial Crisis lasted from Dec 2007
# to June 2009. We chose the after '08 dataset to also end before the 
# COVID-19 crisis. 
datab4_08 <- data[data$date < as.yearmon("Dec 2007", "%b %Y"),] 
dataAfter08 <- data[data$date >=  as.yearmon("Dec 2007", "%b %Y") &
                      data$date < as.yearmon("March 2020", "%b %Y"), ]

#### Cross-validation before 2008:
n_08 <- dim(datab4_08)[1]

hyper_grid_b4_08 <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_08* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 4 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_b4_08))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = datab4_08, ntree = p * 10, 
                      mtry = hyper_grid_b4_08$mtry[i], 
                      nodesize = hyper_grid_b4_08$nodesize[i],
                      replace = hyper_grid_b4_08$replace[i],
                      sampsize = hyper_grid_b4_08$sampsize[i]
  )
  # save rmse
  hyper_grid_b4_08$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_b4_08 <- hyper_grid_b4_08 %>%
  arrange(rmse)

head(hyper_grid_b4_08, 10)
#     mtry nodesize replace sampsize      rmse
#1    66        5   FALSE       95    0.2325632
#2    80       15   FALSE       74    0.2668251
#3    80        3   FALSE       95    0.3233085
#4    80       15    TRUE       95    0.3263221
#5    50        5   FALSE       95    0.3424027

#### Cross-validation after 2008:
n_08_2 <- dim(dataAfter08)[1]

hyper_grid_after_08 <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_08_2* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 6 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_after_08))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = dataAfter08, ntree = p * 10, 
                      mtry = hyper_grid_after_08$mtry[i], 
                      nodesize = hyper_grid_after_08$nodesize[i],
                      replace = hyper_grid_after_08$replace[i],
                      sampsize = hyper_grid_after_08$sampsize[i]
  )
  # save rmse
  hyper_grid_after_08$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_after_08 <- hyper_grid_after_08 %>%
  arrange(rmse)

head(hyper_grid_after_08, 10)
#     mtry nodesize replace sampsize      rmse
#1    30        5   FALSE      117    0.1070436
#2    66       10   FALSE       73    0.1078672
#3    10        5   FALSE      117    0.1894131
#4    50       10    TRUE      117    0.1940878
#5    50        5   FALSE       92    0.2224923
###########################################################################
#  Random Forests- Before and After 2008 
###########################################################################
####### Random Forest Before '08 
RFmodel_b4_08 <- randomForest(US ~ ., data = datab4_08, importance = TRUE,
                              ntree = p*10,
                              mtry = 66,
                              nodesize = 5,
                              replace = FALSE,
                              sampsize = 95,
                              keep.forest = TRUE)

# Extract feature importance
importance_plot2 <- importance(RFmodel_b4_08)

# Convert to data frame for easier manipulation
importance_df2 <- data.frame(Feature = rownames(importance_plot2),
                            Importance = importance_plot2[, 1])

# Order by importance (descending)
importance_df2 <- importance_df2[order(-importance_df2$Importance), ]

# Select top 50 most important features
top_50_2<- head(importance_df2, 50)

# Plot
barplot(top_50_2$Importance, names.arg = top_50_2$Feature, 
        las = 2, col = "steelblue",
        main = "Top 50 Feature Importance Before 2008", 
        cex.names = 0.7) # Reduce text size for readability

####### Random Forest After '08 
RFmodel_after_08 <- randomForest(US ~ ., data = dataAfter08, importance = TRUE,
                                 ntree = p*10,
                                 mtry = 30,
                                 nodesize = 5,
                                 replace = FALSE,
                                 sampsize = 117,
                                 keep.forest = TRUE)

# Extract feature importance
importance_plot3<- importance(RFmodel_after_08)

# Convert to data frame for easier manipulation
importance_df3 <- data.frame(Feature = rownames(importance_plot3),
                             Importance = importance_plot3[, 1])

# Order by importance (descending)
importance_df3 <- importance_df3[order(-importance_df3$Importance), ]

# Select top 50 most important features
top_50_3 <- head(importance_df3, 50)

# Plot
barplot(top_50_3$Importance, names.arg = top_50_3$Feature, 
        las = 2, col = "steelblue",
        main = "Top 50 Feature Importance After 2008", 
        cex.names = 0.7) # Reduce text size for readability

### Merged Plot: 

# Merge both sets to align matching features
merged_importance_08 <- merge(top_50_3, top_50_2, by = "Feature",
                                 all = TRUE, suffixes = c("_After", "_Before"))

# Replace NA with 0 for features that are missing in one of the sets
merged_importance_08[is.na(merged_importance_08)] <- 0

# Sort by maximum importance in either dataset to keep the order meaningful
merged_importance_08 <- merged_importance_08[order(
  -pmax(merged_importance_08$Importance_After,
        merged_importance_08$Importance_Before)), ]

long_importance_08 <- merged_importance_08 %>%
  pivot_longer(cols = c(Importance_After, Importance_Before), 
               names_to = "Dataset", 
               values_to = "Importance") %>%
  mutate(Dataset = recode(Dataset, 
                          "Importance_After" = "After 2008", 
                          "Importance_Before" = "Before 2008"))

# Plot using long format data
ggplot(long_importance_08, aes(x = reorder(Feature, Importance), 
                            y = Importance, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Before 2008" = "darkorange",
                               "After 2008" = "steelblue")) +
  labs(title = "Comparison of Feature Importance Before and After 2008",
       x = "Feature",
       y = "Importance",
       fill = "Dataset") +
  coord_flip() +  # Flip for better readability
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Reduce text size for readability

############################################################################
#  Random Forests- Before/After '08- "Coefficient" calculation
###########################################################################
# Coefficients before '08:
data_pred_b4_08 <- datab4_08[, -11] #remove US from the data

effect_CA_b4_08 <- partial_deriv_approx(rf_model, data_pred_b4_08, "CA",
                                  delta_value)
effect_GB_b4_08 <- partial_deriv_approx(rf_model, data_pred_b4_08, "lag_GB", 
                                  delta_value)
effect_EU_b4_08 <- partial_deriv_approx(rf_model, data_pred_b4_08, "FR", 
                                  delta_value)
effect_JP_b4_08 <- partial_deriv_approx(rf_model, data_pred_b4_08, "JP", 
                                  delta_value)

# Coefficients after '08:
data_pred_after_08 <- dataAfter08[, -11] #remove US from the data

effect_CA_after_08 <- partial_deriv_approx(rf_model, data_pred_after_08, "CA",
                                        delta_value)
effect_GB_after_08 <- partial_deriv_approx(rf_model, data_pred_after_08, "lag_GB", 
                                        delta_value)
effect_EU_after_08 <- partial_deriv_approx(rf_model, data_pred_after_08, "FR", 
                                        delta_value)
effect_JP_after_08 <- partial_deriv_approx(rf_model, data_pred_after_08, "JP", 
                                        delta_value)

############################################################################
#  Random Forests- Before and After COVID-19- Cross Validation 
###########################################################################
# Datasets:
# Officially the Great Recession/2008 Financial Crisis lasted from Dec 2007
# to June 2009. We chose the before covid dataset to be from after June 2009 
# to before March 2020. And after to be from March 2020 onwards. 
datab4_covid <- data[data$date >= as.yearmon("June 2009", "%b %Y") &
                       data$date < as.yearmon("March 2020", "%b %Y"),] 
dataAfterCovid <- data[data$date >=  as.yearmon("March 2020", "%b %Y"), ]

#### Cross-validation before COVID-19:
n_covid <- dim(datab4_covid)[1]

hyper_grid_b4_covid <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_covid* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 5 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_b4_covid))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = datab4_covid, ntree = p * 10, 
                      mtry = hyper_grid_b4_covid$mtry[i], 
                      nodesize = hyper_grid_b4_covid$nodesize[i],
                      replace = hyper_grid_b4_covid$replace[i],
                      sampsize = hyper_grid_b4_covid$sampsize[i]
  )
  # save rmse
  hyper_grid_b4_covid$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_b4_covid <- hyper_grid_b4_covid %>%
  arrange(rmse)

head(hyper_grid_b4_covid, 10)
#     mtry nodesize replace sampsize      rmse
#1    10        5   FALSE      103      0.02451452
#2    80        5   FALSE      103      0.08492078
#3    66        5    TRUE       64      0.09494432
#4    66        5   FALSE       81      0.09547033
#5    80       10   FALSE      103      0.10071857

#### Cross-validation after COVID-19:
n_covid_2 <- dim(dataAfterCovid)[1]

hyper_grid_after_covid <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_covid_2* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 2 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_after_covid))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = dataAfterCovid, ntree = p * 10, 
                      mtry = hyper_grid_after_covid$mtry[i], 
                      nodesize = hyper_grid_after_covid$nodesize[i],
                      replace = hyper_grid_after_covid$replace[i],
                      sampsize = hyper_grid_after_covid$sampsize[i]
  )
  # save rmse
  hyper_grid_after_covid$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_after_covid <- hyper_grid_after_covid %>%
  arrange(rmse)

head(hyper_grid_after_covid, 10)
#     mtry nodesize replace sampsize      rmse
#1    80        5   FALSE       46    0.05705443
#2    10       15   FALSE       36    0.13846380
#3    80        3   FALSE       36    0.17699976
#4    10        3   FALSE       46    0.20728905
#5    66       15   FALSE       46    0.20956426
############################################################################
#  Random Forests- Before and After COVID-19
###########################################################################
####### Random Forest Before COVID-19
RFmodel_b4_covid <- randomForest(US ~ .,
                                 data = datab4_covid, importance = TRUE,
                                 ntree = p*10,
                                 mtry = 10,
                                 nodesize = 5,
                                 replace = FALSE,
                                 sampsize = 103,
                                 keep.forest = TRUE)

# Extract feature importance
importance_plot4<- importance(RFmodel_b4_covid)

# Convert to data frame for easier manipulation
importance_df4 <- data.frame(Feature = rownames(importance_plot4),
                             Importance = importance_plot4[, 1])

# Order by importance (descending)
importance_df4 <- importance_df4[order(-importance_df4$Importance), ]

# Select top 50 most important features
top_50_4 <- head(importance_df4, 50)

# Plot
barplot(top_50_4$Importance, names.arg = top_50_5$Feature, 
        las = 2, col = "steelblue",
        main = "Top 50 Feature Importance Before COVID-19", 
        cex.names = 0.7) # Reduce text size for readability


RFmodel_after_covid <- randomForest(US ~ .,
                                    data = dataAfterCovid, importance = TRUE,
                                    ntree = p*10,
                                    mtry = 80,
                                    nodesize = 5,
                                    replace = FALSE,
                                    sampsize = 46,
                                    keep.forest = TRUE)

# Extract feature importance
importance_plot5<- importance(RFmodel_after_covid)

# Convert to data frame for easier manipulation
importance_df5 <- data.frame(Feature = rownames(importance_plot5),
                             Importance = importance_plot5[, 1])

# Order by importance (descending)
importance_df5 <- importance_df5[order(-importance_df5$Importance), ]

# Select top 50 most important features
top_50_5 <- head(importance_df5, 50)

# Plot
barplot(top_50_5$Importance, names.arg = top_50_5$Feature, 
        las = 2, col = "steelblue",
        main = "Top 50 Feature Importance After COVID-19", 
        cex.names = 0.7) # Reduce text size for readability

#### Merged Plot: 
# Plotting both together for a more intuitive feel of what's happening before
# and after a crisis:

# Merge both sets to align matching features
merged_importance_covid <- merge(top_50_5, top_50_4, by = "Feature",
                                 all = TRUE, suffixes = c("_After", "_Before"))

# Replace NA with 0 for features that are missing in one of the sets
merged_importance_covid[is.na(merged_importance_covid)] <- 0

# Sort by maximum importance in either dataset to keep the order meaningful
merged_importance_covid <- merged_importance_covid[order(
  -pmax(merged_importance_covid$Importance_After,
        merged_importance_covid$Importance_Before)), ]

long_importance <- merged_importance_covid %>%
  pivot_longer(cols = c(Importance_After, Importance_Before), 
               names_to = "Dataset", 
               values_to = "Importance") %>%
  mutate(Dataset = recode(Dataset, 
                          "Importance_After" = "After COVID-19", 
                          "Importance_Before" = "Before COVID-19"))

# Plot using long format data
ggplot(long_importance, aes(x = reorder(Feature, Importance), 
                            y = Importance, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Before COVID-19" = "darkorange",
                               "After COVID-19" = "steelblue")) +
  labs(title = "Comparison of Feature Importance Before and After COVID-19",
       x = "Feature",
       y = "Importance",
       fill = "Dataset") +
  coord_flip() +  # Flip for better readability
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Reduce text size for readability
############################################################################
#  Random Forests- Before/After COVID-19- "Coefficient" calculation
###########################################################################
# Coefficients before COVID-19:
data_pred_b4_covid <- datab4_covid[, -11] #remove US from the data

effect_CA_b4_covid <- partial_deriv_approx(rf_model, data_pred_b4_covid, "CA",
                                        delta_value)
effect_GB_b4_covid <- partial_deriv_approx(rf_model, data_pred_b4_covid, "lag_GB", 
                                        delta_value)
effect_EU_b4_covid <- partial_deriv_approx(rf_model, data_pred_b4_covid, "FR", 
                                        delta_value)
effect_JP_b4_covid <- partial_deriv_approx(rf_model, data_pred_b4_covid, "JP", 
                                        delta_value)

# Coefficients after COVID-19:
data_pred_after_covid <- dataAfterCovid[, -11] #remove US from the data

effect_CA_after_covid <- partial_deriv_approx(rf_model, data_pred_after_covid,
                                              "CA", delta_value)
effect_GB_after_covid <- partial_deriv_approx(rf_model, data_pred_after_covid, 
                                              "lag_GB", delta_value)
effect_EU_after_covid <- partial_deriv_approx(rf_model, data_pred_after_covid,
                                              "FR", delta_value)
effect_JP_after_covid <- partial_deriv_approx(rf_model, data_pred_after_covid,
                                              "JP", delta_value)
############################################################################
#  Linear Regression-Before and After 2008
###########################################################################

############################################################################
#  Linear Regression-Before and After Covid-19
###########################################################################
