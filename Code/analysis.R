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



############################################################################
#  Random Forests- Entire Dataset
###########################################################################
rf_model <- randomForest(US ~ ., data = data, importance = TRUE,
                         ntree = 2000, replace = TRUE, 
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
explainer <- explain(rf_model, data = data, #data w/o date column 
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

# Show marginal effect of each monetary policy, holding the rest fixed:
pdp_ca <- partial(rf_model, pred.var = c("CA", "lag_GB", "FR"), train = data) 
plotPartial(pdp_ca, 
      main = "Partial Dependence of US Monetary Policy on Foreign Monetary policy")

# Individual impact; not very intuitive, but:
pdp_gb <- partial(rf_model, pred.var = "lag_GB", train = data) 
plotPartial(pdp_gb, 
            main = "Partial Dependence of US Monetary Policy on GB Monetary policy")
############################################################################
#  Random Forests- Before and After 2008 
###########################################################################
datab4_08 <- data[data$date < as.yearmon("Jan 2008", "%b %Y"),]
dataAfter08 <- data[!data$date < as.yearmon("Jan 2008", "%b %Y"),]

RFmodel_b4_08 <- randomForest(US ~ ., data = datab4_08, importance = TRUE,
                         ntree = 1000, replace = TRUE, 
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

RFmodel_after_08 <- randomForest(US ~ ., data = dataAfter08, importance = TRUE,
                                 ntree = 1000, replace = TRUE, 
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
  scale_fill_manual(values = c("After 2008" = "steelblue", 
                               "Before 2008" = "darkorange")) +
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
#  Random Forests- Before and After COVID-19
###########################################################################
datab4_covid <- data[data$date < as.yearmon("March 2020", "%b %Y"),]
dataAfterCovid <- data[!data$date < as.yearmon("March 2020", "%b %Y"),]


RFmodel_b4_covid <- randomForest(US ~ ., data = datab4_covid, importance = TRUE,
                                 ntree = 1000, replace = TRUE, 
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


RFmodel_after_covid <- randomForest(US ~ ., data = dataAfterCovid,
                                    importance = TRUE,
                                    ntree = 1000, replace = TRUE, 
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

####
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
  scale_fill_manual(values = c("After COVID-19" = "steelblue", 
                               "Before COVID-19" = "darkorange")) +
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
