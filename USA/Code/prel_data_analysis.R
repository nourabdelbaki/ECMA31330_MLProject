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


#### 
#setwd('C:/Users/HP/Downloads/UChicago/1. Courses/2. Winter Quarter 2025/2.3 MACSS 31330 Econometrics and Machine Learning/ECMA31330_MLProject/USA')
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject/USA")

# USING DATA "1998_G7_US"
data <- read.csv("1998_G7_US.csv")
###################################################
# Running OLS where y = monetary policy USA

possible_regressors <- data %>% select(-US)
N <- nrow(data)
k <- ncol(possible_regressors[,2:ncol(possible_regressors)])
S <- 50 #we start with 50 simulations

#function to run ols regression
ols_simulation <- function(data, k, S, N){
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
  
  #these are given
  xs <- as.matrix(possible_regressors[,2:ncol(possible_regressors)])
  #gamma is the true value so I don't need a function for that anymore
  y <- data %>% select(US)
  
  
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

result <- ols_simulation(data, k, S, N)

best_reg <- result[[1]]
best_betaj <- result[[2]]
R2_best <- result[[3]]
Rs <- result[[5]]

print(best_reg)

colnames(Rs)[best_reg] #"CA"


# Plots to understand the data

summary(data)

# The problem with OLS is that we have high dimensional data. My number of
# predictors is large compared to N, leading to overfitting. To get a 
# subset of best predictors, I would need to run many regressions--> Sign to use PCA
# to reduce dimensionality and retain most information.

# Another problem of our data is that it may have implicit interactions between
# regressors that we don't know about. --> sign to use Random Forests. 


#Descriptive plots to understand the data

###################################### 
## Correlation of monetary policies with other variables

# Compute correlation matrix for all variables
mat_data <- as.matrix(data[,-1])  # Exclude date column

cor_matrix <- cor(mat_data, use = "complete.obs")  # Full correlation matrix

# Extract correlations for inflation variables
selected_vars <- colnames(mat_data)[5:11]  # Get variable names for columns 6-12 (EXTERNAL MONETARY POLICIES)
cor_subset <- cor_matrix[selected_vars, ]  # Keep all columns, but only rows from selected variables

# Convert to long format
cor_df <- melt(cor_subset)
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

# Convert factors to character to avoid level mismatch
cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)

# Remove self-correlations (Var1 == Var2)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]

# Ensure each pair appears only once
cor_df <- cor_df %>%
  mutate(pair = pmin(Var1, Var2)) %>%
  mutate(pair = paste(pair, pmax(Var1, Var2), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE)

# Select top 50 absolute correlations
top_cor <- cor_df[order(-abs(cor_df$Correlation)), ][1:50, ]

# Plot the highest 50 correlations
ggplot(top_cor, aes(x = reorder(paste(Var1, Var2, sep = " - "), abs(Correlation)), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 50 Correlations of Selected Variables with All Variables", x = "Variable Pair", y = "Correlation")


###################################### 
## Correlation of inflation with other inflation variables

# Extract correlations for inflation variables
selected_vars <- colnames(mat_data)[14:105]  # Get variable names for columns 6-12 (EXTERNAL MONETARY POLICIES)
cor_subset <- cor_matrix[selected_vars, ]  # Keep all columns, but only rows from selected variables

# Convert to long format
cor_df <- melt(cor_subset)
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

# Convert factors to character to avoid level mismatch
cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)

# Remove self-correlations (Var1 == Var2)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]

# Ensure each pair appears only once
cor_df <- cor_df %>%
  mutate(pair = pmin(Var1, Var2)) %>%
  mutate(pair = paste(pair, pmax(Var1, Var2), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE)

# Select top 50 absolute correlations
top_cor <- cor_df[order(-abs(cor_df$Correlation)), ][1:50, ]

# Plot the highest 50 correlations
ggplot(top_cor, aes(x = reorder(paste(Var1, Var2, sep = " - "), abs(Correlation)), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 50 Correlations of Selected Variables with All Variables", x = "Variable Pair", y = "Correlation")



#################################### PCA


# Standardize the data
data_scaled <- scale(mat_data)

# Step 1: Extract unique variable names from the top correlations
#top_vars <- unique(c(top_cor$Var1, top_cor$Var2))

# Step 2: Subset the original data to include only the most relevant variables
#selected_data <- mat_data_imputed[, colnames(mat_data_imputed) %in% top_vars]

# Step 3: Perform PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Step 4: Visualize PCA loadings (only the most relevant variables)
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("blue", "red"),
             repel = TRUE, title = "PCA Loadings of Top Correlated Variables")

# Rearrange the results of PCA so we have the variables and their loadings 
loading_scores <- data.frame(pca_result$rotation)
loading_scores$Variables <- rownames(loading_scores)

# Order the loadings according to first factor
loading_scores_1 <- loading_scores[order(-abs(loading_scores$PC1)), ]
# Top 5 variables loading into the first factor
top5_PC1 <- loading_scores_1[1:15, ]

# Order the loadings according to second factor
loading_scores_2 <- loading_scores[order(-abs(loading_scores$PC2)), ]
# Top 5 variables loading into the second factor
top5_PC2 <- loading_scores_2[1:15, ]

######

# Add names to the scores data frame
scores <- data.frame(pca_result$x)
scores$component <- data$US

# Variance of the scores for each principal component
variance_scores <- apply(scores[, -ncol(scores)], 2, var)  # Exclude the 'University' column
variance_scores

# Calculate the proportion of variance explained by each principal component
pve <- variance_scores / sum(variance_scores) *100
pve


# The principal components do not explain the variance is not an option


############################################
## Important features through random forests

# Fit the Random Forest model (target variable is 'US')
rf_model <- randomForest(US ~ ., data = mat_data, importance = TRUE)

# Extract feature importance. Importance score= Mean Decrease in Accuracy
importance_plot <- importance(rf_model)

# Convert to data frame for easier manipulation
importance_df <- data.frame(Feature = rownames(importance_plot), Importance = importance_plot[, 1])

# Order by importance (descending)
importance_df <- importance_df[order(-importance_df$Importance), ]

# Select top 50 most important features
top_50 <- head(importance_df, 50)

# Plot
barplot(top_50$Importance, names.arg = top_50$Feature, 
        las = 2, col = "steelblue", main = "Top 50 Feature Importance", 
        cex.names = 0.7) # Reduce text size for readability




#####################################################
### Usual order of monetary policy decisions

# 1. France (before)
# 2. Italy (before)
# 3. Canada (before)
# 4. Japan (before)
# 5. United States
# 6. Great Britain (after)
# 7. Denmark (after)

# Run the random forest using t-1 when the monetary policy decision
# ocurred after the US and t if it ocurred before. 

# Convert mat_data to a data frame
mat_data <- as.data.frame(mat_data)

# Variables to lag. Countries that take monetary policy before the US
lagged_vars <- c("GB", "DE")

# Create a lagged dataset
mat_data_lagged <- mat_data %>%
  mutate(across(all_of(lagged_vars), ~lag(.x, 1), .names = "lag_{.col}")) %>%  # Lag selected variables
  select(-all_of(lagged_vars)) %>%
  na.omit()  # Remove rows with missing values due to lagging

# Fit the Random Forest model (target variable is 'US')
rf_model <- randomForest(US ~ ., data = mat_data_lagged, importance = TRUE)

# Extract feature importance
importance_plot <- importance(rf_model)

# Convert to data frame for easier manipulation
importance_df <- data.frame(Feature = rownames(importance_plot), Importance = importance_plot[, 1])

# Order by importance (descending)
importance_df <- importance_df[order(-importance_df$Importance), ]

# Select top 50 most important features
top_50 <- head(importance_df, 50)

# Plot
barplot(top_50$Importance, names.arg = top_50$Feature, 
        las = 2, col = "steelblue", main = "Top 50 Feature Importance", 
        cex.names = 0.7) # Reduce text size for readability



#############################################################
### Feature importance in different periods

# Pre and post Great Recession January 2008

# Convert mat_data to a data frame 
# CHECK THE DATE WHEN CHANGING DATABASE
mat_data_new <- data
mat_data_new$date <- seq(from = as.Date("1990-01-01"), by = "month", length.out = nrow(mat_data_new))

# Variables to lag. Countries that take monetary policy before the US
lagged_vars <- c("GB", "DE")  

# Create a lagged dataset
mat_data_lagged <- mat_data_new %>%
  mutate(across(all_of(lagged_vars), ~lag(.x, 1), .names = "lag_{.col}")) %>%  # Lag selected variables
  select(-all_of(lagged_vars)) %>%
  na.omit()  # Remove rows with missing values due to lagging

# Divide the data
mat_data_pre <- filter(mat_data_lagged, date < as.Date("2008-01-01"))
mat_data_post  <- filter(mat_data_lagged, date >= as.Date("2008-01-01"))

# Remove date column before training 
mat_data_pre <- select(mat_data_pre, -date)
mat_data_post  <- select(mat_data_post, -date)

# Random Forest model before 2008
rf_pre <- randomForest(US ~ ., data = mat_data_pre, importance = TRUE)

# Random Forest model after 2008
rf_post <- randomForest(US ~ ., data = mat_data_post, importance = TRUE)

# Extract feature importance for both models
importance_before <- importance(rf_pre)
importance_after  <- importance(rf_post)

# Convert to data frame for easier manipulation
importance_df_before <- data.frame(Feature = rownames(importance_before), Importance = importance_before[, 1])
importance_df_after  <- data.frame(Feature = rownames(importance_after), Importance = importance_after[, 1])

# Order by importance (descending)
importance_df_before <- importance_df_before[order(-importance_df_before$Importance), ]
importance_df_after  <- importance_df_after[order(-importance_df_after$Importance), ]

# Select top 50 most important features
top_50_before <- head(importance_df_before, 50)
top_50_after  <- head(importance_df_after, 50)

# Plot feature importance (Before 2008)
barplot(top_50_before$Importance, names.arg = top_50_before$Feature, 
        las = 2, col = "steelblue", main = "Top 50 Feature Importance (Before 2008)", 
        cex.names = 0.7)

# Plot feature importance (After 2008)
barplot(top_50_after$Importance, names.arg = top_50_after$Feature, 
        las = 2, col = "darkred", main = "Top 50 Feature Importance (After 2008)", 
        cex.names = 0.7)



#############################################################
### Feature importance in different periods

# Pre and post Pandemic March 2020

# Convert mat_data to a data frame 
# CHECK THE DATE WHEN CHANGING DATABASE
mat_data_new <- data
mat_data_new$date <- seq(from = as.Date("1990-01-01"), by = "month", length.out = nrow(mat_data_new))

# Variables to lag. Countries that take monetary policy before the US
lagged_vars <- c("GB", "DE")  

# Create a lagged dataset
mat_data_lagged <- mat_data_new %>%
  mutate(across(all_of(lagged_vars), ~lag(.x, 1), .names = "lag_{.col}")) %>%  # Lag selected variables
  select(-all_of(lagged_vars)) %>%
  na.omit()  # Remove rows with missing values due to lagging

# Divide the data
mat_data_pre <- filter(mat_data_lagged, date < as.Date("2020-03-01"))
mat_data_post  <- filter(mat_data_lagged, date >= as.Date("2020-03-01"))

# Remove date column before training 
mat_data_pre <- select(mat_data_pre, -date)
mat_data_post  <- select(mat_data_post, -date)

# Random Forest model before March 2020
rf_pre <- randomForest(US ~ ., data = mat_data_pre, importance = TRUE)

# Random Forest model after March 2020
rf_post <- randomForest(US ~ ., data = mat_data_post, importance = TRUE)

# Extract feature importance for both models
importance_before <- importance(rf_pre)
importance_after  <- importance(rf_post)

# Convert to data frame for easier manipulation
importance_df_before <- data.frame(Feature = rownames(importance_before), Importance = importance_before[, 1])
importance_df_after  <- data.frame(Feature = rownames(importance_after), Importance = importance_after[, 1])

# Order by importance (descending)
importance_df_before <- importance_df_before[order(-importance_df_before$Importance), ]
importance_df_after  <- importance_df_after[order(-importance_df_after$Importance), ]

# Select top 50 most important features
top_50_before <- head(importance_df_before, 50)
top_50_after  <- head(importance_df_after, 50)

# Plot feature importance (Before March 2020)
barplot(top_50_before$Importance, names.arg = top_50_before$Feature, 
        las = 2, col = "steelblue", main = "Top 50 Feature Importance (Before March 2020)", 
        cex.names = 0.7)

# Plot feature importance (After March 2020)
barplot(top_50_after$Importance, names.arg = top_50_after$Feature, 
        las = 2, col = "darkred", main = "Top 50 Feature Importance (After March 2020)", 
        cex.names = 0.7)





####################################################
##  Factor DiD

# What we expect:
# 1. If the time is significant, then in general the feature importance changed
# significantly before and after the pandemic.

# 2. If the category is significant, we can identify which categories
# gained/lost importance

# We need categories to test our hypothesis easier
# There are 2 important reasons to use categories instead of the ~105 variables
# 1. Separate estimates can be difficult to interpret. 
# 2. Each regression can have a higher noise separately (high variance = weak significance)

external_mon_pol_others <- c()
external_mon_pol_ea <- c("lag_DE", "FR", "IT")
gdp <- c("rgdp_growth", "rgdp")

# Categorize features
categorize_feature <- function(feature) {
  if (str_detect(feature, "^inf_")) {
    return("Inflation Components")
  } else if (feature %in% external_mon_pol_ea) {
    return("External Monetary Policy (EA)")
  } else if (feature == "UNRATE") {
    return ("Unemployment Rate")
  } else if (feature == "EXPINF1YR") {
    return ("Inflation Expectations 1 Year Ahead")
  } else if (feature %in% gdp) {
    return ("Real GDP")
  } else if (feature == "VIXCLS") {
    return ("Volatility Index")
  } else if (feature == "WTISPLC") {
    return ("Oil prices")
  } else {
    return("External Monetary Policy (Others)")
  }
}

# Apply categorization to before and after datasets
importance_df_before$category <- sapply(importance_df_before$Feature, categorize_feature)
importance_df_after$category <- sapply(importance_df_after$Feature, categorize_feature)

# Create a dummy variable to identify before/after pandemic
importance_df_before$time <- 0  # Pre-pandemic
importance_df_after$time <- 1   # Post-pandemic

# Merge my feature importance
importance_combined <- rbind(importance_df_before, importance_df_after)
importance_combined$category <- as.factor(importance_combined$category)

# Run the Factor DiD regression
# Importance is the dependent variable (change in importance = treatment effect)
# Time captures the overall change in feature importance
# category captures differences in feature importance across categories
# |Feature adds controls for baseline differences across variables. It remocves the
#   influence of features that always were important.
# |0 The first one refers to instrumental variables (which we are not using)
# |0 The second one refers to clustering standard error (not used either FOR NOW)
#   We may use this for robust results

model <- felm(Importance ~ time * category | Feature | 0 | 0, data = importance_combined)

# Summary of the model
summary(model)

# Visualizing the change in feature importance
ggplot(importance_combined, aes(x = Feature, y = Importance, fill = as.factor(time))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~category, scales = "free_x") +
  labs(title = "Feature Importance Before and After the Pandemic",
       x = "Feature",
       y = "Importance Score",
       fill = "Time Period") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "darkred")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



####

check <- importance_combined %>%
  group_by(category) %>%
  summarize(
    mean_importance = mean(Importance, na.rm = TRUE),
    sd_importance = sd(Importance, na.rm = TRUE),
    min_importance = min(Importance, na.rm = TRUE),
    max_importance = max(Importance, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(sd_importance)  # Sort by standard deviation


ggplot(importance_combined, aes(x = category, y = Importance)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Feature Importance by Category")




# The problem is that the category of Mon Pol (EA) has low variance inside the group
# because the mon pol of FR, IT, DE are the same. 
# I try to do it separately, only keeping categories of inflation vars

# Categorize features
categorize_feature <- function(feature) {
  if (str_detect(feature, "^inf_")) {
    return("Inflation Components")
  } else if (feature == "CA") {
    return("External Monetary Policy (CA)")
  } else if (feature == "FR") {
    return("External Monetary Policy (FR)")
  } else if (feature == "lag_DE") {
    return("External Monetary Policy (lag_DE)")
  } else if (feature == "lag_GB") {
    return("External Monetary Policy (lag_GB)")
  } else if (feature == "IT") {
    return("External Monetary Policy (IT)")
  } else if (feature == "JP") {
    return("External Monetary Policy (JP)")
  } else if (feature == "UNRATE") {
    return ("Unemployment Rate")
  } else if (feature == "EXPINF1YR") {
    return ("Inflation Expectations 1 Year Ahead")
  } else if (feature %in% gdp) {
    return ("Real GDP")
  } else if (feature == "VIXCLS") {
    return ("Volatility Index")
  } else if (feature == "WTISPLC") {
    return ("Oil prices")
  } else {
    return("Others")
  }
}

# Apply categorization to before and after datasets
importance_df_before$category <- sapply(importance_df_before$Feature, categorize_feature)
importance_df_after$category <- sapply(importance_df_after$Feature, categorize_feature)

# Create a dummy variable to identify before/after pandemic
importance_df_before$time <- 0  # Pre-pandemic
importance_df_after$time <- 1   # Post-pandemic

# Merge my feature importance
importance_combined <- rbind(importance_df_before, importance_df_after)
importance_combined$category <- as.factor(importance_combined$category)

# Run the Factor DiD regression
# Importance is the dependent variable (change in importance = treatment effect)
# Time captures the overall change in feature importance
# category captures differences in feature importance across categories
# |Feature adds controls for baseline differences across variables. It remocves the
#   influence of features that always were important.
# |0 The first one refers to instrumental variables (which we are not using)
# |0 The second one refers to clustering standard error (not used either FOR NOW)
#   We may use this for robust results

model <- felm(Importance ~ time * category | Feature | 0 | 0, data = importance_combined)

# Summary of the model
summary(model)

# Visualizing the change in feature importance
ggplot(importance_combined, aes(x = Feature, y = Importance, fill = as.factor(time))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~category, scales = "free_x") +
  labs(title = "Feature Importance Before and After the Pandemic",
       x = "Feature",
       y = "Importance Score",
       fill = "Time Period") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "darkred")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## Try every feature

importance_combined <- importance_combined %>%
  mutate(Importance = Importance + rnorm(n(), mean = 0, sd = 0.001))  # Small noise

model <- felm(Importance ~ time * category + Feature | 0 | 0 | 1, data = importance_combined)

summary(model)

# Visualizing the change in feature importance
ggplot(importance_combined, aes(x = Feature, y = Importance, fill = as.factor(time))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~category, scales = "free_x") +
  labs(title = "Feature Importance Before and After the Pandemic",
       x = "Feature",
       y = "Importance Score",
       fill = "Time Period") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "darkred")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
