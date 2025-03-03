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

data_cleaned <- data[, Filter(function(x) any(!is.na(x)), .SD)]

# Running OLS where y = monetary policy USA

possible_regressors <- data_cleaned %>% select(-US)
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
  
  #these are given
  xs <- as.matrix(possible_regressors[,2:ncol(possible_regressors)])
  #gamma is the true value so I don't need a function for that anymore
  y <- data_cleaned %>% select(US)
  
  
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


# Plots to understand the data

summary(data)

# The problem with OLS is that we have high dimensional data. My number of
# predictors is large compared to N, leading to overfitting. To get a 
# subset of best predictors, I would need to run many regressions--> Sign to use PCA
# to reduce dimensionality and retain most information.

# Another problem of our data is that it may have implicit interactions between
# regressors that we don't know about. --> sign to use Random Forests. 


#Descriptive plots to understand the data

###################################### Correlation

library(ggplot2)
library(corrplot)
library(reshape2)

# Compute correlation matrix for all variables
mat_data <- as.matrix(data_cleaned[,2:117])  # Exclude date column
mat_data_imputed <- mat_data
mat_data_imputed[is.na(mat_data_imputed)] <- apply(mat_data_imputed, 2, mean, na.rm = TRUE)

cor_matrix <- cor(mat_data_imputed, use = "complete.obs")  # Full correlation matrix

# Extract correlations for Monetary policy decisions in G20 countries
selected_vars <- colnames(data_cleaned[,-1])[4:22]  # Get variable names for columns 4-22 (EXTERNAL MONETARY POLICIES)
cor_subset <- cor_matrix[selected_vars, ]  # Keep all columns, but only rows from selected variables

# Convert to long format
cor_df <- melt(cor_subset)
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

# Convert factors to character to avoid level mismatch
cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)

# Remove self-correlations (Var1 == Var2)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]

# Select top 20 absolute correlations
top_cor <- cor_df[order(-abs(cor_df$Correlation)), ][1:50, ]

# Plot the highest 20 correlations
ggplot(top_cor, aes(x = reorder(paste(Var1, Var2, sep = " - "), abs(Correlation)), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 50 Correlations of Selected Variables with All Variables", x = "Variable Pair", y = "Correlation")





#################################### PCA

library(ggplot2)
library(factoextra)
library(FactoMineR)

#Data without lags

data_nolags <- mat_data_imputed
# Standardize the data
data_nolags_scaled <- scale(data_nolags)

# Step 1: Extract unique variable names from the top correlations
#top_vars <- unique(c(top_cor$Var1, top_cor$Var2))

# Step 2: Subset the original data to include only the most relevant variables
#selected_data <- mat_data_imputed[, colnames(mat_data_imputed) %in% top_vars]

# Step 3: Perform PCA
pca_result <- prcomp(data_nolags_scaled, center = TRUE, scale. = TRUE)

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
scores$component <- data_cleaned$US

# Variance of the scores for each principal component
variance_scores <- apply(scores[, -ncol(scores)], 2, var)  # Exclude the 'University' column
variance_scores

# Calculate the proportion of variance explained by each principal component
pve <- variance_scores / sum(variance_scores)
pve


# The principal components do not explain the variance is not an option


library(randomForest)

# Fit the Random Forest model (target variable is 'US')
rf_model <- randomForest(US ~ ., data = mat_data_imputed, importance = TRUE)

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
