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



# The objective of the following document is to create k-means,
# run random forests and obtain the betas, 
# running a regression with betas as the dependent variable
# and controlling that regression with the categories (fixing x_2)
# from this regression we want to see if the effect of time is significant


#Set path
setwd("C:/Users/HP/Downloads/UChicago/1. Courses/2. Winter Quarter 2025/2.3 MACSS 31330 Econometrics and Machine Learning/Final project/database/USA")


# USING DATA "1990_G7_US"
data <- fread("1990_G7_US.csv", na.strings = "NA")


##################################################
##    K_MEANS

# Scale the numeric data
data_no_date <- data[, -1]
data_scaled <- scale(data_no_date[, lapply(.SD, as.numeric), .SDcols = names(data_no_date)])


# I need to transpose the data so the cluster is on variables and not on dates
data_scaled <- as.data.table(t(data_scaled))
# Assign dates to colnames
setnames(data_scaled, old = names(data_scaled), new = as.character(data[[1]]))  # Use dates as column names
# Add a new column for variable names
data_scaled[, Variable := colnames(data_no_date)]
# Move Variable column to the first position
setcolorder(data_scaled, c("Variable", setdiff(names(data_scaled), "Variable")))


set.seed(123)
# Number of clusters (3) (without using variable names)
k <- 3
kmeans_res <- kmeans(data_scaled[,-1], centers = k)

# Add cluster labels
data_scaled$Cluster <- as.factor(kmeans_res$cluster)

# If we add the cluster level to the database it would create difficulties 
# in the analysis. We can treat it as a third dimension instead

data_long <- melt(data_scaled, 
                  id.vars = c("Variable", "Cluster"),  # Keep these columns fixed
                  variable.name = "Date",  # Column name for the time periods
                  value.name = "Value")  # Column name for observations

# We need to format the date

