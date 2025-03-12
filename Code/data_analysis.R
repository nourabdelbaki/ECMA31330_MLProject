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
library(dtwclust)



# The objective of the following document is to create k-means,
# run random forests and obtain the betas, 
# running a regression with betas difference as the dependent variable
# and controlling that regression with the categories (fixing x_2)
# from this regression we want to see if the effect of time is significant


# USING DATA "1990_G7_US"
data <- fread("1990_G7_US.csv", na.strings = "NA")

# We need to format the date for future use
data <- data %>%
  mutate(date = seq.Date(from = as.Date("1990-01-01"), by = "month", length.out = n()))


##################################################
##    K_MEANS

# Scale the numeric data
data_no_date <- data[, -1]
data_scaled <- scale(data_no_date[, lapply(.SD, as.numeric), .SDcols = names(data_no_date)])

#DATA RESTRUCTURE
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
# Our first approach is using 3 clusters (ideally, 1 will capture monetary
# policies, 1 inflation components, and 1 for inflation outliers or other components)
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



####################################
## SECOND APPROACH KMEANS

## To figure out the ideal number of clusters:
# ref: https://www.datacamp.com/tutorial/k-means-clustering-r
## Would be better to use something that capitalizes on the time element:
# ref: https://levelup.gitconnected.com/unveiling-patterns-in-time-a-guide-to-time-series-clustering-with-tslearn-50a2ff305afe
# Above is in python, so we found similar package in R, but intuition from above
# ref: https://cran.r-project.org/web/packages/dtwclust/index.html and
# https://journal.r-project.org/archive/2019/RJ-2019-023/RJ-2019-023.pdf 

# The one we want is the partitional kmeans 
# Partitional clustering is a strategy used to create partitions. 
# In this case, the data is explicitly assigned to one and only one cluster 
# out of k total clusters.

# For centroid: One approach is to use partition around medoids (PAM). A medoid is simply a representative object
# from a cluster, in this case also a time-series, whose average distance to all other objects in the same
# cluster is minimal.

kmeans_dynamic <- tsclust(data_scaled[,-1], type = "partitional", k=3L, 
                          distance = "L2", centroid = "pam", seed = 123) 

# Add cluster labels
data_scaled$Cluster_dyn <- as.factor(kmeans_dynamic@cluster)

# If we add the cluster level to the database it would create difficulties 
# in the analysis. We can treat it as a third dimension instead

data_long <- melt(data_scaled, 
                  id.vars = c("Variable", "Cluster", "Cluster_dyn"),  # Keep these columns fixed
                  variable.name = "Date",  # Column name for the time periods
                  value.name = "Value")  # Column name for observations


# Plot using the original clustering method
ggplot(data_long, aes(x = Date, y = Value, group = Variable, color = as.factor(Cluster))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~Cluster, scales = "free_y") +
  theme_minimal() +
  ggtitle("Time Series Clustering - Method 1")

# Plot using the dynamic clustering method
ggplot(data_long, aes(x = Date, y = Value, group = Variable, color = as.factor(Cluster_dyn))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~Cluster_dyn, scales = "free_y") +
  theme_minimal() +
  ggtitle("Time Series Clustering - Method 2")
