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
set.seed(123)



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

# Take away US monetary policy
data <- data %>% select(-US)

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


##################################################
##    K_MEANS (WHOLE DATA)

# Instead of using a fixed number of clusters, I'll do iteration until I see the
# addition of one cluster is not worth it (https://www.datacamp.com/tutorial/k-means-clustering-r)

n_clusters <- 50

# Initialize total within-cluster sum of squares
sq <- numeric(n_clusters)

k_star <- 0
diff_star <- 0

# Loop looking at every option of clusters
for (k in 1:n_clusters){
  kmeans_res <- kmeans(data_scaled[,-1], centers = k)
  sq[k] <- kmeans_res$tot.withinss
  
  #Checking the difference once we add a new cluster
  if (k == 1){
    diff <- abs(sq[k])
  } else {
    diff <- abs(sq[k] - sq[k-1])
  }
  
  # Adding a small enough threshold (where the plot starts appearing flat)
  if (diff <= 1000) {
    k_star <- k
    diff_Star <- diff
    break
  }
}

# Produce a scree plot
sq_df <- tibble(clusters = 1:n_clusters, sq=sq)


scree_plot <- ggplot(sq_df[1:k_star,], aes(x=clusters, y=sq, group=1)) +
  geom_point(size=4) +
  geom_line() + 
  scale_x_continuous() +
  xlab("Num of clusters")

scree_plot

# For the general data, it gives us k = 8

# Add cluster labels
data_scaled$Cluster <- as.factor(kmeans_res$cluster)

# If we add the cluster level to the database it would create difficulties 
# in the analysis. We can treat it as a third dimension instead

data_long <- melt(data_scaled, 
                  id.vars = c("Variable", "Cluster"),  # Keep these columns fixed
                  variable.name = "Date",  # Column name for the time periods
                  value.name = "Value")  # Column name for observations



####################################
## SECOND APPROACH KMEANS (WHOLE DATA)

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

# Instead of using a fixed number of clusters, I'll do iteration until I see the
# addition of one cluster is not worth it (https://www.datacamp.com/tutorial/k-means-clustering-r)

n_clusters <- 50

# Initialize total within-cluster sum of squares
sq <- numeric(n_clusters)

k_star <- 0
diff_star <- 0


# Loop looking at every option of clusters
for (k in 2:n_clusters){
  # For centroid: One approach is to use partition around medoids (PAM). A medoid is simply a representative object
  # from a cluster, in this case also a time-series, whose average distance to all other objects in the same
  # cluster is minimal.
  
  kmeans_dynamic <- tsclust(data_scaled[,-1], type = "partitional", k = k, 
                            distance = "L2", centroid = "pam", seed = 123) 
  
  # For these kmeans the distance in within the clustinfo variable (avg_dist) and computes
  # the average distance between series and their respective centroids (crisp partition).
  
  sq[k] <- max(kmeans_dynamic@clusinfo$av_dist)
  
  #Checking the difference once we add a new cluster
  diff <- abs(sq[k] - sq[k-1])

  
  #Adding a small enough threshold (where the plot starts appearing flat)
  if (diff <= 0.0001) {
    k_star <- k
    diff_star <- diff
    break
  }
}

# Produce a scree plot
sq_df <- tibble(clusters = 1:n_clusters, sq=sq)


scree_plot <- ggplot(sq_df[2:k_star,], aes(x=clusters, y=sq, group=1)) +
  geom_point(size=4) +
  geom_line() + 
  scale_x_continuous() +
  xlab("Num of clusters")

scree_plot

# k_star = 10

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



######################################################
## KMEANS (By country, by inflation, etc)

# DIVIDE THE DATA IN GROUPS
######################
# Monetary policy data
######################

countries <- c("CA", "DE", "FR", "IT", "JP", "lag_GB")

# Salect monetary policies
monetary <- data_scaled %>% filter(Variable %in% countries)

# Number of clusters (3) (without using variable names)
# This was arbitrary, no need to choose the best number of clusters
# Expected groups from G7 countries: 1 North American countries, 2 European, 3 Asian
k <- 3
monetary_kmeans <- kmeans(monetary[,-1], centers = k)

# Add cluster labels
monetary$Cluster <- as.factor(monetary_kmeans$cluster)


data_long_monetary <- melt(monetary, 
                  id.vars = c("Variable", "Cluster"),  # Keep these columns fixed
                  variable.name = "Date",  # Column name for the time periods
                  value.name = "Value")  # Column name for observations


grouped_monetary <- data_long_monetary %>%
  group_by(Date, Cluster) %>%
  summarise(mean_value = mean(Value), .groups = "drop") %>%
  pivot_wider(names_from = Cluster, values_from = mean_value, names_prefix = "Clus_mon_")

######################
# Inflation
######################


inflation <- data_scaled %>% 
  filter(str_detect(Variable, "^inf_"))

# To pick k (first link)
# Instead of using a fixed number of clusters, I'll do iteration until I see the
# addition of one cluster is not worth it (https://www.datacamp.com/tutorial/k-means-clustering-r)

n_clusters <- 50

# Initialize total within-cluster sum of squares
sq <- numeric(n_clusters)

k_star <- 0
diff_star <- 0

# Loop looking at every option of clusters
for (k in 1:n_clusters){
  inflation_kmeans <- kmeans(inflation[,-1], centers = k)
  sq[k] <- inflation_kmeans$tot.withinss
  
  #Checking the difference once we add a new cluster
  if (k == 1){
    diff <- abs(sq[k])
  } else {
    diff <- abs(sq[k] - sq[k-1])
  }
  
  # Adding a small enough threshold (where the plot starts appearing flat)
  if (diff <= 1000) {
    k_star <- k
    diff_star <- diff
    break
  }
}

# Produce a scree plot
sq_df <- tibble(clusters = 1:n_clusters, sq=sq)


scree_plot <- ggplot(sq_df[1:k_star,], aes(x=clusters, y=sq, group=1)) +
  geom_point(size=4) +
  geom_line() + 
  scale_x_continuous() +
  xlab("Num of clusters")

scree_plot

# For the general data, it gives us k = 6

# Add cluster labels
inflation$Cluster <- as.factor(inflation_kmeans$cluster)





######################################################
## KMEANS (By country, by inflation, etc) SECOND APPROACH

# DIVIDE THE DATA IN GROUPS
######################
# Monetary policy data
######################

monetary <- data_scaled[5:10,]

# Number of clusters (3) (without using variable names)
# This was arbitrary, no need to choose the best number of clusters
# Expected groups from G7 countries: 1 North American countries, 2 European, 3 Asian
k <- 3
monetary_kmeans_dyn <- tsclust(data_scaled[,-1], type = "partitional", k = k, 
                           distance = "L2", centroid = "pam", seed = 123)

# Add cluster labels
monetary$Cluster_dyn <- as.factor(monetary_kmeans@cluster)

######################
# Inflation
######################

inflation <- data_scaled[13:104,]

# To pick k (first link)
# Instead of using a fixed number of clusters, I'll do iteration until I see the
# addition of one cluster is not worth it (https://www.datacamp.com/tutorial/k-means-clustering-r)

n_clusters <- 50

# Initialize total within-cluster sum of squares
sq <- numeric(n_clusters)

k_star <- 0
diff_star <- 0

# Loop looking at every option of clusters
for (k in 2:n_clusters){
  # For centroid: One approach is to use partition around medoids (PAM). A medoid is simply a representative object
  # from a cluster, in this case also a time-series, whose average distance to all other objects in the same
  # cluster is minimal.
  
  kmeans_dynamic <- tsclust(inflation[,-1], type = "partitional", k = k, 
                            distance = "L2", centroid = "pam", seed = 123) 
  
  # For these kmeans the distance in within the clustinfo variable (avg_dist) and computes
  # the average distance between series and their respective centroids (crisp partition).
  
  sq[k] <- max(kmeans_dynamic@clusinfo$av_dist)
  
  #Checking the difference once we add a new cluster
  diff <- abs(sq[k] - sq[k-1])
  
  
  #Adding a small enough threshold (where the plot starts appearing flat)
  if (diff <= 0.0001) {
    k_star <- k
    diff_star <- diff
    break
  }
}

# Produce a scree plot
sq_df <- tibble(clusters = 1:n_clusters, sq=sq)


scree_plot <- ggplot(sq_df[2:k_star,], aes(x=clusters, y=sq, group=1)) +
  geom_point(size=4) +
  geom_line() + 
  scale_x_continuous() +
  xlab("Num of clusters")

scree_plot

# k_star = 8

# Add cluster labels
inflation$Cluster_dyn <- as.factor(kmeans_dynamic@cluster)


# We need to add 3 to  these cluster so we have (1, 2, 3) for monetary
# and (4, 5, 6, ...) for inflation, the others should be inside the same cluster
# (the last + 1). 



