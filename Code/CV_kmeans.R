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
library(tidyr)
library(cluster)
set.seed(123)

# The objective of the following document is to create k-means,
# and cross validate to choose the best amount of clusters and
# the best model using the Silhouette score.


# USING DATA "1998_G7_US"
data <- fread("1998_G7_US.csv", na.strings = "NA")

# We need to format the date for future use
data <- data %>%
  mutate(date = seq.Date(from = as.Date("1998-01-01"), by = "month", length.out = n()))

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

data_kmeans <- data_scaled

# Maximum number of clusters to test
n_clusters <- 10

# Initialize silhouette scores
sil_scores <- numeric(n_clusters - 1)  # k must be at least 2

k_star <- 0

# Loop through different cluster numbers
for (k in 2:n_clusters) {  
  kmeans_res <- kmeans(data_kmeans[,-1], centers = k)  # Run k-means
  
  # Compute silhouette score
  sil_info <- silhouette(kmeans_res$cluster, dist(data_kmeans[,-1]))
  sil_scores[k] <- mean(sil_info[, 3])  # Store average silhouette width
  
}

# Determine the optimal number of clusters
k_star <- which.max(sil_scores) 

# Create a dataframe for visualization
sil_df <- tibble(clusters = 1:n_clusters, silhouette = sil_scores)

# Plot silhouette scores
sil_plot <- ggplot(sil_df, aes(x = clusters, y = silhouette, group = 1)) +
  geom_point(size = 4, color = "blue") +
  geom_line(color = "blue") +
  xlab("Number of Clusters (k)") +
  ylab("Average Silhouette Score") +
  scale_x_continuous(breaks = seq(1, n_clusters, by = 1)) +
  ggtitle("Silhouette Scores for Different k") +
  theme_minimal()

print(sil_plot)

# Print optimal k
print(paste("Optimal number of clusters:", k_star))

# Add cluster labels
data_kmeans$Cluster <- as.factor(kmeans_res$cluster)

# If we add the cluster level to the database it would create difficulties
# in the analysis. We can treat it as a third dimension instead

data_long <- melt(data_kmeans,
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

data_kmeans_dyn <- data_scaled

# Maximum number of clusters to test
n_clusters <- 10

# Initialize silhouette scores
sil_scores <- numeric(n_clusters - 1)  # k must be at least 2
k_star <- 0

# Loop through different cluster numbers
for (k in 2:n_clusters) {  
  tsclust_res <- tsclust(data_kmeans_dyn[,-1], type = "partitional", k = k, 
                         distance = "L2", centroid = "dba", seed = 123, parallel = TRUE)  # DTW-based clustering
  
  # Compute dissimilarity matrix
  diss_matrix <- proxy::dist(data_kmeans_dyn[, -1], method = "L2")  
  
  # Compute silhouette score
  sil_info <- silhouette(tsclust_res@cluster, diss_matrix)  
  sil_scores[k] <- mean(sil_info[, 3])  # Store average silhouette width
}

# Determine the optimal number of clusters
k_star <- which.max(sil_scores) 

# Create a dataframe for visualization
sil_df <- tibble(clusters = 1:n_clusters, silhouette = sil_scores)

# Plot silhouette scores
sil_plot <- ggplot(sil_df, aes(x = clusters, y = silhouette, group = 1)) +
  geom_point(size = 4, color = "blue") +
  geom_line(color = "blue") +
  xlab("Number of Clusters (k)") +
  ylab("Average Silhouette Score") +
  scale_x_continuous(breaks = seq(1, n_clusters, by = 1)) +
  ggtitle("Silhouette Scores for Different k") +
  theme_minimal()

print(sil_plot)

# Print optimal k
print(paste("Optimal number of clusters:", k_star))

# Add cluster labels
data_kmeans_dyn$Cluster <- as.factor(tsclust_res@cluster)

# Convert data to long format for visualization
data_long_dyn <- melt(data_kmeans_dyn,
                  id.vars = c("Variable", "Cluster"),  # Keep these columns fixed
                  variable.name = "Date",  # Column name for the time periods
                  value.name = "Value")  # Column name for observations



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
# Expected groups from G7 countries: 1 North American country, 2 European, 3 Asian
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
  filter(str_detect(Variable, "^inf_|^lag_inf"))

# To pick k (first link)
# Instead of using a fixed number of clusters, I'll do iteration until I see the
# addition of one cluster is not worth it (https://www.datacamp.com/tutorial/k-means-clustering-r)

# Maximum number of clusters to test
n_clusters <- 10

# Initialize silhouette scores
sil_scores <- numeric(n_clusters - 1) 

k_star <- 0

# Loop through different cluster numbers
for (k in 2:n_clusters) {  
  inflation_k <- kmeans(inflation[,-1], centers = k)  # Run k-means
  
  # Compute silhouette score
  sil_info <- silhouette(inflation_k$cluster, dist(inflation[,-1]))
  sil_scores[k] <- mean(sil_info[, 3])  # Store average silhouette width
  
}

max_kmeans <- max(sil_scores)

# Determine the optimal number of clusters
k_star <- which.max(sil_scores) 

# Create a dataframe for visualization
sil_df <- tibble(clusters = 1:n_clusters, silhouette = sil_scores)

# Plot silhouette scores
sil_plot <- ggplot(sil_df, aes(x = clusters, y = silhouette, group = 1)) +
  geom_point(size = 4, color = "blue") +
  geom_line(color = "blue") +
  xlab("Number of Clusters (k)") +
  ylab("Average Silhouette Score") +
  scale_x_continuous(breaks = seq(1, n_clusters, by = 1)) +
  ggtitle("Silhouette Scores for Different k") +
  theme_minimal()

print(sil_plot)

# Print optimal k
print(paste("Optimal number of clusters:", k_star))



######################################################
## KMEANS (By country, by inflation, etc) SECOND APPROACH

# DIVIDE THE DATA IN GROUPS
######################
# Monetary policy data
######################

countries <- c("CA", "DE", "FR", "IT", "JP", "lag_GB")

# Salect monetary policies
monetary_dyn <- data_scaled %>% filter(Variable %in% countries)

# Number of clusters (3) (without using variable names)
# This was arbitrary, no need to choose the best number of clusters
# Expected groups from G7 countries: 1 North American countries, 2 European, 3 Asian
k <- 3
monetary_kmeans_dyn <- tsclust(monetary_dyn[,-1], type = "partitional", k = k, 
                           distance = "L2", centroid = "pam", seed = 123)

# Add cluster labels
monetary_dyn$Cluster_dyn <- as.factor(monetary_kmeans_dyn@cluster)

data_long_monetary_dyn <- melt(monetary_dyn, 
                           id.vars = c("Variable", "Cluster_dyn"),  # Keep these columns fixed
                           variable.name = "Date",  # Column name for the time periods
                           value.name = "Value")  # Column name for observations

# Grouped with mean
grouped_monetary <- data_long_monetary_dyn %>%
  group_by(Date, Cluster_dyn) %>%
  summarise(mean_value = mean(Value), .groups = "drop") %>%
  pivot_wider(names_from = Cluster_dyn, values_from = mean_value, names_prefix = "Clus_mon_")

# Grouped with PCA
# Step 1: Compute PCA (assuming `Value` is the numeric feature)
pca_res <- PCA(data_long_monetary_dyn %>% select(Value), scale.unit = TRUE, graph = FALSE)

# Step 2: Extract principal component scores
pca_scores <- as.data.frame(pca_res$ind$coord)

# Step 3: Add back the original information
data_pca <- data_long_monetary_dyn %>%
  mutate(PC1 = pca_scores$Dim.1)



######################
# Inflation 
######################

inflation_dyn <- data_scaled %>% 
  filter(str_detect(Variable, "^inf_|^lag_inf"))

# To pick k (first link)
# Instead of using a fixed number of clusters, I'll do iteration until I see the
# addition of one cluster is not worth it (https://www.datacamp.com/tutorial/k-means-clustering-r)

# Maximum number of clusters to test
n_clusters <- 10
k_star <- 0

# Running different approaches of the tsclust package

######################  APPROACH 1: DBA/DTW
# Initialize silhouette scores
sil_scores_dba <- numeric(n_clusters - 1)  # k must be at least 2

# Loop through different cluster numbers
for (k in 2:n_clusters) {  
  # Using "dtw_basic" and "parallel" for faster computation
  tsclust_res <- tsclust(inflation_dyn[,-1], type = "partitional", k = k, 
                         distance = "dtw_basic", centroid = "dba", seed = 123, parallel = TRUE)  # DTW-based clustering
  
  # Compute dissimilarity matrix
  diss_matrix <- proxy::dist(inflation_dyn[, -1], method = "dtw_basic")  
  
  # Compute silhouette score
  sil_info <- silhouette(tsclust_res@cluster, diss_matrix)  
  sil_scores_dba[k] <- mean(sil_info[, 3])  # Store average silhouette width
}

max_dba <- max(sil_scores_dba)


####################### APPROACH 2: sdtw_cent/sdtw
# Initialize silhouette scores
sil_scores_stdw <- numeric(n_clusters - 1)  # k must be at least 2

# Loop through different cluster numbers
for (k in 2:n_clusters) {  
  # Using "dtw_basic" and "parallel" for faster computation
  tsclust_res <- tsclust(inflation_dyn[,-1], type = "partitional", k = k, 
                         distance = "L2", centroid = "sdtw_cent", seed = 123, parallel = TRUE)  # DTW-based clustering
  
  # Compute dissimilarity matrix
  diss_matrix <- proxy::dist(inflation_dyn[, -1], method = "L2")  
  
  # Compute silhouette score
  sil_info <- silhouette(tsclust_res@cluster, diss_matrix)  
  sil_scores_stdw[k] <- mean(sil_info[, 3])  # Store average silhouette width
}

max_stdw <- max(sil_scores_stdw)

###################### APPROACH 3: Mean/L2
# Initialize silhouette scores
sil_scores_mean <- numeric(n_clusters - 1)  # k must be at least 2

# Loop through different cluster numbers
for (k in 2:n_clusters) {  
  # Using "dtw_basic" and "parallel" for faster computation
  tsclust_res <- tsclust(inflation_dyn[,-1], type = "partitional", k = k, 
                         distance = "L2", centroid = "mean", seed = 123, parallel = TRUE)  # DTW-based clustering
  
  # Compute dissimilarity matrix
  diss_matrix <- proxy::dist(inflation_dyn[, -1], method = "L2")  
  
  # Compute silhouette score
  sil_info <- silhouette(tsclust_res@cluster, diss_matrix)  
  sil_scores_mean[k] <- mean(sil_info[, 3])  # Store average silhouette width
}

max_mean <- max(sil_scores_mean)


############################################################

# Compare all the approaches and get the best model

best_max <- max(max_kmeans, max_mean, max_dba, max_stdw)

if (best_max == max_kmeans){
  # Determine the optimal number of clusters
  k_star <- which.max(sil_scores) 
  best_model <- sil_scores
} else if (best_max == max_mean) {
  k_star <- which.max(sil_scores_mean) 
  best_model <- sil_scores_mean
} else if (best_max == max_dba) {
  k_star <- which.max(sil_scores_dba) 
  best_model <- sil_scores_dba
} else {
  k_star <- which.max(sil_scores_stdw)
  best_model <- sil_scores_stdw
}


# Create a dataframe for visualization
sil_df <- tibble(clusters = 1:n_clusters, silhouette = best_model)

# Plot silhouette scores
sil_plot <- ggplot(sil_df, aes(x = clusters, y = silhouette, group = 1)) +
  geom_point(size = 4, color = "blue") +
  geom_line(color = "blue") +
  xlab("Number of Clusters (k)") +
  ylab("Average Silhouette Score") +
  scale_x_continuous(breaks = seq(1, n_clusters, by = 1)) +
  ggtitle("Silhouette Scores for Different k") +
  theme_minimal()

print(sil_plot)

# Print optimal k
print(paste("Optimal number of clusters:", k_star))


########################################
## The best model is the means from tsclust

tsclust_res <- tsclust(inflation_dyn[,-1], type = "partitional", k = k_star, 
                       distance = "L2", centroid = "mean", seed = 123, parallel = TRUE)  # DTW-based clustering

inflation$Cluster <- as.factor(tsclust_res@cluster)



# data long format
data_long_inflation <- melt(inflation, 
                            id.vars = c("Variable", "Cluster"),  # Keep these columns fixed
                            variable.name = "Date",  # Column name for the time periods
                            value.name = "Value")  # Column name for observations


# Group using average
grouped_inflation <- data_long_inflation %>%
  group_by(Date, Cluster) %>%
  summarise(mean_value = mean(Value), .groups = "drop") %>%
  pivot_wider(names_from = Cluster, values_from = mean_value, names_prefix = "Clus_inf_")



################################################################
## SAVE DATA

write.csv(grouped_monetary, "monetary_clustered.csv", row.names = FALSE)
write.csv(grouped_inflation, "inflation_clustered.csv", row.names = FALSE)


#################################################################
## VISUALIZATIONS

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


