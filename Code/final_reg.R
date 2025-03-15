# Nour Abdelbaki and Giuliana Triberti

# USING DATA "1998_G7_US"
data <- read.csv("1998_G7_US.csv", na.strings = "NA")

# From the kmeans approach.The best model was the one from the Time Series Clustering
# package, with centroids as "mean" (this is the same as the normal kmeans model), but
# using the distance "L2" with is slightly different from the Euclidean distance
# used by the kmeans model. The best model was chosen comparing the Silhouette score
# instead of the Elbow mechanism, to avoid heuristic rules. The best number of 
# clusters for inflation was found via cross validation (best no. clusters = 2), whereas
# for monetary policy we chose 3 clusters motivated by the geographical regions of the
# countries. The cluster results aligned greatly to what was expected and assigned a
# different cluster to different regions.

inf_clust <- read.csv("inflation_clustered.csv", na.strings = "NA")
mon_clust <- read.csv("monetary_clustered.csv", na.strings = "NA")


# From the random forest approach. The best model was chosen doing cross validation of
# node size, wether sampling is with replacement or without, the sample size and 
# the number of variables to test the MSE and define how the tree will move forward.
# We take the betas from this approach.




# Finally, we will run a regression to see if a difference in the coefficient of 
# a variable on the US has to do with a crisis period or not. In this regression
# we will control using the clusters of inflation and monetary policy from other 
# countries, as well as the alternate variables that may have an effect on this
# change. 


# Run second-stage regression
second_stage <- lm(Beta ~ t + factor(inf_clust) + factor(mon_clust) + 
                     factor(Cluster_unemployment), 
                   data = second_stage_data)









