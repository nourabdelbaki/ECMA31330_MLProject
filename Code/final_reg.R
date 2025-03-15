# Nour Abdelbaki and Giuliana Triberti

library(zoo)

# USING DATA "1998_G7_US"
data <- read.csv("1998_G7_US.csv", na.strings = "NA")

# From the kmeans approach.The best model was the one from the Time Series Clustering
# package, with centroids as "mean" (this is the same as the normal kmeans model), but
# using the distance "L2" which is the Euclidean distance. The best model was chosen comparing the Silhouette score
# instead of the Elbow mechanism, to avoid heuristic rules. The best number of 
# clusters for inflation was found via cross validation (best no. clusters = 2), whereas
# for monetary policy we chose 3 clusters motivated by the geographical regions of the
# countries. The cluster results aligned greatly to what was expected and assigned a
# different cluster to different regions.

inf_clust <- read.csv("inflation_clustered.csv", na.strings = "NA")
inf_clust <- inf_clust[,-1]
inf_clust$date <- data$date
mon_clust <- read.csv("monetary_clustered.csv", na.strings = "NA")


# From the random forest approach. The best model was chosen doing cross validation of
# node size, wether sampling is with replacement or without, the sample size and 
# the number of variables to test the MSE and define how the tree will move forward.
# We take the betas from this approach.

data_08_all <- read.csv("data_08_all.csv", na.strings = "NA")

data_covid_all <- read.csv("data_covid_all.csv", na.strings = "NA")

df_08 <- left_join(data_08_all, inf_clust, by = "date")
df_08<- df_08[,-1] # Drop duplicate row index 

df_covid <- left_join(data_covid_all, inf_clust, by = "date")
df_covid <- df_covid[,-1]

# Motivated by our monetary policy clustering using K-means, we group the entire
# EU together by averaging their interest rates. Note that this has an impact
# only before June 1998 and transition time for Italy's rate to settle at the EU
# rate. 
df_08 <- df_08 %>%
  mutate(EU = rowMeans(select(., DE, FR, IT)))

df_covid <- df_covid %>%
  mutate(EU = rowMeans(select(., DE, FR, IT)))

# Finally, we will run a regression to see if a difference in the coefficient of 
# a variable on the US has to do with a crisis period or not. In this regression
# we will control using the clusters of inflation and monetary policy from other 
# countries, as well as the alternate variables that may have an effect on this
# change. 


# Run second-stage regression for 2008 crisis periods
second_stage_CA <- lm(beta_CA ~ crisis + Clus_inf_1 + Clus_inf_2
                   + rgdp + rgdp_growth + UNRATE + EXPINF1YR + lag_GB + EU +
                     JP + VIXCLS + WTISPLC + GSCPI, 
                   data = df_08)

second_stage_GB <- lm(beta_GB ~ crisis + Clus_inf_1 + Clus_inf_2
                   + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + EU +
                     JP + VIXCLS + WTISPLC + GSCPI, 
                   data = df_08)

second_stage_EU <- lm(beta_EU ~ crisis + Clus_inf_1 + Clus_inf_2
                    + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                      JP + VIXCLS + WTISPLC + GSCPI, 
                    data = df_08)

second_stage_JP <- lm(beta_JP ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        EU + VIXCLS + WTISPLC + GSCPI, 
                      data = df_08)

summary(second_stage_CA)
summary(second_stage_GB)
summary(second_stage_EU)
summary(second_stage_JP)



# Run second-stage regression for COVID crisis period
second_stage_CA_cov <- lm(beta_CA ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + lag_GB + EU +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

second_stage_GB_cov <- lm(beta_GB ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + EU +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

######## CHANGE beta_FR for beta_EU once it is grouped #####################
second_stage_EU_cov <- lm(beta_FR ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

second_stage_JP_cov <- lm(beta_JP ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        EU + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

summary(second_stage_CA_cov)
summary(second_stage_GB_cov)
summary(second_stage_EU_cov)
summary(second_stage_JP_cov)


###############################################################
## EVALUATION USING PLACEBO TEST

## For 2008 crisis period
set.seed(123)
df_08$randomPlacebo <- sample(df_08$crisis)

# We need to format the date for future use
df_08 <- df_08 %>%
  mutate(date = seq.Date(from = as.Date("1998-01-01"), by = "month", length.out = n()))

df_08$ShiftCrisis <- ifelse(df_08$date < "2006-01-01",
                            0, 1)

second_stage_CA <- lm(beta_CA ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + lag_GB + EU +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_08)

second_stage_GB <- lm(beta_GB ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + EU +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_08)

second_stage_EU <- lm(beta_EU ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_08)

second_stage_JP <- lm(beta_JP ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        EU + VIXCLS + WTISPLC + GSCPI, 
                      data = df_08)

summary(second_stage_CA)
summary(second_stage_GB)
summary(second_stage_EU)
summary(second_stage_JP)


## For COVID crisis period
set.seed(123)
df_covid$randomPlacebo <- sample(df_covid$crisis)

# We need to format the date for future use
df_covid <- df_covid %>%
  mutate(date = seq.Date(from = as.Date("1998-01-01"), by = "month", length.out = n()))

df_covid$ShiftCrisis <- ifelse(df_covid$date < "2006-01-01",
                            0, 1)

second_stage_CA_cov <- lm(beta_CA ~ crisis + Clus_inf_1 + Clus_inf_2
                          + rgdp + rgdp_growth + UNRATE + EXPINF1YR + lag_GB + EU +
                            JP + VIXCLS + WTISPLC + GSCPI, 
                          data = df_covid)

second_stage_GB_cov <- lm(beta_GB ~ crisis + Clus_inf_1 + Clus_inf_2
                          + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + EU +
                            JP + VIXCLS + WTISPLC + GSCPI, 
                          data = df_covid)

second_stage_EU_cov <- lm(beta_EU ~ crisis + Clus_inf_1 + Clus_inf_2
                          + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                            JP + VIXCLS + WTISPLC + GSCPI, 
                          data = df_covid)

second_stage_JP_cov <- lm(beta_JP ~ crisis + Clus_inf_1 + Clus_inf_2
                          + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                            EU + VIXCLS + WTISPLC + GSCPI, 
                          data = df_covid)

summary(second_stage_CA_cov)
summary(second_stage_GB_cov)
summary(second_stage_EU_cov)
summary(second_stage_JP_cov)


