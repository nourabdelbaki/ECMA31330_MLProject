# Nour Abdelbaki and Giuliana Triberti

## In this code, we run an OLS regression with a treatment term, where the
# treatment is the shock of the crisis. Once for 2008 Financial Crisis and 
# other COVID-19. 

# Import Libraries
library(zoo)
library(stargazer)

# From the kmeans approach.The best model was the one from the Time Series Clustering
# package, with centroids as "mean" (this is the same as the normal kmeans model), but
# using the distance "L2" which is the Euclidean distance. The best model was 
# chosen by comparing the Silhouette score instead of the Elbow mechanism, 
# to avoid heuristic rules. The best number of  clusters for inflation was found
# via cross validation (best no. clusters = 2). We then averaged the components
# within each cluster to help us run our necessary regressions. 

# A note on kmeans for monetary policy: we chose 3 clusters motivated 
# by the geographical regions of the countries. The cluster results aligned 
# exactly to what was expected and assigned a different cluster to different 
# regions.

# Read Datasets
inf_clust <- read.csv("inflation_clustered.csv")
data_08_all <- read.csv("data_08_all.csv")
data_covid_all <- read.csv("data_covid_all.csv")

# Fix date on datasets 
inf_clust$date <- inf_clust$Date %>%
  as.Date(.)%>%
  as.yearmon(., "%b %Y")

inf_clust <- inf_clust[,-1]

data_08_all$date <- data_08_all$date %>%
  as.yearmon(., "%b %Y")

data_covid_all$date <- data_covid_all$date %>%
  as.yearmon(., "%b %Y")

# Join datasets with inflation clusters
df_08 <- left_join(data_08_all, inf_clust, by = "date")
df_08<- df_08[,-1] # Drop duplicate row index 

df_covid <- left_join(data_covid_all, inf_clust, by = "date")
df_covid <- df_covid[,-1]


# For the regression, we will control using the clusters of inflation and
# monetary policy from other countries, as well as the domestic variables 
# that may have an effect on US monetary policy. 

# Run second-stage regression for 2008 crisis periods
OLS_CA_08 <- lm(beta_CA ~ crisis + Clus_inf_1 + Clus_inf_2
                   + rgdp + rgdp_growth + UNRATE + EXPINF1YR + lag_GB + EU +
                     JP + VIXCLS + WTISPLC + GSCPI, 
                   data = df_08)

OLS_GB_08 <- lm(beta_GB ~ crisis + Clus_inf_1 + Clus_inf_2
                   + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + EU +
                     JP + VIXCLS + WTISPLC + GSCPI, 
                   data = df_08)

OLS_EU_08 <- lm(beta_EU ~ crisis + Clus_inf_1 + Clus_inf_2
                    + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                      JP + VIXCLS + WTISPLC + GSCPI, 
                    data = df_08)

OLS_JP_08 <- lm(beta_JP ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        EU + VIXCLS + WTISPLC + GSCPI, 
                      data = df_08)

summary(OLS_CA_08)
stargazer(OLS_CA_08)

summary(OLS_GB_08)
stargazer(OLS_GB_08)

summary(OLS_EU_08)
stargazer(OLS_EU_08)

summary(OLS_JP_08)
stargazer(OLS_JP_08)

# Run second-stage regression for COVID crisis period
OLS_CA_covid <- lm(beta_CA ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + lag_GB + EU +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

OLS_GB_covid <- lm(beta_GB ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + EU +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

OLS_EU_covid <- lm(beta_EU ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        JP + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

OLS_JP_covid <- lm(beta_JP ~ crisis + Clus_inf_1 + Clus_inf_2
                      + rgdp + rgdp_growth + UNRATE + EXPINF1YR + CA + lag_GB +
                        EU + VIXCLS + WTISPLC + GSCPI, 
                      data = df_covid)

summary(OLS_CA_covid)
stargazer(OLS_CA_covid)

summary(OLS_GB_covid)
stargazer(OLS_GB_covid)

summary(OLS_EU_covid)
stargazer(OLS_EU_covid)

summary(OLS_JP_covid)
stargazer(OLS_JP_covid)
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

df_covid$ShiftCrisis <- ifelse(df_covid$date < "2010-01-01",
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