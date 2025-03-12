#     Nour Abdelbaki & Giuliana Triberti

### USA database ###
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(purrr)
library(zoo)

#Import datasets
#setwd("C:/Users/HP/Downloads/UChicago/1. Courses/2. Winter Quarter 2025/2.3 MACSS 31330 Econometrics and Machine Learning/Final project/database/USA")
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject/USA")
############################################################################
#  Inflation_components
###########################################################################
inf <- fread("Inflation_components_reduced.csv", na.strings = "---")
inf <- inf[4:nrow(inf),3:ncol(inf)]

# Extract year and month
year <- as.character(unlist(inf[1, ]))  # Convert first row into a vector
month <- as.character(unlist(inf[2, ])) # Convert second row into a vector

# Create date for each column in yearmon class to avoid adding artificial day
date <- as.yearmon(paste(month, year), "%b %Y")

# Remove first two rows
inf_df <- inf[-c(1, 2), ]

# Transpose data and turn into a dataframe
transposed_inf_df <- transpose(inf_df)
transposed_inf_df <- as.data.table(transposed_inf_df)

# Assign the first column to be the dates
transposed_inf_df <- cbind(date, transposed_inf_df)

# Rename the remaining columns as "inf_1", "inf_2", ...
new_colnames <- paste0("inf_", seq_len(ncol(transposed_inf_df) - 1))
setnames(transposed_inf_df, old = names(transposed_inf_df)[-1], new = new_colnames)

#Format date 
#transposed_inf_df$date <- as.Date(transposed_inf_df$date, format = "%Y-%m-%d")

#Format numbers
transposed_inf_df <- transposed_inf_df %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

# Dropping the subcomponents that's all NA (inf_43 and inf_73)
transposed_inf_df <- transposed_inf_df %>%
  select_if(~!all(is.na(.))) 

# inf_43 is Net expenditures abroad by U.S. residents (131)
# inf_47 is Net foreign travel
##############################################################################
#  Unemployment rate
##############################################################################
un_rate <- fread("UNRATE.csv", na.strings = "---")
setnames(un_rate, old = "observation_date", new = "date")
un_rate$date <- as.yearmon(un_rate$date, "%b %Y")

##############################################################################
#  Real GDP Growth, from preceding period
##############################################################################
rgdp <- fread("RGDP_Growth.csv")
setnames(rgdp, old = "observation_date", new = "date")
rgdp$date <- rgdp$date %>% as.Date(., format = "%d/%m/%Y") %>%
  as.yearmon(., "%b %Y")
setnames(rgdp, old = "GDP", new = "rgdp_growth")

gdp_dollars <- fread("RGDP_dollars.csv")
setnames(gdp_dollars, old = "observation_date", new = "date")
gdp_dollars$date <- gdp_dollars$date %>% as.Date(., format = "%Y/%m/%d") %>%
  as.yearmon(., "%b %Y")
setnames(gdp_dollars, old = "GDPC1", new = "rgdp")

rgdp <- left_join(rgdp, gdp_dollars, by = "date")
##############################################################################
#  Inflation expectation
##############################################################################
inf_exp <- fread("EXPINF1YR.csv", na.strings = "---")
setnames(inf_exp, old = "observation_date", new = "date")
inf_exp$date <- as.yearmon(inf_exp$date, "%b %Y")

##############################################################################
#  G20 monetary policy rates
##############################################################################
# Read the CSV file
irate <- fread("G20_monetary_pol.csv", na.strings = "---")

# Transpose the data table
irate <- t(irate)
irate <- as.data.table(irate)

# Remove the first 3 columns (descriptions provided from the page)
irate <- irate[, -c(1,2,3)]

# Set the column names using the original column names (country codes)
colnames(irate) <- as.character(unlist(irate[1, ]))

colnames(irate)[colnames(irate) == "REF_AREA"] <- "date" 

#Remove the first row
irate <- irate[-1, ]

#Put the date format instead of list
irate[[1]] <- as.Date(unlist(irate[[1]]), format = "%d/%m/%Y")

#Mutate data so it is a number
irate <- irate %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))
##############################################################################
# G7 Monetary Policy Rates 
##############################################################################
# Read the CSV file
irate7 <- fread("G7_monetary_pol.csv", na.strings = "---")

irate7 <- irate7 %>%
  t(.) %>%
  as.data.table(.)

# Set the column names using the original column names (country codes)
colnames(irate7) <- as.character(unlist(irate7[1, ]))

colnames(irate7)[colnames(irate7) == "REF_AREA"] <- "date" 

#Remove the first row
irate7 <- irate7[-1, ]

#Put the date format instead of list
irate7[[1]] <- as.Date(unlist(irate7[[1]]), format = "%Y-%m-%d")
irate7$date <- as.yearmon(irate7$date, "%b %Y")

#Mutate data so it is a number
irate7 <- irate7 %>%
  mutate(across(-1, ~ as.numeric(as.character(.))))

# Fill out Italy, Germany, and France rates using the EU rate starting '98-99
# bc it was the time EU Central bank was founded, and they started having unified
# monetary policy decisions. 

EU99 <- fread("EU99.csv")

EU99 <- EU99 %>%
  t(.) %>%
  as.data.table(.) %>%
  .[-1, ] %>% # Drop first row
  .[, -(1:3), with = FALSE]  %>% # Drop first 3 columns bs descriptions
  setnames(., old = c(names(.)[1], names(.)[2]), new = c("date", "EU")) %>%
  mutate(date = as.yearmon(as.Date(date, format = "%m/%d/%y"), "%b %Y")) %>%
  mutate(EU = as.numeric(EU))

# Define columns to fill
fill_cols <- c("FR", "IT", "DE")

# Join and selectively fill NAs
irate7 <- irate7 %>%
  left_join(EU99, by = "date") %>%
  mutate(across(all_of(fill_cols), ~ coalesce(.x, EU))) %>%
  select(-EU)

# Fill out missing entries for JP using another datasource (FRED)
jp_supp <- fread("JP_supplement_irate.csv")
jp_supp <- jp_supp %>%  
  setnames(., old = c(names(.)[1], names(.)[2]), new = c("date", "JP_supp")) %>%
  mutate(date = as.yearmon(as.Date(date, format = "%Y/%m/%d"), "%b %Y")) %>%
  mutate(JP_supp = as.numeric(JP_supp))

irate7$JP <- coalesce(irate7$JP, jp_supp$JP_supp[match(irate7$date,
                                                       jp_supp$date)])

##############################################################################
#  WTI oil price
##############################################################################
wti <- fread("WTISPLC.csv", na.strings = "---")
setnames(wti, old = "observation_date", new = "date")
wti$date <- as.yearmon(wti$date, "%b %Y")

##############################################################################
#  Global Supply Chain Pressure
##############################################################################
gscpi <- fread("gscpi_data.csv")
gscpi <- gscpi[5:nrow(gscpi), 1:2]
setnames(gscpi, old = "Date", new = "date")

gscpi$date <- gscpi$date %>%  
  as.Date(., format = "%d/%m/%Y") %>%
  as.yearmon(., "%b %Y")

##############################################################################
#  VIX
##############################################################################
vix <- fread("VIXCLS.csv")
setnames(vix, old = "observation_date", new = "date")
vix$date <- as.yearmon(vix$date, "%b %Y")

##############################################################################
#### Merged Data: G7 Monthly, 1990
##############################################################################
datasets <- list(un_rate, inf_exp, rgdp, irate7, wti, vix,
                 transposed_inf_df)

merged_data <- reduce(datasets, left_join, by = "date")

# Interpolate GDP & GDP Growth from Quarterly to Monthly
# using simple linear interpolation
# Ref:https://www.rdocumentation.org/packages/zoo/versions/1.8-12/topics/na.approx
merged_data$rgdp <- na.approx(merged_data2$rgdp, rule = 2)
merged_data$rgdp_growth <- na.approx(merged_data2$rgdp_growth, rule = 2)

# Lagging the inflation, rgdp, rgdp_growth, and inflation components variables
# as well as GB because they typically make their monetary policy decision 
# after the US does. So, US only observes the past interest rate decision of GB.
to_lag_vars <- colnames(merged_data)
to_lag_vars <- to_lag_vars[-c(1,2,3,6,7,8,10,11,12,13,14)]

# Create a lagged dataset
merged_data <- merged_data %>%
  mutate(across(all_of(to_lag_vars), ~lag(.x, 1), 
                .names = "lag_{.col}")) %>%  # Lag selected variables
  select(-c("GB")) %>%
  na.omit()

# Subset for all the observations of Jan 1990 and after
merged_data <- merged_data[merged_data$date >= as.yearmon("Jan 1990", "%b %Y"), ]

# Drop Jan 2025 observation bc no inflation data yet
merged_data <- merged_data[1:(nrow(merged_data)-1),]

write.csv(merged_data2, "1990_G7_US.csv", row.names = FALSE)
##############################################################################
#### Merged Data-2 G7 Monthly, 1998 (+Global Supply Chain Presence)
##############################################################################
merged_data2 <- left_join(merged_data, gscpi, by="date")

# Subset for all the observations of Jan 1998 and after
merged_data2 <- merged_data2[merged_data2$date >= as.yearmon("Jan 1998", "%b %Y"), ]

write.csv(merged_data2, "1998_G7_US.csv", row.names = FALSE)