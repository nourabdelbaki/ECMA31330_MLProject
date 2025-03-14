#     Nour Abdelbaki & Giuliana Triberti
## In this code, we are cross-validating our random forests for each time period.

# Import Libraries
library(data.table)
library(dplyr)
library(tidyr)
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
library(DALEX) # SHAP 
library(pdp) # Partial Dependence Plot
set.seed(123)

## Set working directory
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject")

## Read dataset
data <- read.csv("1998_G7_US.csv")

###### Random Forests ###### 
# ref: https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
############################################################################
#  Random Forests- Entire Dataset- Cross Validation 
###########################################################################
# Cross-validation: 
# Ref: https://bradleyboehmke.github.io/HOML/random-forest.html

p <- dim(data)[2] - 1 #Total number of features w/o US data
n <- dim(data)[1]

hyper_grid <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 10 MINS TO RUN!! 
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = data, ntree = p * 10, 
                      mtry = hyper_grid$mtry[i], 
                      nodesize = hyper_grid$nodesize[i],
                      replace = hyper_grid$replace[i],
                      sampsize = hyper_grid$sampsize[i]
  )
  # save rmse
  hyper_grid$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_all <- hyper_grid %>%
  arrange(rmse)

head(hyper_grid_all, 10)
#     mtry nodesize replace sampsize      rmse
#1    80       15   FALSE      259    0.3479939
#2    66       15    TRUE      259    0.4057546
#3    30        3    TRUE      162    0.4061437
#4    30        3   FALSE      259    0.4272611

############################################################################
#  Random Forests- Before and After 2008- Cross Validation 
###########################################################################
# Datasets:
# Officially the Great Recession/2008 Financial Crisis lasted from Dec 2007
# to June 2009. We chose the after '08 dataset to also end before the 
# COVID-19 crisis. 
datab4_08 <- data[data$date < as.yearmon("Dec 2007", "%b %Y"),] 
dataAfter08 <- data[data$date >=  as.yearmon("Dec 2007", "%b %Y") &
                      data$date < as.yearmon("March 2020", "%b %Y"), ]

#### Cross-validation before 2008:
n_08 <- dim(datab4_08)[1]

hyper_grid_b4_08 <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_08* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 4 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_b4_08))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = datab4_08, ntree = p * 10, 
                      mtry = hyper_grid_b4_08$mtry[i], 
                      nodesize = hyper_grid_b4_08$nodesize[i],
                      replace = hyper_grid_b4_08$replace[i],
                      sampsize = hyper_grid_b4_08$sampsize[i]
  )
  # save rmse
  hyper_grid_b4_08$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_b4_08 <- hyper_grid_b4_08 %>%
  arrange(rmse)

head(hyper_grid_b4_08, 10)
#     mtry nodesize replace sampsize      rmse
#1    66        5   FALSE       95    0.2325632
#2    80       15   FALSE       74    0.2668251
#3    80        3   FALSE       95    0.3233085
#4    80       15    TRUE       95    0.3263221
#5    50        5   FALSE       95    0.3424027

#### Cross-validation after 2008:
n_08_2 <- dim(dataAfter08)[1]

hyper_grid_after_08 <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_08_2* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 6 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_after_08))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = dataAfter08, ntree = p * 10, 
                      mtry = hyper_grid_after_08$mtry[i], 
                      nodesize = hyper_grid_after_08$nodesize[i],
                      replace = hyper_grid_after_08$replace[i],
                      sampsize = hyper_grid_after_08$sampsize[i]
  )
  # save rmse
  hyper_grid_after_08$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_after_08 <- hyper_grid_after_08 %>%
  arrange(rmse)

head(hyper_grid_after_08, 10)
#     mtry nodesize replace sampsize      rmse
#1    30        5   FALSE      117    0.1070436
#2    66       10   FALSE       73    0.1078672
#3    10        5   FALSE      117    0.1894131
#4    50       10    TRUE      117    0.1940878
#5    50        5   FALSE       92    0.2224923
############################################################################
#  Random Forests- Before and After COVID-19- Cross Validation 
###########################################################################
# Datasets:
# Officially the Great Recession/2008 Financial Crisis lasted from Dec 2007
# to June 2009. We chose the before covid dataset to be from after June 2009 
# to before March 2020. And after to be from March 2020 onwards. 
datab4_covid <- data[data$date >= as.yearmon("June 2009", "%b %Y") &
                       data$date < as.yearmon("March 2020", "%b %Y"),] 
dataAfterCovid <- data[data$date >=  as.yearmon("March 2020", "%b %Y"), ]

#### Cross-validation before COVID-19:
n_covid <- dim(datab4_covid)[1]

hyper_grid_b4_covid <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_covid* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 5 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_b4_covid))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = datab4_covid, ntree = p * 10, 
                      mtry = hyper_grid_b4_covid$mtry[i], 
                      nodesize = hyper_grid_b4_covid$nodesize[i],
                      replace = hyper_grid_b4_covid$replace[i],
                      sampsize = hyper_grid_b4_covid$sampsize[i]
  )
  # save rmse
  hyper_grid_b4_covid$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_b4_covid <- hyper_grid_b4_covid %>%
  arrange(rmse)

head(hyper_grid_b4_covid, 10)
#     mtry nodesize replace sampsize      rmse
#1    10        5   FALSE      103      0.02451452
#2    80        5   FALSE      103      0.08492078
#3    66        5    TRUE       64      0.09494432
#4    66        5   FALSE       81      0.09547033
#5    80       10   FALSE      103      0.10071857

#### Cross-validation after COVID-19:
n_covid_2 <- dim(dataAfterCovid)[1]

hyper_grid_after_covid <- expand.grid(
  mtry = floor(p * c(.05, .15, .25, .333, .4)),
  nodesize = c(3, 5, 10, 15),
  replace = c(TRUE, FALSE),                            
  sampsize = floor(n_covid_2* c(.5, .63, .8)),             
  rmse = NA                                              
)

### TAKES AROUND 2 MINS TO RUN:
for(i in seq_len(nrow(hyper_grid_after_covid))) {
  # fit model for ith hyperparameter combination
  fit <- randomForest(formula= US ~ ., data = dataAfterCovid, ntree = p * 10, 
                      mtry = hyper_grid_after_covid$mtry[i], 
                      nodesize = hyper_grid_after_covid$nodesize[i],
                      replace = hyper_grid_after_covid$replace[i],
                      sampsize = hyper_grid_after_covid$sampsize[i]
  )
  # save rmse
  hyper_grid_after_covid$rmse[i] <- sqrt(fit$mse)
}

hyper_grid_after_covid <- hyper_grid_after_covid %>%
  arrange(rmse)

head(hyper_grid_after_covid, 10)
#     mtry nodesize replace sampsize      rmse
#1    80        5   FALSE       46    0.05705443
#2    10       15   FALSE       36    0.13846380
#3    80        3   FALSE       36    0.17699976
#4    10        3   FALSE       46    0.20728905
#5    66       15   FALSE       46    0.20956426