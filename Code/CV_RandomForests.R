#     Nour Abdelbaki & Giuliana Triberti
## In this code, we are cross-validating our random forests for each time period.
# Then, running the model and saving the model object to use for our analysis. 

# Import Libraries
library(dplyr)
library(tidyr)
library(zoo)
library(randomForest)

# Set seed for reproducibility
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
  # save rmse as the sqrt of the last MSE in the vector as it is the MSE 
  # of the entire ensemble/after growing all trees
  hyper_grid$rmse[i] <- sqrt(tail(fit$mse, 1)) 
}

hyper_grid_all <- hyper_grid %>%
  arrange(rmse)

head(hyper_grid_all, 10)
#     mtry nodesize replace sampsize      rmse
#1    50        3   FALSE      259    0.2039051
#2    66        3   FALSE      259    0.2063256
#3    50        5   FALSE      259    0.2065509
#4    30        3   FALSE      259    0.2082388
#5    80        3   FALSE      259    0.2095830

rf_model <- randomForest(US ~ ., data = data, importance = TRUE,
                         ntree = p*10,
                         mtry = 50,
                         nodesize = 3,
                         replace = FALSE,
                         sampsize = 259,
                         keep.forest = TRUE)

saveRDS(rf_model, file = "Models/RF_model_ALL.rds")
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
  hyper_grid_b4_08$rmse[i] <- sqrt(tail(fit$mse, 1))
}

hyper_grid_b4_08 <- hyper_grid_b4_08 %>%
  arrange(rmse)

head(hyper_grid_b4_08, 10)
#     mtry nodesize replace sampsize      rmse
#1    80        3   FALSE       95    0.1948956
#2    66        3   FALSE       95    0.1958852
#3    66        5   FALSE       95    0.1959256
#4    80        5   FALSE       95    0.1967450
#5    50        3   FALSE       95    0.1973288

RFmodel_b4_08 <- randomForest(US ~ ., data = datab4_08, importance = TRUE,
                              ntree = p*10,
                              mtry = 80,
                              nodesize = 3,
                              replace = FALSE,
                              sampsize = 95,
                              keep.forest = TRUE)
saveRDS(RFmodel_b4_08, file = "Models/RF_model_b4_08.rds")

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
  hyper_grid_after_08$rmse[i] <- sqrt(tail(fit$mse, 1))
}

hyper_grid_after_08 <- hyper_grid_after_08 %>%
  arrange(rmse)

head(hyper_grid_after_08, 10)
#     mtry nodesize replace sampsize      rmse
#1    50        3   FALSE      117    0.1900644
#2    80        3   FALSE      117    0.1900901
#3    30        3   FALSE      117    0.1915321
#4    66        3   FALSE      117    0.1931518
#5    66        5   FALSE      117    0.1952007

RFmodel_after_08 <- randomForest(US ~ ., data = dataAfter08, importance = TRUE,
                                 ntree = p*10,
                                 mtry = 50,
                                 nodesize = 3,
                                 replace = FALSE,
                                 sampsize = 117,
                                 keep.forest = TRUE)
saveRDS(RFmodel_after_08, file = "Models/RFmodel_after_08.rds")

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
  hyper_grid_b4_covid$rmse[i] <- sqrt(tail(fit$mse, 1))
}

hyper_grid_b4_covid <- hyper_grid_b4_covid %>%
  arrange(rmse)

head(hyper_grid_b4_covid, 10)
#     mtry nodesize replace sampsize      rmse
#1    66        3   FALSE      103 0.07039257
#2    80        3   FALSE      103 0.07153476
#3    66        5   FALSE      103 0.07340228
#4    50        5   FALSE      103 0.07360984
#5    80        5   FALSE      103 0.07386279

RFmodel_b4_covid <- randomForest(US ~ .,
                                 data = datab4_covid, importance = TRUE,
                                 ntree = p*10,
                                 mtry = 66,
                                 nodesize = 3,
                                 replace = FALSE,
                                 sampsize = 103,
                                 keep.forest = TRUE)
saveRDS(RFmodel_b4_covid, file = "Models/RFmodel_b4_covid.rds")

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
  hyper_grid_after_covid$rmse[i] <- sqrt(tail(fit$mse, 1))
}

hyper_grid_after_covid <- hyper_grid_after_covid %>%
  arrange(rmse)

head(hyper_grid_after_covid, 10)
#     mtry nodesize replace sampsize      rmse
#mtry nodesize replace sampsize      rmse
#1    30        5   FALSE       46 0.1693780
#2    50        3   FALSE       46 0.1696800
#3    80        3   FALSE       46 0.1768711
#4    80        5   FALSE       46 0.1775708
#5    66        5   FALSE       46 0.1807041

RFmodel_after_covid <- randomForest(US ~ .,
                                    data = dataAfterCovid, importance = TRUE,
                                    ntree = p*10,
                                    mtry = 30,
                                    nodesize = 5,
                                    replace = FALSE,
                                    sampsize = 46,
                                    keep.forest = TRUE)

saveRDS(RFmodel_after_covid, file = "Models/RFmodel_after_covid.rds")