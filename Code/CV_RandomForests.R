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

# Motivated by our K-means clustering of monetary policy, where EU countries 
# ended up being clustered together, and by the fact that the EU Central Bank
# was founded in June 1998, where they started making uniformed interest rate
# decisions; we see a deviation in the first year or so of the foundation because
# Italy's rates were much higher, so they gradually came down to the EU-level
# and all their rates stayed consistent since then. So, we will average the
# interest rates of Germany (DE), France (FR), and Italy (IT) and rename it EU.
# This will really only have an effect on our first ~15-20 rows of data. 

data <- data %>%
  mutate(EU = rowMeans(select(., DE, FR, IT))) %>%
  select(-c(DE, FR, IT))
  
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
  set.seed(123)
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
#1    49        3   FALSE      259 0.2047037
#2    65        3   FALSE      259 0.2051887
#3    49        5   FALSE      259 0.2086128
#4    79        3   FALSE      259 0.2089355
#5    65        5   FALSE      259 0.2122589

set.seed(123)
rf_model <- randomForest(US ~ ., data = data, importance = TRUE,
                         ntree = p*10,
                         mtry = 49,
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
  set.seed(123)
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
#1    65        3   FALSE       95 0.1952897
#2    79        5   FALSE       95 0.1972430
#3    79        3   FALSE       95 0.1973314
#4    65        5   FALSE       95 0.1978486
#5    49        3   FALSE       95 0.2031262

set.seed(123)
RFmodel_b4_08 <- randomForest(US ~ ., data = datab4_08, importance = TRUE,
                              ntree = p*10,
                              mtry = 65,
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
  set.seed(123)
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
#1    29        5   FALSE      117 0.1937976
#2    49        5   FALSE      117 0.1949063
#3    49        3   FALSE      117 0.1950001
#4    65        3   FALSE      117 0.1970330
#5    79        5   FALSE      117 0.1971461

set.seed(123)
RFmodel_after_08 <- randomForest(US ~ ., data = dataAfter08, importance = TRUE,
                                 ntree = p*10,
                                 mtry = 29,
                                 nodesize = 5,
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
  set.seed(123)
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
#1    65        3   FALSE      103 0.07115987
#2    79        3   FALSE      103 0.07125893
#3    79        5   FALSE      103 0.07364892
#4    49        5   FALSE      103 0.07387483
#5    49        3   FALSE      103 0.07420892

set.seed(123)
RFmodel_b4_covid <- randomForest(US ~ .,
                                 data = datab4_covid, importance = TRUE,
                                 ntree = p*10,
                                 mtry = 65,
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
  set.seed(123)
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
#1    65        5   FALSE       46 0.1881848
#2    65        3   FALSE       46 0.1896140
#3    49        5   FALSE       46 0.1901565
#4    79        5   FALSE       46 0.1964010
#5    79        3   FALSE       46 0.2007996

set.seed(123)
RFmodel_after_covid <- randomForest(US ~ .,
                                    data = dataAfterCovid, importance = TRUE,
                                    ntree = p*10,
                                    mtry = 65,
                                    nodesize = 5,
                                    replace = FALSE,
                                    sampsize = 46,
                                    keep.forest = TRUE)

saveRDS(RFmodel_after_covid, file = "Models/RFmodel_after_covid.rds")