#     Nour Abdelbaki & Giuliana Triberti
## In this code, we are evaluating our random forests' models. ADD DESCRIPTION
# HERE 

# Import Libraries
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(randomForest)
library(reshape2)
set.seed(123)

## Set working directory
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject")

## Read dataset
data <- read.csv("1998_G7_US.csv")
############################################################################
#  Random Forests- Load in the different models
###########################################################################

