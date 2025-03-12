#     Nour Abdelbaki & Giuliana Triberti

## Load libraries
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(purrr)
library(zoo)
#library(randomForest)
library(ggplot2)
library(corrplot)
#library(reshape2)
#library(factoextra)
#library(FactoMineR)
#library(stringr)
#library(lfe)  # For fixed-effects regression

set.seed(123)

## Set working directory
setwd("~/Desktop/MACSS-Econ/Winter 2025/ECMA 31330/ECMA31330_MLProject")

## Read dataset
data <- read.csv("1998_G7_US.csv")
############################################################################
#  Descriptive Statistics
###########################################################################

#### ADD HERE PLOTS AND/OR TABLES FOR WRITE UP
##### TO FIX!!!!!!! 
############################################################################
#  Correlations
###########################################################################
### Compute correlation matrix for all variables
mat_data <- as.matrix(data[,-1])  # Exclude date column

cor_matrix <- cor(mat_data, use = "complete.obs")  # Full correlation matrix

# Extract correlations for external monetary policy, US, and other variables:
# Select monetary policy (external and US) variables:
selected_vars <- colnames(mat_data)[5:10]
# Keep all columns, but only rows from selected variables:
cor_subset <- cor_matrix[selected_vars, ]

# Convert to long format
cor_df <- melt(cor_subset)
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

# Convert factors to character to avoid level mismatch
cor_df$Var1 <- as.character(cor_df$Var1)
cor_df$Var2 <- as.character(cor_df$Var2)

# Remove self-correlations (Var1 == Var2)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]

# Ensure each pair appears only once
cor_df <- cor_df %>%
  mutate(pair = pmin(Var1, Var2)) %>%
  mutate(pair = paste(pair, pmax(Var1, Var2), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE)

# Select top 50 absolute correlations
top_cor <- cor_df[order(-abs(cor_df$Correlation)), ][1:50, ]

# Plot the highest 50 correlations
ggplot(top_cor, aes(x = reorder(paste(Var1, Var2, sep = " - "),
                                abs(Correlation)), y = Correlation,
                    fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 50 Correlations of Selected Variables with All Variables",
       x = "Variable Pair", y = "Correlation")

## Quite informative and insightful for next steps! 

### Compute correlation matrix for inflation components and each other
selected_vars2 <- colnames(mat_data)[13:104] # Current inflation components
cor_subset2 <- cor_matrix[selected_vars2, ]

# Convert to long format
cor_df2 <- melt(cor_subset2)
colnames(cor_df2) <- c("Var1", "Var2", "Correlation")

# Convert factors to character to avoid level mismatch
cor_df2$Var1 <- as.character(cor_df2$Var1)
cor_df2$Var2 <- as.character(cor_df2$Var2)

# Remove self-correlations (Var1 == Var2)
cor_df2 <- cor_df2[cor_df2$Var1 != cor_df2$Var2, ]

# Ensure each pair appears only once
cor_df2 <- cor_df2 %>%
  mutate(pair = pmin(Var1, Var2)) %>%
  mutate(pair = paste(pair, pmax(Var1, Var2), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE)

# Select top 50 absolute correlations
top_cor2 <- cor_df2[order(-abs(cor_df2$Correlation)), ][1:50, ]

# Plot the highest 50 correlations
ggplot(top_cor2, aes(x = reorder(paste(Var1, Var2, sep = " - "), 
                                 abs(Correlation)),
                     y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Top 50 Correlations of Inflation Components",
       x = "Variable Pair", y = "Correlation")

## Makes sense but not very informative!

############################################################################
#  OLS- All Data
###########################################################################
full_model <- lm(US ~., data = data[,-1]) #w/o date column

summary(full_model)

# Create a LaTEX table of those who are significant for Appendix and commentary
##### TO FIX!!!!!!! 
############################################################################
#  OLS- Before '08 and after '08
###########################################################################
datab4_08 <- data[data$date < as.yearmon("Jan 2008", "%b %Y"),]

dataAfter08 <- data[!data$date < as.yearmon("Jan 2008", "%b %Y"),]

b4_08_ols <- lm(US ~., data =datab4_08[,-1])

summary(b4_08_ols)

### Will not run OLS bc N < p!!! (add to our why ML)

after_08_ols <- lm(US ~., data =dataAfter08[,-1])

summary(after_08_ols)
# Create a LaTEX table of those who are significant for Appendix and commentary
##### TO FIX!!!!!!! 

############################################################################
#  OLS- Before COVID-19 and after COVID-19
###########################################################################
datab4_covid <- data[data$date < as.yearmon("March 2020", "%b %Y"),]

dataAfterCovid <- data[!data$date < as.yearmon("March 2020", "%b %Y"),]

before_covid_OLS <- lm(US ~., data =datab4_covid[,-1])

summary(before_covid_OLS)

after_covid_OLS <- lm(US ~., data =dataAfterCovid[,-1])

### Will not run full OLS bc N < p!!! (add to our why ML)

### To avoid curse of dimensionality with this comparison, let's run a more
# reduced regression of just the US on external monetary policy as well as 
# inflation expectation, unemployment, rgdp growth and log rgdp, which is 
# of course a very flawed  regression specification but could be helpful in 
# exploration

# dataAfterCovid[c(2:11, 108)]

before_covid_reduced <- lm(US ~., data =datab4_covid[,c(2:11, 108)])

summary(before_covid_reduced)

after_covid_reduced <- lm(US ~., data =dataAfterCovid[,c(2:11, 108)])

summary(after_covid_reduced)

# Not sure if very informative for our RQ/could just mention we did it, but it
# did not add much for our analysis moving forward.

############################################################################
#  Kmeans- Inflation components
###########################################################################
## To figure out the ideal number of clusters:
# ref: https://www.datacamp.com/tutorial/k-means-clustering-r
## Would be better to use something that capitalizes on the time element:
# ref: https://levelup.gitconnected.com/unveiling-patterns-in-time-a-guide-to-time-series-clustering-with-tslearn-50a2ff305afe
# Above is in python, so we found similar package in R, but intuition from above
# ref: https://cran.r-project.org/web/packages/dtwclust/index.html and
# https://journal.r-project.org/archive/2019/RJ-2019-023/RJ-2019-023.pdf 

set.seed(123)

############################################################################
#  Kmeans- Countries
###########################################################################