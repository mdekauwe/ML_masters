####
##  Fit a random forest to predict GPP, here we're testing the use of lags
##
##  author: Martin De Kauwe
##  date: 27th February, 2025
##  email: mdekauwe@gmail.com
####

library(tidyverse)
library(caret)
library(randomForest)
library(zoo)
library(mgcv)

source("R/read_flux_file.R")
source("R/convert_to_daily.R")
source("R/PET.R")

standardise <- function(df, vars_to_scale) {
  df_scaled <- df
  df_scaled[vars_to_scale] <- scale(df[vars_to_scale])
  
  return(df_scaled)
}

setwd("./")
fname = "data/FLX_FR-Pue_FLUXNET2015_FULLSET_HH_2000-2014_2-4.csv"

df = read_flux(fname)
print(df)

# bit brutal, someone might want to take more care, just exclude missing days
df <- df %>%
  filter_all(all_vars(. >= -9000))

#df <- df %>%
#  mutate(Rn = ifelse(Rn < 0, 0, Rn))

df <- convert_to_daily(df)

# Add cumulative water deficit to the dataframe
df$pet <- priestley_taylor_pet(df$Rn, df$Tair, df$Pa)
df$cwd <- df$pet - df$ET

# Probably not needed, but I don't want to mess around tidying up the data
df <- df %>% drop_na()


###
# Random forest
###

set.seed(123)  # for reproducibility

# Split data in test and training 
idx <- createDataPartition(df$GPP, p=0.8, list=FALSE)
train_data <- df[idx, ]
test_data <- df[-idx, ]

# Drop any rows with NA values in testData
test_data <- test_data %>% drop_na()

# Create a 7-day moving cumulative sum for CWD
train_data$cumulative_cwd_7day <- rollapply(train_data$cwd, width=7, FUN=sum, 
                                            align="right", fill=NA, na.rm=TRUE)
test_data$cumulative_cwd_7day <-  rollapply(test_data$cwd, width=7, FUN=sum, 
                                            align="right", fill=NA, na.rm=TRUE)

# Drop any rows with NA values after the rolling sum calculations
train_data <- train_data %>% drop_na()
test_data <- test_data %>% drop_na()

# Create lagged variables for Tair and VPD, something like a 3-day HW
train_data <- train_data %>%
  mutate(
    Tair_lag1 = lag(Tair, 1),
    Tair_lag2 = lag(Tair, 2),
    Tair_lag3 = lag(Tair, 3),
    VPD_lag1 = lag(VPD, 1),
    VPD_lag2 = lag(VPD, 2),
    VPD_lag3 = lag(VPD, 3)
  ) %>%
  drop_na()  # remove rows with any NA values after creating lags

test_data <- test_data %>%
  mutate(
    Tair_lag1 = lag(Tair, 1),
    Tair_lag2 = lag(Tair, 2),
    Tair_lag3 = lag(Tair, 3),
    VPD_lag1 = lag(VPD, 1),
    VPD_lag2 = lag(VPD, 2),
    VPD_lag3 = lag(VPD, 3)
  ) %>%
  drop_na()  # remove rows with any NA values after creating lags



# Rescale the training and test data predictors
predictors <- c("Tair", "SW_rad", "VPD", "cumulative_cwd_7day",
                "Tair_lag1", "Tair_lag2", "Tair_lag3", 
                "VPD_lag1", "VPD_lag2", "VPD_lag3")

train_data <- standardise(train_data, predictors)
test_data <- standardise(test_data, predictors)

# Train the random forest model
model <- randomForest(GPP ~ Tair + SW_rad + VPD + cumulative_cwd_7day + 
                        Tair_lag1 + Tair_lag2 + Tair_lag3 + 
                        VPD_lag1 + VPD_lag2 + VPD_lag3, 
                      data=train_data, ntree=500, mtry=3)
print(model)

# Predict on the testing data
predictions <- predict(model, test_data)
results <- data.frame(Observed=test_data$GPP, Predicted=predictions)

# Dumps some stats
rmse <- sqrt(mean((results$Observed - results$Predicted)^2))
rsq <- cor(results$Observed, results$Predicted)^2
mean_observed <- mean(results$Observed)
nse <- (1 - sum((results$Observed - results$Predicted)^2) / 
          sum((results$Observed - mean_observed)^2))

cat("RMSE:", rmse, "\n")
cat("R-squared:", rsq, "\n")
cat("Nash-Sutcliffe Efficiency (NSE):", nse, "\n")

ggplot(results, aes(x=Observed, y=Predicted)) +
  geom_point() +
  geom_abline(intercept=0, slope=1, color="red") +
  theme_minimal() +
  labs(x="Observed GPP", y="Predicted GPP")


importance_values <- importance(model)
print(importance_values)

# the higher the score, the more important the variable is in predicting GPP
varImpPlot(model, main = "")

# the model with the lower Out-of-Bag (OOB) MSE is better.
print(simpler_model$mse[length(simpler_model$mse)])  
print(model$mse[length(model$mse)])  
