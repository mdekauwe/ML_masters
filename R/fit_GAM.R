library(tidyverse)
library(caret)
library(randomForest)
library(zoo)
library(mgcv)

source("R/read_flux_file.R")
source("R/convert_to_daily.R")
source("R/PET.R")

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

# Lag the variables to add time-lag effects
train_data <- train_data %>%
  mutate(
    Tair_lag3 = lag(Tair, 3),
    VPD_lag3 = lag(VPD, 3)
  ) %>%
  drop_na()

test_data <- test_data %>%
  mutate(
    Tair_lag3 = lag(Tair, 3),
    VPD_lag3 = lag(VPD, 3)
  ) %>%
  drop_na()

gam_model <- gam(GPP ~ s(Tair) + s(VPD) + s(cumulative_cwd_7day) + 
                   s(Tair_lag3) + s(VPD_lag3), data=train_data, 
                 method = "REML")

summary(gam_model)


predictions_gam <- predict(gam_model, newdata=test_data)
results_gam <- data.frame(Observed=test_data$GPP, Predicted=predictions_gam)


# Dumps some stats
rmse_gam <- sqrt(mean((results_gam$Observed - results_gam$Predicted)^2))
rsq_gam <- cor(results_gam$Observed, results_gam$Predicted)^2
mean_observed_gam <- mean(results_gam$Observed)
nse_gam <- (1 - sum((results_gam$Observed - results_gam$Predicted)^2) / 
              sum((results_gam$Observed - mean_observed_gam)^2))
cat("RMSE:", rmse_gam, "\n")
cat("R-squared:", rsq_gam, "\n")
cat("Nash-Sutcliffe Efficiency (NSE):", nse_gam, "\n")

