####
##  Fit a GAM and a Random forest to predict flux GPP, make a comparison plot
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
library(RColorBrewer)

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

# Rescale the training and test data predictors
predictors <- c("Tair", "SW_rad", "VPD", "cumulative_cwd_7day")  
train_data <- standardise(train_data, predictors)
test_data <- standardise(test_data, predictors)

# Train the random forest model
model <- randomForest(GPP ~ Tair + SW_rad + VPD + cumulative_cwd_7day, 
                      data=trainData, 
                      ntree=500, mtry=3)
print(model)

# Predict on the testing data
predictions_rf <- predict(model, test_data)
results <- data.frame(Observed=test_data$GPP, Predicted=predictions_rf)

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


###
# GAM
###

gam_model <- gam(GPP ~ s(Tair) + s(VPD) + s(cumulative_cwd_7day), 
                 data=train_data, method = "REML")

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


# Compare the GAM to the the RF
results_combined <- data.frame(
  date=as.Date(test_data$date),  
  Observed=test_data$GPP,
  Predicted_RF=predictions_rf,
  Predicted_GAM=predictions_gam
)

# Plot Observed GPP, Random Forest Predicted, and GAM Predicted
ggplot(results_combined, aes(x=date)) +  
  geom_line(aes(y=Observed, color="Observed"), linewidth=1) +  
  geom_line(aes(y=Predicted_RF, color="Random Forest"), linewidth=1) +  
  geom_line(aes(y=Predicted_GAM, color="GAM"), linewidth=1) +  
  ylab(expression("GPP" ~ ("g C" ~ m^{-2} ~ d^{-1}))) +
  xlab(expression(' ')) +
  scale_color_manual(values=brewer.pal(3, "Set2")) +  
  ylim(0, 10) +  # Set y-axis range from 0 to 10
  theme_classic() +  
  theme(
    legend.title=element_blank(),
    legend.position=c(0.8, 0.9),
    legend.background=element_rect(fill = alpha("white", 0.5))
  )


