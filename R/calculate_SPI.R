####
##  Calculate SPI to "find" drought
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
library(SPEI)
library(RColorBrewer)

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

df <- convert_to_daily(df)




# Aggregate daily rainfall into monthly totals
precip_monthly <- df %>%
  group_by(Year=year(date), Month=month(date)) %>%
  summarise(Rainfall=sum(Precip, na.rm=TRUE), .groups="drop") %>%
  mutate(date=as.Date(paste(Year, Month, "01", sep="-")))

# Compute 3-month SPI
spi_3 <- spi(precip_monthly$Rainfall, scale=3)

# Add SPI values back to the monthly dataframe
precip_monthly <- precip_monthly %>%
  mutate(SPI_3 = spi_3$fitted)

df <- df %>%
  left_join(precip_monthly %>% select(date, SPI_3), by="date")

# Define drought classification based on SPI
df <- df %>%
  mutate(Drought = case_when(
    SPI_3 <= -1.6 ~ "Drought",
    TRUE ~ "No Drought"
  ))


# Create the time series plot of GPP with drought shading
ggplot(df, aes(x=date, y=GPP)) +
  # Add drought period shading based on the existing Drought classification
  geom_rect(data = df %>% filter(Drought == "Drought"), 
            aes(xmin=date, xmax=lead(date, default=last(date)), 
                ymin=-Inf, ymax=Inf), 
            fill = "red", alpha=0.2, inherit.aes=FALSE) +
 
  geom_line(color = "#66C2A5FF", linewidth = 1) +
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

plot(df$SPI_3)
