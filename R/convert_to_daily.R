####
##  Convert the 30 min to daily ... someone should add something to check
##. if the data is hourly ...
##
##
##  author: Martin De Kauwe
##  date: 27th February, 2025
##  email: mdekauwe@gmail.com
####
library(tidyverse)

convert_to_daily <- function(df) {

  # Conversion constants
  UMOL_TO_MOL <- 1E-6
  MOL_C_TO_GRAMS_C <- 12.0
  SEC_2_HLFHOUR <- 1800.
  WM2_TO_KG_M2_S <- 1.0 / ( 2.45 * 1E6 )
  PA_2_KPA = 0.001
  HPA_2_KPA = 0.1

  df <- df %>%
    mutate(
      GPP = GPP * UMOL_TO_MOL * MOL_C_TO_GRAMS_C * SEC_2_HLFHOUR,
      ET = Qle * WM2_TO_KG_M2_S * SEC_2_HLFHOUR,
      VPD = VPD * HPA_2_KPA
    )


  # Resample to daily
  df <- df %>%
    mutate(date = floor_date(date, "day")) %>%
    group_by(date) %>%
    summarise(
      GPP = sum(GPP, na.rm=TRUE),
      ET = sum(ET, na.rm=TRUE),
      Precip = sum(Precip, na.rm=TRUE),
      Qle = mean(Qle, na.rm=TRUE),
      Tair = mean(Tair, na.rm=TRUE),
      SW_rad = mean(SW_rad, na.rm=TRUE),
      Rn = mean(Rn, na.rm=TRUE),
      Pa = mean(Pa, na.rm=TRUE),
      VPD = mean(VPD, na.rm=TRUE),
      .groups = "drop"
    )

  return(df)
}
