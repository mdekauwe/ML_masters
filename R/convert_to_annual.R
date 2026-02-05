library(dplyr)
library(lubridate)

convert_to_annual <- function(df_daily) {

  # Add year column
  df_daily <- df_daily %>%
    mutate(year = year(date))

  df_annual <- df_daily %>%
    group_by(year) %>%
    summarise(
      GPP = sum(GPP, na.rm = TRUE),
      ET = sum(ET, na.rm = TRUE),
      Precip = sum(Precip, na.rm = TRUE),
      Qle = mean(Qle, na.rm = TRUE),
      Tair = mean(Tair, na.rm = TRUE),
      SW_rad = mean(SW_rad, na.rm = TRUE),
      Rn = mean(Rn, na.rm = TRUE),
      Pa = mean(Pa, na.rm = TRUE),
      VPD = mean(VPD, na.rm = TRUE),
      .groups = "drop"
    )

  return(df_annual)
}
