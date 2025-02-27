####
##  Read a eddy covariance flux file
##
##  author: Martin De Kauwe
##  date: 27th February, 2025
##  email: mdekauwe@gmail.com
####

read_flux <- function(fname) {

  df <- read_csv(fname)

  # Convert TIMESTAMP_START to datetime and set as index
  df <- df %>%
    mutate(date = ymd_hm(TIMESTAMP_START)) %>%
    select(-TIMESTAMP_START, -TIMESTAMP_END) %>%
    relocate(date)

  df <- df %>%
    mutate(
      year = year(date),
      doy = yday(date),
      hod = hour(date)
    )

  # Just going to keep a few useful things for now...someone should expand the
  # list
  df <- df %>%
    rename(
      GPP = GPP_NT_VUT_REF,
      Qle = LE_F_MDS,
      Precip = P_ERA,
      Tair = TA_F_MDS,
      SW_rad = SW_IN_F_MDS,
      Rn = NETRAD,
      Pa = PA_F,
      VPD = VPD_F_MDS
    )

  vars_to_keep <- c("GPP", "Qle", "Precip", "Tair", "SW_rad", "Rn", "Pa", "VPD")
  df <- df %>% select(date, all_of(vars_to_keep), year, doy, hod)

  return(df)
}
