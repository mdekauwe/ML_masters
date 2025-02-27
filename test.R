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


plot(df$cwd)