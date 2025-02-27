source("R/read_flux_file.R")

setwd("data/")
fname = "FLX_FR-Pue_FLUXNET2015_FULLSET_HH_2000-2014_2-4.csv"

df = read_flux(fname)
print(df)
