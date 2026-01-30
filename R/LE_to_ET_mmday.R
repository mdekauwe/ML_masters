LE <- 150 # example, this would be your flux dats

# latent heat of vaporisation
lambda <- 2.45e6  # J/kg

# seconds per day
secs_day <- 86400

# convert to mm/day
ET_mm_day <- LE / lambda * secs_day * 1000
ET_mm_day
