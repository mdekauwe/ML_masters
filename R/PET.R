####
##  Priestley-Taylor potential ET
##
##  author: Martin De Kauwe
##  date: 27th February, 2025
##  email: mdekauwe@gmail.com
####

priestley_taylor_pet <- function(Rn, Tair, P) {

  alpha = 1.26      # PT constant
  cp <- 1.013e-3    # Specific heat of air (MJ/kg°C)
  epsilon <- 0.622  # Ratio of molecular weights of water vapor to dry air
  lambda <- 2.45    # Latent heat of vaporization (MJ/kg)
  G = 0             # Going to assume no ground heat flux on daily timescales

  # Psychrometric constant (kPa/°C)
  gamma <- (cp * P) / (epsilon * lambda)

  # Saturation vapor pressure (kPa)
  es <- 0.6108 * exp((17.27 * Tair) / (Tair + 237.3))

  # Slope of saturation vapor pressure curve (kPa/°C)
  delta <- (4098 * es) / ((Tair+ 237.3)^2)

  # Priestley-Taylor PET calculation (mm/day)
  pet <- alpha * (delta / (delta + gamma)) * ((Rn - G) / (lambda * 1e6)) * 86400

  return(pet)
}
