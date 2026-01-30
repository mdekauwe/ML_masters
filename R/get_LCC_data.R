# Vcmax data -> https://zenodo.org/records/14848098

library(terra)
library(ncdf4)

# Harvard forest 
latitude <- 42.5315
longitude <- -72.1900



latlon_to_line_sample <- function(lat, lon, nlines = 1800, nsamples = 3600) {
 
  lat_res <- 180. / nlines  
  line <- round((90. - lat) / lat_res)
  
  lon_res <- 360. / nsamples
  sample <- round((lon + 180.) / lon_res)
  
  line <- pmin(pmax(line, 1), nlines)
  sample <- pmin(pmax(sample, 1), nsamples)
  
  return(list(line = line, sample = sample))
}

#
## global leaf chlorophyll content
#
nc <- nc_open("/Users/xj21307/Downloads/Chl_2003_2011_weeklyAll_01Deg_sf100.nc")

# Pixel coordinates
line_idx   <- 500   # 1..1800
sample_idx <- 1000  # 1..3600

coords <- latlon_to_line_sample(latitude, longitude)

pixel_time_series <- ncvar_get(nc, "chl_all",
                               start = c(coords$line, coords$sample, 1, 1),
                               count = c(1, 1, -1, -1))
fillvalue <- ncatt_get(nc, "chl_all", "_FillValue")$value
pixel_time_series[pixel_time_series == fillvalue] <- NA

dim(pixel_time_series)  # 52 x 9 (weeks x years)
pixel_time_series

LCC <- pixel_time_series / 100 # ug/cm²

# LCC: 52 weeks × 9 years
weekly_climatology <- rowMeans(LCC, na.rm = TRUE)

plot(1:52, weekly_climatology, type='l', xlab='Week', ylab='LCC (μg/cm²)')

# Create a sequence days
daily_weeks <- seq(1, 52, by = 1/7) 

# Interpolate linearly
daily_climatology <- approx(x = weeks, y = weekly_climatology, xout = daily_weeks)$y

# Plot
plot(daily_weeks, daily_climatology, type='l', xlab='Week', ylab='LCC (μg/cm²)')
points(weeks, weekly_climatology, col='red')  # show original weekly points

nc_close(nc)
