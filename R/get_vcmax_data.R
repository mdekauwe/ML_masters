# Vcmax data -> https://zenodo.org/records/14848098

library(terra)
library(ncdf4)

# Harvard forest 
latitude <- 42.5315
longitude <- -72.1900

#
## Vcmax from GOME-2 SIF
#
gome_file <- "/Users/xj21307/Downloads/GOME2_VcmaxTg_05deg.tif"

vcmax_gome_raster <- rast(gome_file)
coords <- data.frame(lon = longitude, lat = latitude)
vcmax_gome <- terra::extract(vcmax_gome_raster, coords)
vcmax_gome <- data.frame(lat = latitude, lon = longitude, 
                           Vcmax = vcmax_gome)
vcmax_gome <- vcmax_gome$Vcmax.GOME2_VcmaxTg_05deg

print(vcmax_gome)

