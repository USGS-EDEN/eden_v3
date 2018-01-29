#------------------------------------------------------------------------------
# Find depth from EDEN v3 using the new WERP DEM 

# Saira Haider 
# shaider@usgs.gov               
#------------------------------------------------------------------------------

library(rgdal)
library(raster)

## USER INPUTS ##
input_tif <- "./Output/Stage/historical_rasters/stage_cm_20070101.tif"
output_tif <- "./Output/Depths/historical_werp_depth/depth_cm_20070101.tif"

#------------------------------------------------------------------------------

werp_dem <- raster("../GIS_Library/DEM/werpeden_400m_cm.tif")
eden_v3 <- raster(input_tif)

werp_dem
eden_v3

# 'crop' function not working, but it doesn't seem to be necessary for raster substraction
# werp_dem <- crop(werp_dem, eden_v3)
plot(werp_dem)
plot(eden_v3)

depth_raster <- eden_v3 - werp_dem
depth_raster
plot(depth_raster)

writeRaster(depth_raster, output_tif)
