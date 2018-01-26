#------------------------------------------------------------------------------
# Script for creating a raster of the mean and standard deviation of
# the historical alt-EDEN dataset

# Saira Haider 
# shaider@usgs.gov               
#------------------------------------------------------------------------------

library(raster)
library(rgdal)

#------------------------------------------------------------------------------

v3_files <- list.files("./Output/Comparison/historical_diff_rasters", 
                        pattern = ".tif$", full.names = TRUE)
v3_rstack <- stack(alt_files)

# Find RMSE
# testing
sqrt(mean(v3_rstack [1:10]^2)) 
rmse <- sqrt(mean(v3_rstack^2))
plot(rmse)
writeRaster(rmse, "./Output/Comparison/diff_2007to2017q3_rmse.tif")

# Find Standard deviation
calc(alt_brk[[1:3]], fun = sd) # testing

cores <- 4
beginCluster(cores, type = "SOCK")
stdev <- calc(alt_brk, fun = sd)
endCluster()

plot(stdev)
writeRaster(stdev, "./Output/Comparison/diff_2007to2017q3_sd.tif")