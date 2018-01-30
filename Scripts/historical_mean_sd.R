#------------------------------------------------------------------------------
# Script for creating a raster of the mean and standard deviation of
# the historical alt-EDEN dataset

# Saira Haider 
# shaider@usgs.gov               
#------------------------------------------------------------------------------

library(raster)
#library(rgdal)

#------------------------------------------------------------------------------

v3_files <- list.files("./Output/Comparison/historical_diff_rasters", 
                        pattern = ".tif$", full.names = TRUE)
v3_rstack <- stack(v3_files)



## Use multiple cores
cores <- 4
beginCluster(cores, type = "SOCK")

# Find RMSE
sqrt(mean(v3_rstack[[1:10]]^2)) # testing
rmse <- sqrt(mean(v3_rstack^2))
plot(rmse)
writeRaster(rmse, "./Output/Comparison/diff_2007to2017q3_rmse.tif")

# Find Standard deviation
calc(v3_rstack[[1:3]], fun = sd) # testing
stdev <- calc(v3_rstack, fun = sd)

## Stop using multiple cores
endCluster()

plot(stdev)
writeRaster(stdev, "./Output/Comparison/diff_2007to2017q3_sd.tif")
