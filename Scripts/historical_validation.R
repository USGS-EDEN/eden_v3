#------------------------------------------------------------------------------
# This script finds the differences between EDEN v3 (i.e. EDEN in R) and 
# EDEN v2 (official EDEN writtent in ArcPy) for a set historical period

# Output: 1) Difference raster with real EDEN
#         2) Table of RMSE for each day
#         3) Map of difference raster

# Saira Haider 
# shaider@usgs.gov
#------------------------------------------------------------------------------

library(raster)

## USER INPUTS ##

eden_v3_folder <- "./Output/Stage/historical_rasters/"
eden_v2_folder <- "../EDEN/surfaces/rasters"

rmse_file <- "./Output/Comparison/RMSE_v3_minus_v2.csv"
output_diff_rasters <- "./Output/Comparison/historical_diff_rasters/"
output_diff_maps <- "./Output/Comparison/historical_diff_maps/"

#------------------------------------------------------------------------------
# Prepare data

eden_v3 <- list.files(path = eden_v3_folder, full.names = TRUE)
eden_v3 <- sort(eden_v3)

eden_v2 <- list.files(path = eden_v2_folder, full.names = TRUE, recursive = TRUE,
                      pattern = "*.tif$")


# Get dates
dates  <- unlist(regmatches(eden_v3, gregexpr("[[:digit:]]+", eden_v3)))
dates

#------------------------------------------------------------------------------
# FIND DIFFERENCE BETWEEN V3 AND V2

# vector to store RMSE
error_vec <- vector()

for(i in 1:length(eden_v3)){    #i <- 10
  
  # import EDEN v3 raster
  eden_v3_r <- raster(eden_v3[i])
  
  # get date
  day <- dates[i]
  
  # import EDEN v2 raster
  eden_v2_r <- grep(day, eden_v2, value = TRUE)
  eden_v2_r <- raster(eden_v2_r)
  
  # create difference raster & export
  eden_v2_r <- round(eden_v2_r)
  diff_raster <- eden_v3_r - eden_v2_r 

  diff_file <- paste0(output_diff_rasters, "diff_v3_minus_v2_cm_", day, ".tif")
  writeRaster(diff_raster, diff_file)
  
  # create map & export
  map_file <- paste0(output_diff_maps, "diff_v3_minus_v2_cm_", day, ".jpg")
  jpeg(map_file, width = 600, height = 800)
  plot(diff_raster)
  dev.off()
  
  # find RMSE
  diff <- na.omit(getValues(diff_raster))
  error_vec[i] <- sqrt(mean(diff^2))
}

#------------------------------------------------------------------------------
## Create RMSE dataframe & export
rmse_df <- data.frame(cbind(dates, error_vec), stringsAsFactors = FALSE)
colnames(rmse_df) <- c("dates", "rmse_cm")
rmse_df$rmse_cm <- round(error_vec, digits = 2)
head(rmse_df)

write.csv(rmse_df, rmse_file, row.names = FALSE)

mean(error_vec)
summary(error_vec)
