#------------------------------------------------------------------------------
# This script finds the differences between EDEN v3 (i.e. EDEN in R) and 
# EDEN v2 (official EDEN written in ArcPy) for a set historical period

# Output: 1) Difference raster with real EDEN
#         2) Table of RMSE for each day
#         3) Map of difference raster

# Saira Haider 
# shaider@usgs.gov
#------------------------------------------------------------------------------

library(raster)

## USER INPUTS ##

#eden_v3_folder <- "./Output/Stage/historical_rasters/"
eden_v3_folder <- "./Output/Stage/EDENv3_nowlround_onepN202/"
eden_v2_folder <- "../EDEN/Data/surfaces/rasters"

rmse_file <- "./Output/Comparison/EDENv3_nowlround_onepN202/RMSE_v3_minus_v2.csv"
output_diff_rasters <- "./Output/Comparison/EDENv3_nowlround_onepN202/historical_diff_rasters/"
output_diff_maps <- "./Output/Comparison/EDENv3_nowlround_onepN202/historical_diff_maps/"

#------------------------------------------------------------------------------
# Prepare data

eden_v3 <- list.files(path = eden_v3_folder, full.names = TRUE)
#eden_v3 <- sort(eden_v3)
# Want 2014 Quarter 2 through 2018 Quarter 1
eden_v3 <- eden_v3[21:36]
eden_v3_stack <- stack(eden_v3)
crs(eden_v3_stack) <- crs("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

eden_v2 <- list.files(path = eden_v2_folder, full.names = TRUE, recursive = TRUE,
                      pattern = "*.tif$")


# Get dates from EDEN v2
#dates  <- unlist(regmatches(eden_v3, gregexpr("[[:digit:]]+", eden_v3)))
dates <- c("20140401", "20180331") 
v2_date1 <- grep(dates[1], eden_v2)
v2_date2 <- grep(dates[2], eden_v2)
eden_v2[v2_date1]
eden_v2[v2_date2]
eden_v2 <- eden_v2[v2_date1:v2_date2]

#------------------------------------------------------------------------------
# FIND DIFFERENCE BETWEEN V3 AND V2

# vector to store RMSE
error_vec <- vector()

for(i in 1:nlayers(eden_v3_stack)){    #i <- 1372
  
  # import EDEN v3 raster
  #eden_v3_r <- raster(eden_v3[i])
  
  # get date
  #day <- dates[i]
  print(eden_v2[i])
  
  # import EDEN v2 raster
  #eden_v2_r <- grep(day, eden_v2, value = TRUE)
  eden_v2_r <- raster(eden_v2[i])
  
  # create difference raster & export
  diff_raster <- eden_v3_stack[[i]] - eden_v2_r 

  day <- substr(names(eden_v2_r), 3, nchar(names(eden_v2_r)))
  diff_file <- paste0(output_diff_rasters, "diff_v3_minus_v2_cm_", day, ".tif")
  writeRaster(diff_raster, diff_file, overwrite = TRUE)
  
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
all_dates <- seq.Date(as.Date(dates, format = "%Y%m%d")[1], as.Date(dates, format = "%Y%m%d")[2], by = 1)
rmse_df <- data.frame(dates = all_dates, 
                      rmse_cm = error_vec)
rmse_df$rmse_cm <- round(error_vec, digits = 2)
head(rmse_df)

write.csv(rmse_df, rmse_file, row.names = FALSE)
# rmse_df <- read.csv(rmse_file, stringsAsFactors = FALSE)

mean(error_vec)
summary(error_vec)

mean(rmse_df$rmse_cm)
sd(rmse_df$rmse_cm)
