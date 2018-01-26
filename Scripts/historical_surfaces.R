#------------------------------------------------------------------------------
# This script runs the R version of the EDEN interpolation over the time period:
# 2007 Quarter 2 - 2017 Quarter 3

# Output: 1) R interpolation as CSV
#         2) R interpolation as raster
#         3) Difference raster with real EDEN
#         4) Table of RMSE for each day
#         5) Map of difference raster

# Saira Haider 
# shaider@usgs.gov
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey
#------------------------------------------------------------------------------

source("eden_v3.R")

#------------------------------------------------------------------------------
# Read in gauge data

medians1 <- list.files(path = "../gage_data", pattern = "median.txt", 
                      full.names = TRUE, recursive = TRUE)
medians2 <- list.files(path = "../gage_data", pattern = "median_flag.txt", 
                       full.names = TRUE, recursive = TRUE)
medians <- c(medians1, medians2)
medians <- sort(medians)

#------------------------------------------------------------------------------

# Get dates
median_dates  <- sub(".*/([0-9]+).*", "\\1", medians)
median_dates
median_dates[1000:1990]
median_dates[1991:2990]
median_dates[2991:3836]


# Read in EDEN rasters
eden <- list.files(path = "../EDEN_surfaces", pattern = "[0123456789].tif$", 
                   full.names = TRUE, recursive = TRUE)
eden <- sort(eden)
eden
eden[1000:1990]
eden[1991:2990]
eden[2991:3836]

#### RUN INTERPOLATION FUNCTIONS------------------------------------------------

output_csvs <- "./output/historical_csvs/"
output_rasters <- "./output/historical_rasters/"
output_diff <- "./output/historical_diff_rasters/"
output_maps <- "./output/historical_diff_maps/"

# vector to store RMSE
error_vec <- vector()

sink("console_print.txt")

for(i in 1:length(eden)){    #i <- 10
  
  df <- read.table(medians[i], sep = "\t", header = TRUE)
  
  # get date
  day <- median_dates[i]
  
  # interpolate & export
  df <- interpolate_gauges_csv(df)
  csv_file <- paste0(output_csvs, day, "_stage_cm.csv")
  write.csv(df, csv_file, row.names = FALSE)
 
  # rasterize & export
  coordinates(df) <- ~X_COORD + Y_COORD
  proj4string(df) <- nad_utm
  gridded(df) <- TRUE
  df_raster <- raster(df)
  raster_file <- paste0(output_rasters, day, "_stage_cm.tif")
  writeRaster(df_raster, raster_file)
   
  # import EDEN raster
  eden_r <- raster(eden[i])
   
  # create difference raster & export
  diff_r <- df_raster - eden_r
  diff_file <- paste0(output_diff, day, "_alt_minus_eden_diff_cm.tif")
  writeRaster(diff_r, diff_file)
  
  # create map & export
  map_file <- paste0(output_maps, day, "_alt_minus_eden_cm.jpg")
  jpeg(map_file, width = 600, height = 800)
  plot(diff_r)
  dev.off()
  
  # calculate RMSE
  error <- find_rmse(df_raster, eden_r) 
  error_vec[i] <- error
    
}
sink()


## Create RMSE dataframe & export
rmse_df <- data.frame(cbind(median_dates, error_vec), stringsAsFactors = FALSE)
colnames(rmse_df) <- c("dates", "rmse_cm")
rmse_df

write.csv(rmse_df, "./output/RMSE_alt_minus_real.csv")

mean(error_vec, na.rm = TRUE)
summary(error_vec)