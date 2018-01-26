#------------------------------------------------------------------------------
# This script runs the R version of the EDEN interpolation over the time period:
# 2007 Quarter 2 - 2017 Quarter 2

# Output: 1) R interpolation as CSV
#         2) R interpolation as raster
#         3) Difference raster with real EDEN
#         4) Table of RMSE for each day
#         5) Map of difference raster

# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey

# Last update: January 12th, 2017                     
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

# --------------------------- JAN 5: ---------------------------------------------
# READING IN GAUGE DATA THAT WAS CORRECTED BY BRYAN 
# ORIGINALLY FILES WERE IMPROPERLY FORMATTED ONLINE
# QUARTER 4 OF 2010 AND QUARTERS 1-3 OF 2011  HAD TO BE REDONE

medians <- list.files("../gage_data/haider20180105b", pattern = "median_flag.txt", 
                      full.names = TRUE, recursive = TRUE)

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

# --------------------------- JAN 5TH ------------------------------------------
## SUBSELECT FOR ONLY THOSE RASTERS THAT MATCH THE POORLY FORMATTED MEDIANS
eden <- eden[1372:1736]
#------------------------------------------------------------------------------


#### RUN INTERPOLATION FUNCTIONS------------------------------------------------

output_csvs <- "./output/historical_csvs/"
output_rasters <- "./output/historical_rasters/"
output_diff <- "./output/historical_diff_rasters/"
output_maps <- "./output/historical_diff_maps/"

# vector to store RMSE
error_vec <- vector()


# --------------------------- JAN 5TH ------------------------------------------
# THIS PROBLEM WAS FIXED
    # From i = 1372 to i = 1736, there's a formatting problem in the daily medians file
    # So have to fill that section with NA's
    medians[1372]
    medians[1736]
    error_vec[1372:1736] <- NA
    # Then keep running the function
#------------------------------------------------------------------------------
    
# --------------------------- JAN 12TH ------------------------------------------
## FIXED THIS PROBLEM ##
    
    
# i = 1932 IS A BAD TIF FILE (20120413)
error_vec[1932] <- NA

# i = 2160 IS A BAD TIF FILE (20121127)
error_vec[2160] <- NA

# i = 3153 IS A BAD TIF FILE (20150817)
error_vec[3153] <- NA

# i = 3305 IS A BAD TIF FILE (20160116)
error_vec[3305] <- NA

# i = 3414 IS A BAD TIF FILE (20160504)
error_vec[3414] <- NA

# i = 3690 IS A BAD TIF FILE (20170204)
error_vec[3690] <- NA
#------------------------------------------------------------------------------


sink("console_print_i1372_to_i1736.txt")
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
rmse_df <- data.frame(cbind(median_dates[1:i], error_vec[1:i]), stringsAsFactors = FALSE)
colnames(rmse_df) <- c("dates", "rmse_cm")
rmse_df

#write.csv(rmse_df, "./output/RMSE_alt_minus_real_BACKUP_20171219.csv")
write.csv(rmse_df, "./output/RMSE_alt_minus_real.csv")

# --------------------------- JAN 5  ------------------------------------------
## READ IN OLD RMSE CSV, ADD NEW VALUES TO IT

### SOMETHING IS WRONG WITH THE DAILY MEDIANS DATA FROM 09-30-2011

rmse_csv <- read.csv("./output/RMSE_alt_minus_real.csv")
rmse_csv[1372:1736, ]
rmse_csv[1736, ]

rmse_csv[1372:1735, ] <- error_vec
mean(rmse_csv$rmse_cm , na.rm = TRUE)


write.csv(rmse_csv, "./output/RMSE_alt_minus_real.csv", row.names = FALSE)

#------------------------------------------------------------------------------

mean(error_vec, na.rm = TRUE)
summary(error_vec)

# NOTES FOR EDEN TEAM:
# PROBLEM WITH EDEN RASTER FROM 10-13-2016?