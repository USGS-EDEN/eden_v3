#------------------------------------------------------------------------------
# This script runs the R version of the EDEN interpolation over the time period:
# 2007 Quarter 1 - 2017 Quarter 3

# Output: 1) R interpolation as CSV
#         2) R interpolation as raster

# Saira Haider 
# shaider@usgs.gov
#------------------------------------------------------------------------------

source("./Scripts/eden_v3b.R")


## USER INPUTS ##

gage_data <- "../EDEN/Data/gage_data"

#output_csvs <- "./Output/Stage/historical_csvs/"
output_rasters <- "./Output/Stage/historical_rasters/"

date1 <- "20140401"
date2 <- "20180330"

#------------------------------------------------------------------------------
# Read in gauge data

medians1 <- list.files(path = gage_data, pattern = "median.txt", 
                      full.names = TRUE, recursive = TRUE)
medians2 <- list.files(path = gage_data, pattern = "median_flag.txt", 
                       full.names = TRUE, recursive = TRUE)
medians <- c(medians1, medians2)
medians <- sort(medians)
#medians[2000:2990]

# Get dates
median_dates  <- substr(basename(medians), 1, 8)
median_dates
median_dates[1000:1990]
median_dates[1991:2990]
median_dates[2991:3836]

# Subset for specified dates
dates_num <- as.numeric(median_dates)
median_dates <- median_dates[dates_num >= date1 & dates_num <= date2]
medians <- medians[dates_num >= date1 & dates_num <= date2]
medians

#------------------------------------------------------------------------------
#### RUN INTERPOLATION FUNCTIONS 

Sys.time()
sink("./Output/Stage/historical_surfaces_consolePrint.txt")
for(i in 1:length(medians)){    #i <- 1
  
  df <- read.table(medians[i], sep = "\t", header = TRUE)
  print(medians[i])
  
  # get date
  day <- median_dates[i]
  
  # interpolate & export
  df_raster <- interpolate_gages(df, format = "raster")
  #csv_file <- paste0(output_csvs, "stage_cm_", day, ".csv")
  #write.csv(df, csv_file, row.names = FALSE)
 
  # rasterize & export
  # coordinates(df) <- ~X_COORD + Y_COORD
  # proj4string(df) <- nad_utm
  # gridded(df) <- TRUE
  # df_raster <- raster(df)
  raster_file <- paste0(output_rasters, "stage_cm_", day, ".tif")
  writeRaster(df_raster, raster_file)
  
}
sink()
Sys.time()
