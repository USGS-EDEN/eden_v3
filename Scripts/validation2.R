#------------------------------------------------------------------------------
# Script for validating altEDEN
# For the 2nd set of benchmarks given to me by Bryan McCloskey

# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey

# Last update: January 5th, 2018                  
#------------------------------------------------------------------------------

library(sp)
library(raster)

#------------------------------------------------------------------------------
## Import benchmark data for validation ## ----

bench_locs <- read.csv("../validation2/haider20180105_utm.csv", 
                       stringsAsFactors = FALSE)
bench <- read.csv("../validation2/beerens20171130_omitnull.csv", 
                  stringsAsFactors = FALSE)
                       
## Add location data to benchmark data
sort(unique(bench_locs$benchmark))
sort(unique(bench$benchmark))

bench <- merge(bench, bench_locs, by = c("benchmark"))
bench$date <- as.Date(bench$date, format = "%m/%d/%Y")
dates <- format(bench$date, format = "%Y%m%d")

# convert ft to cm
bench$stage_cm <- bench$level * 30.48

#------------------------------------------------------------------------------
## GET ALT-EDEN & REAL EDEN DATA FILES


# Alt-EDEN
alteden_files_all <- list.files("./output/historical_rasters", recursive = TRUE, 
                                full.names = TRUE)
alteden_files <- sapply(dates, grep, x = alteden_files_all, value = TRUE)


# EDEN
eden_files_all <- list.files("../EDEN_surfaces", pattern = ".tif$", 
                         recursive = TRUE, full.names = TRUE)
eden_files <- sapply(dates, grep, x = eden_files_all, value = TRUE)


#------------------------------------------------------------------------------
## FIND THE BENCHMARK ERROR WITH ALT-EDEN AND REAL-EDEN

alt_error <- vector()
eden_error <- vector()

for(i in 1:length(dates)){ # i <- 16
  datum <- bench[i, ]
  coordinates(datum) <- ~POINT_X + POINT_Y
  proj4string(datum) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  # Get alt EDEN error
  alt_raster <- raster(alteden_files[i])
  alt_value <- extract(alt_raster, datum)
  alt_error[i] <- alt_value - datum$stage_cm
    
  # Get real EDEN error
  eden_raster <- raster(eden_files[i])
  eden_value <- extract(eden_raster, datum)
  eden_error[i] <- eden_value - datum$stage_cm
  
}
  
## Look at error in alt & real EDEN ## ----
alt_error
eden_error

bench$alt_error <- alt_error
bench$eden_error <- eden_error

summary(alt_error)
summary(eden_error)

## 3 of the points are outside the EDEN domain, hence the NAs

# RMSE
(alt_rmse <- sqrt(mean(alt_error^2, na.rm = TRUE)))
(eden_rmse <- sqrt(mean(eden_error^2, na.rm = TRUE)))

# STANDARD DEVIATION
(alt_sd <- sd(alt_error, na.rm = TRUE))
(eden_sd <- sd(eden_error, na.rm = TRUE))



#------------------------------------------------------------------------------
# EXPORT RESULTS

write.csv(bench, "../validation2/output/validation2.csv", row.names = FALSE)