#------------------------------------------------------------------------------
# Script for comparing v3 and v2 eden
# For the 2nd set of benchmarks given to me by Bryan McCloskey

# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey             
#------------------------------------------------------------------------------

library(sp)
library(raster)

#------------------------------------------------------------------------------
## Import benchmark data for validation ## ----

bench_locs <- read.csv("./Data/Benchmarks/haider20180105_utm.csv", 
                       stringsAsFactors = FALSE)
bench <- read.csv("./Data/Benchmarks/beerens20171130_omitnull.csv", 
                  stringsAsFactors = FALSE)
                       
## Add location data to benchmark data
sort(unique(bench_locs$benchmark))
sort(unique(bench$benchmark))

bench <- merge(bench, bench_locs, by = c("benchmark"))
bench$date <- as.Date(bench$date, format = "%m/%d/%Y")
dates <- format(bench$date, format = "%Y-%m-%d")

# convert ft to cm
bench$stage_cm <- bench$level * 30.48

#------------------------------------------------------------------------------
## GET V3 and V2 EDEN DATA FILES

## Import v3 surfaces as raster stack ## ----
v3_files <- list.files("./Data/BryanSaira_testing/edenV3_bryan", pattern = ".nc",
                       full.names = TRUE)
v3_stack <- stack(v3_files)
proj4string(v3_stack) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## Import the v2 surfaces as raster stack ## ----
v2_files <- list.files("./Data/BryanSaira_testing/edenV2_netcdfs", pattern = ".nc",
                       full.names = TRUE)
v2_stack <- stack(v2_files)
proj4string(v2_stack) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


# Get the indices of the raster stacks for the matching benchmark dates
eden_dates <- as.character(seq.Date(as.Date("2007/04/01"), as.Date("2011/09/30"), by = "day"))
eden_index <- sapply(dates, grep, x = eden_dates)

#------------------------------------------------------------------------------
## FIND THE BENCHMARK ERROR WITH  V3 and V2 EDEN

v3_error <- vector()
v2_error <- vector()

for(i in 1:length(dates)){ # i <- 10
  datum <- bench[i, ]
  coordinates(datum) <- ~POINT_X + POINT_Y
  proj4string(datum) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  # Get v3 error
  v3_value <- extract(v3_stack[[eden_index[i]]], datum)
  v3_error[i] <- v3_value - datum$stage_cm
  
  # Get v2 error
  v2_value <- extract(v2_stack[[eden_index[i]]], datum)
  v2_error[i] <- v2_value - datum$stage_cm
  
}

## Look at error in v3 & v2 EDEN ## ----
v3_error
v2_error

summary(v3_error)
summary(v2_error)

# RMSE
## 3 of the points are outside the EDEN domain, hence the NAs
# there are 7 measurements at those NAs
(v3_rmse <- sqrt(mean(v3_error^2, na.rm = TRUE)))
(v2_rmse <- sqrt(mean(v2_error^2, na.rm = TRUE)))


# STANDARD DEVIATION
(v3_sd <- sd(v3_error, na.rm = TRUE))
(v2_sd <- sd(v2_error, na.rm = TRUE))

#------------------------------------------------------------------------------
# Export
head(bench)
bench$v3_minus_bm <- v3_error
bench$v2_minus_bm <- v2_error
write.csv(bench, "./Output/benchmarks20171130_error.csv", row.names = FALSE)