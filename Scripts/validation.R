#------------------------------------------------------------------------------
# Script for validating altEDEN
# Replicating Z. Liu et al. 2009

# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey

# Last update: November 15, 2017                       
#------------------------------------------------------------------------------

source("altEDEN_fxns.R")

#------------------------------------------------------------------------------

## Import benchmark data for validation ## ----

bench <- read.csv("../validation/beerens20171025.csv", stringsAsFactors = FALSE)
bench_locs <- read.csv("../validation/beerens20171025b.csv", 
                       stringsAsFactors = FALSE)

bench_names <- function(two_strings){
  paste0(two_strings[1], two_strings[2])
}

bench$benchmark <- unlist(lapply(strsplit(bench$benchmark, "BM"), bench_names))
bench <- merge(bench, bench_locs, by = c("benchmark"))
bench$date <- as.Date(bench$date, format = "%m/%d/%y")
dates <- format(bench$date, format = "%Y%m%d")

# convert ft to cm
bench$stage_cm <- bench$stage_ft * 30.48


## Get gage data files ## ----
gagesq2 <- list.files("../gage_data/2007_q2_DM_v2r1", pattern = "_median.txt",
                      full.names = TRUE)
gagesq3 <- list.files("../gage_data/2007_q3_DM_v2r1", pattern = "_median.txt",
                      full.names = TRUE)
gage_files <- c(gagesq2, gagesq3)

# get the correct gages files
gages <- sapply(dates, grep, x = gage_files, value = TRUE)


## Get the real EDEN files ## ----
edenq2 <- list.files("../EDEN_surfaces/2007_q2_tiff_v2r1", pattern = ".tif$",
                      full.names = TRUE)
edenq3 <- list.files("../EDEN_surfaces/2007_q3_tiff_v2r1", pattern = ".tif$",
                      full.names = TRUE)
eden_files <- c(edenq2, edenq3)

# get the correct gages files
eden <- sapply(dates, grep, x = eden_files, value = TRUE)

alt_error <- vector()
eden_error <- vector()
## Get the differences between benchmark point & altEDEN & realEDEN ## ----
for(i in 1:length(gages)){ # i <- 1
  datum <- bench[i, ]
  coordinates(datum) <- ~utm_easting + utm_northing
  proj4string(datum) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  # Get alt EDEN error
  alt <- read.table(gages[i], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  alt_raster <- interpolate_gauges(alt)
  alt_value <- extract(alt_raster, datum)
  alt_error[i] <- alt_value - datum$stage_cm
  
  # Get real EDEN error
  eden_raster <- raster(eden[i])
  eden_value <- extract(eden_raster, datum)
  eden_error[i] <- eden_value - datum$stage_cm
  
}
  
## Look at error in alt & real EDEN ## ----
alt_error
eden_error
  
summary(alt_error)
summary(eden_error)

# RMSE
(alt_rmse <- sqrt(mean(alt_error^2)))
(eden_rmse <- sqrt(mean(eden_error^2)))

## Find the error for WCA3A vs. WCA3B ## ----
bench$alt_error <- alt_error
bench$eden_error <- eden_error

wca3a <- bench[bench$region == "3A", ]
wca3b <- bench[bench$region == "3B", ]

(wca3a_alt_rmse <- sqrt(mean(wca3a$alt_error^2)))
(wca3a_eden_rmse <- sqrt(mean(wca3a$eden_error^2)))

(wca3b_alt_rmse <- sqrt(mean(wca3b$alt_error^2)))
(wca3b_eden_rmse <- sqrt(mean(wca3b$eden_error^2)))
