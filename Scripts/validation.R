#------------------------------------------------------------------------------
# Script for comparing v3 and v2 eden against benchmark data used in Liu et al. 2009
# Replicating Z. Liu et al. 2009

# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey
#------------------------------------------------------------------------------

## Import benchmark data for validation ## ----
bench <- read.csv("./Data/Benchmarks/beerens20171025.csv", stringsAsFactors = FALSE)
bench_locs <- read.csv("./Data/Benchmarks/beerens20171025b.csv", 
                       stringsAsFactors = FALSE)

bench_names <- function(two_strings){
  paste0(two_strings[1], two_strings[2])
}

bench$benchmark <- unlist(lapply(strsplit(bench$benchmark, "BM"), bench_names))
bench <- merge(bench, bench_locs, by = c("benchmark"))
bench$date <- as.Date(bench$date, format = "%m/%d/%y")
dates <- format(bench$date, format = "%Y-%m-%d")

# convert ft to cm
bench$stage_cm <- bench$stage_ft * 30.48


## Import v3 surfaces as raster stack ## ----
v3_files <- list.files("./Data/BryanSaira_testing/edenV3_bryan", pattern = "2007",
                      full.names = TRUE)
v3_stack <- stack(v3_files)
proj4string(v3_stack) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## Import the v2 surfaces as raster stack ## ----
v2_files <- list.files("./Data/BryanSaira_testing/edenV2_netcdfs", pattern = "2007",
                       full.names = TRUE)
v2_stack <- stack(v2_files)
proj4string(v2_stack) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


# Get the indices of the raster stacks for the matching benchmark dates
eden_dates <- as.character(seq.Date(as.Date("2007/04/01"), as.Date("2007/12/31"), by = "day"))
eden_index <- sapply(dates, grep, x = eden_dates)


v3_error <- vector()
v2_error <- vector()

## Get the differences between benchmark point & v3 & v2 ## ----
for(i in 1:length(dates)){ # i <- 1
  datum <- bench[i, ]
  coordinates(datum) <- ~utm_easting + utm_northing
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
(v3_rmse <- sqrt(mean(v3_error^2)))
(v2_rmse <- sqrt(mean(v2_error^2)))

## Find the error for WCA3A vs. WCA3B ## ----
bench$v3_error <- v3_error
bench$v2_error <- v2_error

wca3a <- bench[bench$region == "3A", ]
wca3b <- bench[bench$region == "3B", ]

(wca3a_v3_rmse <- sqrt(mean(wca3a$v3_error^2)))
(wca3a_v2_rmse <- sqrt(mean(wca3a$v2_error^2)))

(wca3b_v3_rmse <- sqrt(mean(wca3b$v3_error^2)))
(wca3b_v2_rmse <- sqrt(mean(wca3b$v2_error^2)))


# Export
head(bench)
bench$v3_minus_bm <- v3_error
bench$v2_minus_bm <- v2_error
write.csv(bench, "./Output/benchmark20171025_error.csv", row.names = FALSE)
