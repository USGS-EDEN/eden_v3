### Set to containing directory of script!
setwd("~/Desktop/eden_V3")
dir <- getwd()

source("./Scripts/eden_v3b.R")

## For testing eden_v3::eden_nc()

# Desired date range
# realtime
#dt <- seq(Sys.Date() - 4, Sys.Date() - 1, "days")
# arbitrary
dt <- seq(as.Date("2017-07-01"), as.Date("2017-09-30"), "days")

netcdf_output <- "./Output/Stage/eden_v3_2017q3.nc"


eden_nc(dt, "database", netcdf_output)

