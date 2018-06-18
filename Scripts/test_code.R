

source("./Scripts/eden_v3.R")

## For testing eden_v3::eden_nc()

# these are data downloaded from the EDEN website - the "Daily Median Output File" for 1 quarter
eden_gage_files <- list.files("../EDEN/Data/gage_data/2017_q3_DM_v2prov", 
                        pattern = "flag.txt", full.names = TRUE)

netcdf_output <- "./Output/Stage/eden_v3_2017q3.nc"


eden_nc(eden_gage_files, netcdf_output)

