#------------------------------------------------------------------------------
# SCRIPT TO RUN THE EVER4CAST DAILY STAGE DATA FROM THE SIMULATIONS
# THROUGH THE R-VERSION OF EDEN
# OUTPUTS A NETCDF OF DEPTH PER SIMULATION

# Saira Haider 
# shaider@usgs.gov
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey
#-----------------------------------------------------------------------------

source("./Scripts/eden_v3.R")
source("../R_library/netCDF_IO_v3.1.R")
library(reshape2)
library(data.table)

#------------------------------------------------------------------------------
# USER INPUTS

sim_medians_folder <- "../Ever4Cast/e4c_v1.1.2_forecast_stage_2018/spa_central_tendency_20180101_20180630/20180104_spa_central_tendency_20180101_20180630_daily_median_data"
output_folder <- "../Ever4Cast//eden_forecast2018/netcdfs/"
output_csv_folder <- "../Ever4Cast/eden_forecast2018/csvs/"

#------------------------------------------------------------------------------
# Set up netcdf info 

xDim            <- 287            #number of columns
yDim            <- 405            #number of rows
cell_size       <- 400            #cell resolution
extent          <- c(463400, 577800, 2951800, 2790200)  #UTM coordinates

out_layer       <- "depth"  
#layerLength     <- length(out_layer)
long_layer_name <- "Water Depth (cm)"
out_units       <- "cm"
out_prec        <- "float"
background      <- NaN                         
source_name     <- "ever4cast_sims.r"
institution     <- "Wetland and Aquatic Research Center, USGS"
qaqc            <- "under review"
comments        <- "Product derived from RBF interpolation of water stage over the EDEN extent"

## Get Data ## ----------------------------------------------------------------

# DEM
dem <- read.csv("../WERP/Output/werp_dem_400m_cm.csv", 
                stringsAsFactors = FALSE)

# List of simulations
sims <- list.files(sim_medians_folder, full.names = TRUE)
sims_filenames <- list.files(sim_medians_folder, full.names = FALSE)
sim_names <- substr(sims_filenames, 1, 8)
sim_list <- lapply(sims, fread)

## Set up time series ## ------------------------------------------------------

# Find first day of the simulations
day1 <- min(sim_list[[1]]$Date)
# Start date for netcdf file 
start_date_str <- paste0(substr(day1, 1, 4), "-", substr(day1, 5, 6), "-", 
                          substr(day1, 7, 8)) 
# Set time dimensions for NetCDF file
tDim <- length(unique(sim_list[[1]]$Date)) # number of time steps
step_vec <- seq(1, tDim)
# Get each time step:
time_steps <- sort(unique(sim_list[[1]]$Date))

## Run loop for each simulation, interpolate, and output netcdf file ## -------
print(sims)

startTime <- proc.time()

for(i in 1:length(sims)){              # i <- 1

  df <- sim_list[[i]]
  daily_list <- split(df, df$Date) # list of daily dataframes
  
  # Run simulation for each day
  print(paste("Simulation ", i, " of ", length(sims)))
  sim_stage_list <- lapply(daily_list, interpolate_gauges_csv)

  # Add column for date
  sim_stage <- rbindlist(sim_stage_list)
  sim_stage$date <- rep(names(daily_list), sapply(sim_stage_list, nrow))

  # Find depth
  sim_depth <- merge(sim_stage, dem, by.x = c("X_COORD", "Y_COORD"), 
                     by.y = c("x_coord", "y_coord"))
  sim_depth$depth <- sim_depth$stage - sim_depth$DEM_cm
  sim_depth <- within(sim_depth, rm(stage, DEM_cm))
  
  # Find min and max for netcdf attributes
  depth_min <- min(sim_depth$depth)
  depth_max <- max(sim_depth$depth)
  
  # Export depth as csv
  output_csv_file <- paste0(output_csv_folder, sim_names[i], "_depth", ".csv")
  fwrite(sim_depth, output_csv_file)
  
  # Write netcdf & export
  output_file <- paste0(output_folder, sim_names[i], ".nc")
  
  nc_out <- createNetCDFfile(out.name = output_file,
                             layer.name = out_layer,
                             units = out_units,
                             prec = out_prec,
                             long.name = long_layer_name,
                             daily = TRUE,
                             extent = extent,
                             cell.size = cell_size,
                             fill.value = background,
                             source.name = source_name,
                             institution = institution,
                             qaqc = qaqc,
                             comments = comments,
                             start.dateStr = start_date_str, 
                             t.size = tDim,
                             layer.min = depth_min,
                             layer.max = depth_max
  )
  
  # Write data for each time step as a matrix to export as netcdf
  for(t in 1:tDim){                               # t <- 1
    print(paste("Simulation ", i, " T-Step ", t, " of ", tDim))
    
    # subset depth dataframe for each timestep
    dt <- as.data.frame(sim_depth[sim_depth$date == time_steps[t], ])
    # create matrix for netcdf
    out_mat <- acast(dt, X_COORD ~ Y_COORD, value.var = out_layer)
    out_mat <- t(apply(out_mat, 1, rev)) # need to rotate matrix
    vec2nc(nc_out, out_mat, out_layer, step_vec[t])
  }
  
  closeNetCDF(nc_out) 
}

stopTime <- proc.time()
duration <- stopTime - startTime
duration[3]/60  # Number of minutes
duration[3]/60/60  # Number of hours
