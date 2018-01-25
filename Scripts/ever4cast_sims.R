#------------------------------------------------------------------------------
# SCRIPT TO RUN THE EVER4CAST DAILY STAGE DATA FROM THE SIMULATIONS
# THROUGH THE R-VERSION OF EDEN
# OUTPUTS A NETCDF OF DEPTH PER SIMULATION



# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey

# Last update: January 2nd, 2017                      
#------------------------------------------------------------------------------
# LIBRARIES

source("ever4cast_eden.R")
source("netCDF_IO_v3.1.R")
library(reshape2)
library(data.table)

#------------------------------------------------------------------------------
# TO SPEED UP THE INTERPOLATION, SOME PARTS OF THE INTERPOLATION MUST BE DONE
# OUTSIDE THE FUNCTION:

id <- read.csv("./data/gauge_name_EArea_id.csv")
nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#### read in polygon shapefiles of all the subzones  ####
wca1 <- readOGR(dsn = path.expand("./data/GIS"),
                layer = "EDEN_grid_poly_Jan_10_WCA1")
wca2b <- readOGR(dsn = path.expand("./data/GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA2B")
wca3b <- readOGR(dsn = path.expand("./data/GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA3B")
pw <- readOGR(dsn = path.expand("./data/GIS"),
              layer = "EDEN_grid_poly_Jan_10_PW")
other <- readOGR(dsn = path.expand("./data/GIS"),
                 layer = "EDEN_grid_poly_OTHER_noWCA2A_noL67ext_noWCA3A")
wca2a <- readOGR(dsn = path.expand("./data/GIS"),
                 layer = "EDEN_grid_poly_WCA2A")
l67ext <- readOGR(dsn = path.expand("./data/GIS"),
                  layer = "EDEN_grid_poly_east_L67ext")
wca3a <- readOGR(dsn = path.expand("./data/GIS"),
                 layer = "EDEN_grid_poly_WCA3A")

##### Create dataframes of the polygon centroids #####
wca1_coords <- wca1@data[, c("X_COORD", "Y_COORD")]
wca2b_coords <- wca2b@data[, c("X_COORD", "Y_COORD")]
wca3b_coords <- wca3b@data[, c("X_COORD", "Y_COORD")]
pw_coords <- pw@data[, c("X_COORD", "Y_COORD")]
other_coords <- other@data[, c("X_COORD", "Y_COORD")]
wca2a_coords <- wca2a@data[, c("X_COORD", "Y_COORD")]
l67ext_coords <- l67ext@data[, c("X_COORD", "Y_COORD")]
wca3a_coords <- wca3a@data[, c("X_COORD", "Y_COORD")]

# convert to anisotropic coords for: EDEN-centroids
wca1_anis <- as.data.frame(coords.aniso(coords = wca1_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(wca1_anis) <- c("x_aniso", "y_aniso") 

wca2b_anis <- as.data.frame(coords.aniso(coords = wca2b_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca2b_anis) <- c("x_aniso", "y_aniso")

wca3b_anis <- as.data.frame(coords.aniso(coords = wca3b_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca3b_anis) <- c("x_aniso", "y_aniso")

pw_anis <- as.data.frame(coords.aniso(coords = pw_coords, 
                                      aniso.pars = c(350, 31/30)))
colnames(pw_anis) <- c("x_aniso", "y_aniso")

other_anis <- as.data.frame(coords.aniso(coords = other_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(other_anis) <- c("x_aniso", "y_aniso")

wca2a_anis <- as.data.frame(coords.aniso(coords = wca2a_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca2a_anis) <- c("x_aniso", "y_aniso")

l67ext_anis <- as.data.frame(coords.aniso(coords = l67ext_coords, 
                                          aniso.pars = c(350, 31/30)))
colnames(l67ext_anis) <- c("x_aniso", "y_aniso")

wca3a_anis <- as.data.frame(coords.aniso(coords = wca3a_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca3a_anis) <- c("x_aniso", "y_aniso")

#------------------------------------------------------------------------------
# Set up netcdf info 
rotate <- function(x) t(apply(x, 2, rev))   # rotates matrix 90 degrees clockwise

xDim            <- 287            #number of columns
yDim            <- 405            #number of rows
cell_size       <- 400            #cell resolution
extent          <- c(463400, 577800, 2951800, 2790200)  #UTM coordinates

out_layer       <- "depth"  
#layerLength     <- length(out_layer)
long_layer_name <- "Water Depth (cm)"
out_units       <- "cm"
out_prec        <- "float"
background      <- -32768                                       
source_name    <- "interp_sim_depths.r"
institution    <- "Wetland and Aquatic Research Center, USGS"
qaqc           <- "under review"
comments       <- "Product derived from RBF interpolation of water stage over the EDEN extent"

#------------------------------------------------------------------------------
# INTERPOLATION OF EVER4CAST SIMULATION DATA & OUTPUT NETCDF ------------------

startTime <- proc.time()

## Get Data ## ----------------------------------------------------------------

# DEM
dem <- read.csv("./data/werp_dem_400m_cm.csv", stringsAsFactors = FALSE)

# List of simulations
sims <- list.files("./20171205_2017-spa-eden-analog-output_daily_median_data",
                   full.names = TRUE)
sim_names <- substr(sims, 58, 65)
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
for(i in 1:length(sims)){              # i <- 1

  df <- sim_list[[i]]
  daily_list <- split(df, df$Date) # list of daily dataframes
  
  # Run simulation for each day
  sim_stage_list <- lapply(daily_list, interpolate_gauges_csv)

  # Add column for date
  sim_stage <- rbindlist(sim_stage_list)
  sim_stage$date <- rep(names(daily_list), sapply(sim_stage_list, nrow))

  # Find depth
  sim_depth <- merge(sim_stage, dem, by.x = c("X_COORD", "Y_COORD"), 
                     by.y = c("x_coord", "y_coord"))
  sim_depth$depth <- sim_depth$stage - sim_depth$DEM_cm
  sim_depth <- within(sim_depth, rm(stage, DEM_cm))
  
  # Write netcdf
  output_file <- paste0("./output/", sim_names[i], "_depth", ".nc")
  
  nc_out <- createNetCDFfile(out.name = output_file,
                             layer.name = out_layer,
                             units = out_units,
                             prec = out_prec,                  #'short' 'integer' 'float' 'double' 'char' 'byte'.
                             long.name = long_layer_name,
                             daily = TRUE,                     # 'TRUE' is daily  or arbitary time step;  'FALSE' is annual time step
                             extent = extent,                  # c(UTMx.left, UTMx.right, UTMy.top, UTMy.bottom)
                             cell.size = cell_size,
                             fill.value = background,
                             source.name = source_name,        #  software that created the file: name, version number, & configuration info
                             institution = institution,        # institution or organization that created the file
                             qaqc = qaqc,                      # quality control/quality assurance status
                             comments = comments,              # ~ 1 to 3 sentence explanation of what this file contains.
                             start.dateStr = start_date_str, 
                             t.size = tDim                     # vector of time steps: supersedes "daily" attribute
  )
  
  # Write data for each time step as a matrix to export as netcdf
  for(t in 1:tDim){                               # t <- 1
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
duration[3]/60/60*4  # Number of hours for 100 simulation
duration[3]/60/60*4/24  # Number of days for 100 simulations
