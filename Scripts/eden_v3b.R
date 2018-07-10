#------------------------------------------------------------------------------
# FXN (1): RUNS A WATER SURFACE INTERPOLATION OVER THE EDEN EXTENT 
# (referred to as EDEN version 3)

# FXN (2): Creates a NetCDF of water stage from set of median files
# TODO add functionality for water depth


## Function #1: interpolate_gages() ------

  # INPUT: 
  # dataframe with coordinates (named "X" and "Y") in NAD83 UTM 17N
  # With median water level (stage) in cm for 1 day
  # Column that contains water level must have the word "median" in the name
  # Column that contains the gage names must have "station" in the name
  
  # OUTPUT:
  # Specify either a dataframe or a RasterLayer object with format argument
  # Options: format ="df" [default], or format = "raster"

## Function #2: eden_nc() -----

  # INPUT:
  # date vector of surfaces to be produced; should be single quarter, at most 
  #   (or, if known, period of constant edenmaster gages)
  # path and file name for netCDF
  # character flag "files" vs "database"; files defaults

  # OUTPUT: 
  # nothing within R - function write the netCDF externally


# ------------
# Saira Haider 
# shaider@usgs.gov
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey
#------------------------------------------------------------------------------

print("These libraries must be installed: geospt, raster, rgdal, geoR, reshape2, ncdf4, RMySQL")

library(geospt)
library(raster)
library(rgdal)
library(geoR)
library(reshape2)
library(RMySQL)
source("./Scripts/netCDF_IO_v3.1.R")


#------------------------------------------------------------------------------
# Convert subarea grids to anisotropic space outside of the function
# This speeds up how long it takes to run the function

# Import polygon shapefiles of all the subareas
subareas <- c("EDEN_grid_poly_Jan_10_WCA1",
              "EDEN_grid_poly_WCA2A",
              "EDEN_grid_poly_Jan_10_WCA2B",
              "EDEN_grid_poly_WCA3A",
              "EDEN_grid_poly_Jan_10_WCA3B",
              "EDEN_grid_poly_east_L67ext",
              "EDEN_grid_poly_OTHER_noWCA2A_noL67ext_noWCA3A",
              "EDEN_grid_poly_Jan_10_PW")
subareas <- lapply(subareas, readOGR, dsn = path.expand("./GIS/V3_subzones"))
names(subareas) <- c("wca1", "wca2a", "wca2b", "wca3a", "wca3b", "l67ext", 
                     "other", "pw")

# Create dataframes of the polygon centroids from the shapefiles
subareas <- lapply(subareas, function(x) x@data[, c("X_COORD", "Y_COORD")] )

# Create new list of subareas with polygon centroids in anisotropic space
subareas_aniso <- lapply(subareas, function(x) as.data.frame(coords.aniso(coords = x, 
                                                                    aniso.pars = c(350*pi/180, 31/30))))
# Change column names
subareas_aniso <- lapply(subareas_aniso, setNames, c("x_aniso", "y_aniso"))

#------------------------------------------------------------------------------

# Connect to database
usr <- "edenweb"
pword <- "edenweb"
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

#------------------------------------------------------------------------------
# Function that takes gages from a subarea and runs the RBF function

run_eden_rbf <- function(subarea_df, n_neigh, subarea_name){
  # subarea_df <- wca1_gages; n_neigh = 8; subarea_name <- 'wca1'
  
  subarea_index <- grep(subarea_name, names(subareas_aniso))
  
  # Convert df to SpatialPoints df
  coordinates(subarea_df) <- ~x_aniso + y_aniso
  proj4string(subarea_df) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  # Run RBF
  subarea_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, 
                     data = subarea_df, 
                     func = "M", 
                     eta = 0, 
                     rho = 0, 
                     n.neigh = n_neigh, 
                     newdata = subareas_aniso[[subarea_index]]) 
  
  # Final clean up
  subarea_rbf <- cbind(subareas[[subarea_index]], subarea_rbf$var1.pred)
  colnames(subarea_rbf)[3] <- "stage"
  
  return(subarea_rbf)
}


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

## Function #1: Runs a radial basis function on the gages
##   to interpolate a water surface over the EDEN extent


# TODO: add input parameter to specify interp is for ever4cast bc:
#   2 gages that have been discontinued from eden surface: WCA2E4 & WCA2F1

interpolate_gages <- function(input_gages, format = "df", edenmaster = "gage_subareaID_21June2018.csv"){
  
  # Import gage ID file
  id <- read.csv(paste0("./Output/", edenmaster), stringsAsFactors = FALSE)
  
  ## --------------------------------------------------------------------------
  # Add subarea classifications to input gage data
  
  print(paste0("Preparing data for ", input_gages$Date[1]))
  print(paste0("using edenmaster ", edenmaster))
  
  # Change input station colname to avoid errors caused by identical colnames
  station_index <- grep("Station", colnames(input_gages), ignore.case = TRUE)
  colnames(input_gages)[station_index] <- "input_station_name"

  # Merge with ID dataframe to add subarea classification
  # - use coordinates to merge bc they change less often than station names
  input_gages$X <- round(input_gages$X, digits = 1)
  input_gages$Y <- round(input_gages$Y, digits = 1)  
  gages <- merge(id, input_gages, 
                 all.x = TRUE, 
                 by.x = c("x_nad83_utm17n", "y_nad83_utm17n"), 
                 by.y = c("X", "Y"))

  
  # Rename column with water level data
  stage_index <- grep("median", colnames(gages), ignore.case = TRUE) 
  colnames(gages)[stage_index] <- "stage_cm"
  
  # Remove unnecessary columns
  gages <- gages[, c("x_nad83_utm17n", "y_nad83_utm17n", "stage_cm", "Station",
                     "wca1", "wca2a", "wca2b", "wca3a", "wca3b","l67ext", "pw", 
                     "other")]
  
  ## --------------------------------------------------------------------------
  # Add water level values for the 9 'fake' or 'pseudo' gages
  
  # Add values for the 4 pseudo-gages that were generated from the _Ex files
  # - 2 have the same name
  gages[gages$Station == "pBCA19+LO1", ]$stage_cm <- gages[gages$Station == "BCA19+", ]$stage_cm
  gages[gages$Station == "pNP202NE1", ]$stage_cm <- gages[gages$Station == "NP202", ]$stage_cm
  gages[gages$Station == "pS12D_DN", ]$stage_cm <- gages[gages$Station == "S12D_DN", ]$stage_cm
  
  
  # Add water level values for the 5 'pseudo' gages
  # - these are the ones manually determined to increase interpolation accuracy
  # - locations are based on pseudo-canal borders from EDEN_v2
  
  # This one on WCA1 - WCA2A border reduces error in the northern pt
  gages[gages$Station == "pseudo_canal_1", ]$stage_cm <- gages[gages$Station == "S10D_DN", ]$stage_cm
  
  # Create linear eqns for the four on the WCA3A/B border
  # - adding these reduces error on the 3A side
  upper <- gages[gages$Station == "S151+H", ]$stage_cm
  lower <- gages[gages$Station == "S333-H", ]$stage_cm
  # 32.57 km between the two gages (as canal distance, not straight-line)
  slope <- (upper - lower) / 32.57 
  
  # 13.48 km = dist b/t upper gage and pseudo gage 2
  gages[gages$Station == "pseudo_canal_2", ]$stage_cm <- round(upper - (slope * 13.48))
  # 15.88 km = dist b/t upper gage and pseudo gage 3
  gages[gages$Station == "pseudo_canal_3", ]$stage_cm <- round(upper - (slope * 15.88))
  # 25.28 km = dist b/t upper gage and pseudo gage 4
  gages[gages$Station == "pseudo_canal_4", ]$stage_cm <- round(upper - (slope * 25.28))
  # 30.14 km = dist b/t upper gage and pseudo gage 5
  gages[gages$Station == "pseudo_canal_5", ]$stage_cm <- round(upper - (slope * 30.14))
  
  ## --------------------------------------------------------------------------
  ## Remove gages that don't have measurements for that day
  
  no_na_values <- sum(is.na(gages$stage_cm))
  print(paste0("The number of missing gages on this day is: ", no_na_values))
  if (no_na_values) print(paste0("Missing data are from gage stations: ", gages[is.na(gages$stage_cm), ]$Station))
  gages <- na.omit(gages)

  ## --------------------------------------------------------------------------
  # Convert gage data to anisotropic coordinates

  gages_aniso <- as.data.frame(coords.aniso(coords = gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                            aniso.pars = c(350*pi/180, 31/30)))
  colnames(gages_aniso) <- c("x_aniso", "y_aniso")  
  gages <- cbind(gages, gages_aniso)
  
  ## --------------------------------------------------------------------------
  # Create dataframe for each subzone classification
  
  wca1_gages <- gages[gages$wca1 == 1, ]
  wca2a_gages <- gages[gages$wca2a == 1, ]
  wca2b_gages <- gages[gages$wca2b == 1, ]
  wca3a_gages <- gages[gages$wca3a == 1, ]
  wca3b_gages <- gages[gages$wca3b == 1, ]
  pw_gages <- gages[gages$pw == 1, ]
  l67ext_gages <- gages[gages$l67ext == 1, ]
  other_gages <- gages[gages$other == 1, ]
                         
  ## --------------------------------------------------------------------------
  print("Running RBF interpolation...")
  
  wca1_rbf <- run_eden_rbf(wca1_gages, 8, "wca1")
  wca2a_rbf <- run_eden_rbf(wca2a_gages, 8, "wca2a")
  wca2b_rbf <- run_eden_rbf(wca2b_gages, 8, "wca2b")
  wca3a_rbf <- run_eden_rbf(wca3a_gages, 8, "wca3a")
  wca3b_rbf <- run_eden_rbf(wca3b_gages, 8, "wca3b")
  # TODO when running simulations for ever4cast, n_neigh has to be reduced to 3 - mb include this in fxn input for specfiying ever4cast
  pw_rbf <- run_eden_rbf(pw_gages, 5, "pw")
  l67ext_rbf <- run_eden_rbf(l67ext_gages, 8, "l67ext")
  other_rbf <- run_eden_rbf(other_gages, 8, "other")
  
  ## --------------------------------------------------------------------------
  # Bind everything together and convert to raster if specified
  
  alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf,
                    l67ext_rbf, wca3a_rbf)
  
  if(format == "raster"){
    coordinates(alt_eden) <- ~X_COORD + Y_COORD
    proj4string(alt_eden) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    gridded(alt_eden) <- TRUE
    alt_eden <- raster(alt_eden)
  }
  
  return(alt_eden)
}  


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

# Function #2: Creates a NetCDF of EDEN water stage

eden_nc <- function(date_range, output_file, files_database = "files"){
 
  ## --------------------------------------------------------------------------
  # If pulling gage data from the database (files_database = "database"):
  
  if (files_database == "database") {
    # Current quarter
    cur_qtr <- paste0(as.POSIXlt(Sys.Date())$year + 1900, quarters(Sys.Date()))
    # Surface creation quarter
    surf_qtr <- paste0(as.POSIXlt(date_range[1])$year + 1900, quarters(date_range[1]))
    
    gage_query <- "select station_name, station_name_web, agency_acronym as agency, utm_easting, utm_northing, dry_elevation, convert_to_navd88_feet as conv, location, area from station, agency, station_datum, location where station.database_agency_id = agency.agency_id and station.station_id = station_datum.station_id and station.location_id = location.location_id and"
    
    if (cur_qtr == surf_qtr) {
      # List of expected upload gages from EDENdb, realtime
      gage_query = paste(gage_query, "edenmaster_new = 1")
    } else {
      # List of expected upload gages from EDENdb, historic
      surf_qtr_strt <- as.POSIXlt(date_range[1])$year + 1900 + as.numeric(substr(quarters(date_range[1]), 2, 2)) / 4
      surf_qtr_end <- as.POSIXlt(date_range[1])$year + 1900 + as.numeric(substr(quarters(rev(date_range)[1]), 2, 2)) / 4
      gage_query <- paste(gage_query, "station.edenmaster_start_num <=", surf_qtr_strt, "and station.edenmaster_end_num >=", surf_qtr_end)
    }
    
    gage_query <- paste(gage_query, "group by station_name order by agency, station_name_web")
    gages <- dbGetQuery(con, gage_query)
    gages$agency[which(gages$agency == "ENP")] <- "NPS"
   
    # Default dry values for gages missing them
    gages$dry_elevation[which(is.na(gages$dry_elevation))] <- -9999
    
    # Create edenmaster file
    master <- read.csv("./Output/pseudogage_subareaID.csv", stringsAsFactors = FALSE)
    for (i in 1:dim(gages)[1]) {
      bits <- rev(unlist(strsplit(substr(paste(as.integer(intToBits(gages$area[i])), collapse = ""), 1, 8), "")))
      row <- c(gages$station_name[i], gages$location[i], round(gages$utm_easting[i], digits = 1), round(gages$utm_northing[i], digits = 1), bits)
      master <- rbind(master, row)
    }
    
    write.csv(master, "./Output/edenmaster.csv", quote = F, row.names = F)
    
    # Clean input directory
    unlink("./Inputs/gage_data/*")
    # Loop by dates to generate annotated daily median flag files
    for (j in 1:length(date_range)) {
      text <- "Agency	Station	X	Y	Daily Median Water Level (cm, NAVD88)	Date	Data Type"
      write.table(text, paste0("./Inputs/gage_data/", format(date_range[j],"%Y%m%d"), "_median_flag.txt"), quote=F, row.names=F, col.names=F, eol="\r\n")
      for(i in 1:length(gages$station_name_web)) {
        # Select gage data
        query <- paste0("select datetime, `stage_", gages$station_name_web[i], "`+", gages$conv[i], " as stage, `flag_", gages$station_name_web[i], "` as flag from stage where datetime >= ", format(date_range[j], "%Y%m%d010000"), " and datetime < ", format(date_range[j] + 1, "%Y%m%d000001"), " order by datetime")
        db <- dbGetQuery(con, query)
        # Calculate daily flags
        flag <- ifelse(length(which(db$flag == "M")) == 24, "M", ifelse(median(db$stage, na.rm=T) < gages$dry_elevation[i], "D", ifelse(length(which(is.na(db$flag))) > 0, "O", "E")))
        # Calculate medians, generate output text
        text <- c(gages$agency[i], gages$station_name_web[i], round(gages$utm_easting[i], 1), round(gages$utm_northing[i], 1), round(median(db$stage, na.rm=T) * 12 * 2.54), format(date_range[j], "%Y%m%d"), flag)
        write.table(t(text), paste0("./Inputs/gage_data/", format(date_range[j], "%Y%m%d"), "_median_flag.txt"), sep="\t", quote=F, row.names=F, col.names=F, append=T, eol="\r\n")
      }
    }
  }

  ## --------------------------------------------------------------------------
  # If pulling gage data downloaded from the EDEN website
  # (the "Daily Median Output File" for 1 quarter)
  # This is the default setting: files_database = "files"
  
  file_list <- list.files("./Inputs/gage_data/", pattern = "flag.txt", full.names = TRUE)
  # cull to date range
  file_dates <- as.Date(substr(file_list, 21, 28), format = "%Y%m%d")
  file_list <- file_list[file_dates %in% date_range]

  # Import gage data
  gage_list <- lapply(file_list, read.table, sep = "\t", header = TRUE)

  # Find first day of the simulations
  day1 <- min(date_range)
  
  # Time steps
  tDim <- length(date_range)

  ## --------------------------------------------------------------------------
  # Run interpolations & prepare netCDF
  
  # Run interpolation for each day
  interp_list <- lapply(gage_list, interpolate_gages, edenmaster = "edenmaster.csv")
  
  # Find min and max for netCDF attributes
  depth_min <- min(unlist(lapply(interp_list, function(df) min(df$stage))))
  depth_max <- max(unlist(lapply(interp_list, function(df) max(df$stage))))

  ## -------------------------------------------------------------------------
  # Set up netCDF header info 
  
  xDim            <- 287
  yDim            <- 405
  cell_size       <- 400
  extent          <- c(463400, 577800, 2951800, 2790200)
  out_layer       <- "stage"
  long_layer_name <- "Water Stage (cm)"
  out_units       <- "cm"
  out_prec        <- "float"
  background      <- NaN                         
  source_name     <- "eden_v3b.R"
  institution     <- "USGS"
  qaqc            <- "under review"
  comments        <- "Product derived from RBF interpolation of gages over the EDEN extent"
  
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
                             start.dateStr = day1, 
                             t.size = tDim,
                             layer.min = depth_min,
                             layer.max = depth_max
  )
    
  # Convert df of interp to a matrix & export as netcdf
  for(j in 1:tDim){ # j <- 1
    print(paste("Day ", j, " of ", tDim))
    
    dt <- interp_list[[j]]
    # create matrix for netcdf
    out_mat <- acast(dt, X_COORD ~ Y_COORD, value.var = out_layer)
    # rotate matrix
    out_mat <- t(apply(out_mat, 1, rev)) 
    
    vec2nc(nc_out, out_mat, out_layer, j)
  }
  
  closeNetCDF(nc_out) 
  
}
