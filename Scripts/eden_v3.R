#------------------------------------------------------------------------------
# RUNS A WATER SURFACE INTERPOLATION OVER THE EDEN EXTENT 
# (referred to as EDEN version 3)

# INPUT of FXN: 
# dataframe with coordinates (named "X" and "Y") in NAD83 UTM 17N
# With median water level (stage) in cm for 1 day
# Column that contains water level must have the word "median" in the name
# Column that contains the gage names must have "station" in the name

# OUTPUT of FXN:
# Specify either a dataframe or a RasterLayer object with format argument
# Options: format ="df" [default], or format = "raster"


# Saira Haider 
# shaider@usgs.gov
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey
#------------------------------------------------------------------------------

print("These libraries must be installed: geospt, raster, rgdal, geoR")

library(geospt)
library(raster)
library(rgdal)
library(geoR)

# Import files
id <- read.csv("./Output/gage_subareaID_6June2018.csv", stringsAsFactors = FALSE)
  # TODO SHOULD THIS BE INSIDE THE FUNCTION to reduce workspace clutter?

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
## Function that runs an radial basis function on the gages
##   to interpolate a water surface over the EDEN extent

# TODO: add input parameter to specify interp is for ever4cast bc:
#   2 gages that have been discontinued from eden surface: WCA2E4 & WCA2F1??

interpolate_gages <- function(input_gages, format = "df"){
  
  ## --------------------------------------------------------------------------
  # Add subarea classifications to input gage data
  
  print("Preparing data...")

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
                     "wca1", "wca2a", "wca2b", "wca3a", "wca3b","l67ext", "pw", "other")]
  
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
  print(paste0("Missing data are from gage stations: ", gages[is.na(gages$stage_cm), ]$Station))
  gages <- na.omit(gages)
  
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
                         
  ##### STARTING SPATIAL ANALYSIS ########## ----------------------------------
  print("Starting spatial analysis...")
  
  ####### CONVERT both gages and EDEN-centroids to anisotropic coords #########
  
  # conver to anisotropic coords for: gages
  wca1_gages_anis <- as.data.frame(coords.aniso(coords = wca1_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                aniso.pars = c(350*pi/180, 31/30)))
  # new & identical coord column names
  colnames(wca1_gages_anis) <- c("x_aniso", "y_aniso")  
  # add stage values back to gage df
  wca1_gages_anis$stage_cm <- wca1_gages$stage_cm
  
  
  wca2b_gages_anis <- as.data.frame(coords.aniso(coords = wca2b_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca2b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2b_gages_anis$stage_cm <- wca2b_gages$stage_cm
  
  
  wca3b_gages_anis <- as.data.frame(coords.aniso(coords = wca3b_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca3b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3b_gages_anis$stage_cm <- wca3b_gages$stage_cm
  
  
  pw_gages_anis <- as.data.frame(coords.aniso(coords = pw_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                              aniso.pars = c(350*pi/180, 31/30)))
  colnames(pw_gages_anis) <- c("x_aniso", "y_aniso")  
  pw_gages_anis$stage_cm <- pw_gages$stage_cm
  
  
  other_gages_anis <- as.data.frame(coords.aniso(coords = other_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(other_gages_anis) <- c("x_aniso", "y_aniso")  
  other_gages_anis$stage_cm <- other_gages$stage_cm
  
  
  wca2a_gages_anis <- as.data.frame(coords.aniso(coords = wca2a_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca2a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2a_gages_anis$stage_cm <- wca2a_gages$stage_cm
  
  
  l67ext_gages_anis <- as.data.frame(coords.aniso(coords = l67ext_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                  aniso.pars = c(350*pi/180, 31/30)))
  colnames(l67ext_gages_anis) <- c("x_aniso", "y_aniso")  
  l67ext_gages_anis$stage_cm <- l67ext_gages$stage_cm
  
  
  wca3a_gages_anis <- as.data.frame(coords.aniso(coords = wca3a_gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca3a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3a_gages_anis$stage_cm <- wca3a_gages$stage_cm
  
  ######### PERFORM RBF INTERPOLATION ######### -------------------------------
  print("Running RBF spatial interpolation...")
  nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  # convert gage data to spatial-points-dataframe for the RBF interpolation
  coordinates(wca1_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca1_gages_anis) <- nad_utm
  # run rbf
  wca1_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca1_gages_anis, func = "M", 
                  eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$wca1) 
  # add output of predicted stage values to df of original coordinates
  wca1_rbf <- cbind(subareas$wca1, round(wca1_rbf$var1.pred))
  colnames(wca1_rbf)[3] <- "stage"
  
  coordinates(wca2b_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca2b_gages_anis) <- nad_utm
  wca2b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2b_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$wca2b) 
  wca2b_rbf <- cbind(subareas$wca2b, round(wca2b_rbf$var1.pred))
  colnames(wca2b_rbf)[3] <- "stage"
  
  coordinates(wca3b_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca3b_gages_anis) <- nad_utm
  wca3b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3b_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$wca3b) 
  wca3b_rbf <- cbind(subareas$wca3b, round(wca3b_rbf$var1.pred))
  colnames(wca3b_rbf)[3] <- "stage"
  
  # NOTE: only 3 neighbors here
  coordinates(pw_gages_anis) <- ~x_aniso + y_aniso
  proj4string(pw_gages_anis) <- nad_utm
  pw_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = pw_gages_anis, func = "M", 
                eta = 0, rho =0, n.neigh = 3, newdata = subareas_aniso$pw) 
  pw_rbf <- cbind(subareas$pw, round(pw_rbf$var1.pred))
  colnames(pw_rbf)[3] <- "stage"
  
  coordinates(other_gages_anis) <- ~x_aniso + y_aniso
  proj4string(other_gages_anis) <- nad_utm
  other_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = other_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$other) 
  other_rbf <- cbind(subareas$other, round(other_rbf$var1.pred))
  colnames(other_rbf)[3] <- "stage"
  
  coordinates(wca2a_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca2a_gages_anis) <- nad_utm
  wca2a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2a_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$wca2a) 
  wca2a_rbf <- cbind(subareas$wca2a, round(wca2a_rbf$var1.pred))
  colnames(wca2a_rbf)[3] <- "stage"
  
  coordinates(l67ext_gages_anis) <- ~x_aniso + y_aniso
  proj4string(l67ext_gages_anis) <- nad_utm
  l67ext_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = l67ext_gages_anis, func = "M", 
                    eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$l67ext) 
  l67ext_rbf <- cbind(subareas$l67ext, round(l67ext_rbf$var1.pred))
  colnames(l67ext_rbf)[3] <- "stage"
  
  coordinates(wca3a_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca3a_gages_anis) <- nad_utm
  wca3a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3a_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = subareas_aniso$wca3a) 
  wca3a_rbf <- cbind(subareas$wca3a, round(wca3a_rbf$var1.pred))
  colnames(wca3a_rbf)[3] <- "stage"
  
  ####### Combine together ###### -------------------------
  alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf,
                    l67ext_rbf, wca3a_rbf)
  
  if(format == "raster"){
    coordinates(alt_eden) <- ~X_COORD + Y_COORD
    proj4string(alt_eden) <- nad_utm
    gridded(alt_eden) <- TRUE
    alt_eden <- raster(alt_eden)
  }
  
  return(alt_eden)
}  
