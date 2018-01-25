#------------------------------------------------------------------------------
# THIS SCRIPT CONTAINS A SERIES OF FUNCTIONS FOR RUNNING A WATER SURFACE 
# INTERPOLATION OVER THE EDEN EXTENT

# THE SCRIPT HERE ACCEPTS CSV MEDIANS FILE AS A STAGE FILE FOR THE GAUGES

# THESE FUNCTIONS DO NOT REQUIRE THE FILES IN THE 'DATA' FOLDER TO RUN 
# BECAUSE THEY SHOULD ALREADY BE LOADED (MAKING THE INTERPOLATION FASTER)

#------------------------------------------------------------------------------

print("These functions require these libraries to be installed: geospt, raster, rgdal, geoR")

library(geospt)
library(raster)
library(rgdal)
library(geoR)

#------------------------------------------------------------------------------
# TO SPEED UP THE INTERPOLATION, SOME PARTS OF THE INTERPOLATION MUST BE DONE
# OUTSIDE THE FUNCTION:

id <- read.csv("./Output/gauge_name_EArea_id.csv")
nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#### read in polygon shapefiles of all the subzones  ####
wca1 <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
                layer = "EDEN_grid_poly_Jan_10_WCA1")
wca2b <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
                 layer = "EDEN_grid_poly_Jan_10_WCA2B")
wca3b <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
                 layer = "EDEN_grid_poly_Jan_10_WCA3B")
pw <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
              layer = "EDEN_grid_poly_Jan_10_PW")
other <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
                 layer = "EDEN_grid_poly_OTHER_noWCA2A_noL67ext_noWCA3A")
wca2a <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
                 layer = "EDEN_grid_poly_WCA2A")
l67ext <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
                  layer = "EDEN_grid_poly_east_L67ext")
wca3a <- readOGR(dsn = path.expand("./GIS/V3_subzones"),
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


##### 1 - FUNCTION TO RUN RBF INTEROPLATION OF GAUGES #### ---------------------
# Output: raster interpolation of water level (stage) in same units as input

interpolate_gauges_csv <- function(gages){
  print("Preparing data...")
  
  station_index <- grep("Station", colnames(gages), ignore.case = TRUE)
  colnames(gages)[station_index] <- "gauge_name"
  
  if("EArea" %in% colnames(gages)) {
    gages <- within(gages, rm("EArea"))
  }
  
  ### MERGE WITH ID COLUMN OF EVER4CAST GAUGE NAMES ###
  # need rows for pseudo & semi gauges; columns for EArea & both versions of gauge names
  gages$X <- round(gages$X, digits = 1)
  gages$Y <- round(gages$Y, digits = 1)
  gages <- merge(id, gages, all.x = TRUE, by.x = c("x", "y"), by.y = c("X", "Y"))
  stage_index <- grep("median", colnames(gages), ignore.case = TRUE) 
  colnames(gages)[stage_index] <- "stage_cm"
  gages <- gages[, c("x", "y", "Station", "EArea", "stage_cm")]
  
  #### ADD PSEUDO- & SEMI- GAUGE STAGE VALUES ####
  # add 4 semi-gauges stage values (2 have the same name)
  gages[gages$Station == "pBCA19+LO1", ]$stage_cm <- gages[gages$Station == "BCA19+", ]$stage_cm
  gages[gages$Station == "pNP202NE1", ]$stage_cm <- gages[gages$Station == "NP202", ]$stage_cm
  gages[gages$Station == "pS12D_DN", ]$stage_cm <- gages[gages$Station == "S12D_DN", ]$stage_cm
  
  # add 5 pseudo-gauge stage values
  # the one on the wca1 - wca2a border reduces error in the northern pt
  gages[gages$Station == "pseudo_canal_1", ]$stage_cm <- gages[gages$Station == "S10D_DN", ]$stage_cm
  # create linear eqns for the four on the wca3a border
  # adding these reduce error on the 3A side
  upper <- gages[gages$Station == "S151+H", ]$stage_cm
  lower <- gages[gages$Station == "S333-H", ]$stage_cm
  slope <- (upper - lower) / 32.57 # 32.57 km between the two gauges (as canal distance, not straight-line)
  # 13.48 km = dist b/t upper gauge and pseudo gauge 2
  gages[gages$Station == "pseudo_canal_2", ]$stage_cm <- round(upper - (slope * 13.48)) #279
  # 15.88 km = dist b/t upper gauge and pseudo gauge 3
  gages[gages$Station == "pseudo_canal_3", ]$stage_cm <- round(upper - (slope * 15.88)) #277
  # 25.28 km = dist b/t upper gauge and pseudo gauge 4
  gages[gages$Station == "pseudo_canal_4", ]$stage_cm <- round(upper - (slope * 25.28)) #268
  # 30.14 km = dist b/t upper gauge and pseudo gauge 5
  gages[gages$Station == "pseudo_canal_5", ]$stage_cm <- round(upper - (slope * 30.14)) #264
  
  #### ADD 'EArea' VALUE to the pseudo-gauges ####
  gages[gages$Station == "pseudo_canal_1", ]$EArea <- "Water Conservation Area 2A"
  gages[gages$Station == "pseudo_canal_2", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_3", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_4", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_5", ]$EArea <- "Water Conservation Area 3A"
  
  ## remove gauges that don't have measurements for that day....
  no_na_values <- sum(is.na(gages$stage_cm))
  print("The number of missing gauges on this day is: ", no_na_values)
  print(no_na_values)
  gages <- na.omit(gages)
  
  # SORT GAGES INTO SUBZONES --------------------------------------------------
  # selection criteria were found in RBFInterpolation.py
  # as well as determined manually by examining output
  
  wca1_gages <- gages[gages$EArea == "Water Conservation Area 1" | gages$EArea == "L39 Canal" | gages$EArea == "L40 Canal", ]
  wca2b_gages <- gages[gages$EArea == "Water Conservation Area 2B" | gages$EArea == "L38E Canal" & gages$Station != "S7-T", ]
  wca3b_gages <- gages[gages$EArea == "Water Conservation Area 3B" & gages$Station != "S9A-T" & gages$Station != "SITE_69E", ] 
  pw_gages <- gages[gages$EArea == "Pennsuco Wetlands" & gages$Station != "S380-H" & gages$Station != "NWWF", ] 
  wca2a_gages <- gages[gages$EArea == "Water Conservation Area 2A" | 
                         gages$Station == "S7-T", ] 
  wca3a_gages <- gages[gages$EArea == "Water Conservation Area 3A" | 
                         gages$EArea == "Miami Canal" |
                         gages$EArea == "L28 Canal" |
                         gages$EArea == "Tamiami Canal" |
                         gages$Station == "EDEN_6" |
                         gages$EArea == "L28 Interceptor Canal",]
  # gages for new subzone east of L67-ext:
  l67ext_gages <- gages[gages$Station == "S334-H" | gages$Station == "S333-T" | 
                          gages$Station == "NESRS1" | gages$Station == "NESRS2" |
                          gages$Station == "G-3578" | gages$Station == "G-3577" |
                          gages$Station == "G-3575" | gages$Station == "G-3576" |
                          gages$Station == "G-3574" | gages$Station == "G-3272" |
                          gages$Station == "G-3273" | gages$Station == "ANGEL.S" |
                          gages$Station == "G-596"| gages$Station == "NESRS4" |
                          gages$Station == "pNP202NE1" | gages$Station == "G-3626" |
                          gages$Station == "G-3628" | gages$Station == "G-3437" |
                          gages$Station == "RG1" | gages$Station == "RG3" |
                          gages$Station == "RG2" | gages$Station == "NP206" |
                          gages$Station == "P33" | gages$Station == "NP202" |
                          gages$Station == "NP203", ]
  
  # 'other' subzone more complicated to sort
  # first: create the 'other' subzone as usual 
  # (the one ifelse command has this part: & gages$Station != "pBCA19+LO1" could try running it that way too)
  other_gages <- gages[gages$EArea != "Water Conservation Area 1" & gages$EArea != "L39 Canal" & 
                         gages$EArea != "L40 Canal"  & gages$EArea != "Water Conservation Area 2B" &
                         gages$EArea != "L38E Canal" & gages$EArea != "Water Conservation Area 3B" & 
                         gages$EArea != "Pennsuco Wetlands" & gages$EArea != "Water Conservation Area 2A" & 
                         gages$Station != "S7-T" & gages$EArea != "L30 Canal" & 
                         gages$Station != "S31M-H" & gages$Station != "S380-H" &
                         gages$Station != "NWWF" | gages$Station == "SITE_69E" |
                         gages$Station == "S9A-T", ]
  # second, remove certain gages east of l67-ext that could influence 'other' surface
  other_gages <- other_gages[other_gages$Station != "NESRS1" & other_gages$Station != "NESRS2" &
                               other_gages$Station != "G-3576" & other_gages$Station != "G-3574" &
                               other_gages$Station != "S334-H" & other_gages$Station != "S334-T" &
                               other_gages$Station != "S333-T", ]
  # third, remove gages that are in WCA3A
  other_gages <- other_gages[other_gages$EArea != "Water Conservation Area 3A" &
                               other_gages$EArea != "Tamiami Canal" & 
                               other_gages$EArea != "Miami Canal" |
                               other_gages$Station == "3ASW+", ]
  
  # don't have to do this with new EDEN files?!
  # going to include it JUST IN CASE 
  # fix incorrect location of gauge G-3567
  # fyi: this gauge NOT located in EVER4CAST files
  gages$X[gages$Station == "G-3567"] <- 556509
  gages$Y[gages$Station == "G-3567"] <- 2864737
  
  ##### STARTING SPATIAL ANALYSIS ########## ----------------------------------
  print("Starting spatial analysis...")
  
  ####### CONVERT both gages and EDEN-centroids to anisotropic coords #########
  
  # conver to anisotropic coords for: gages
  wca1_gages_anis <- as.data.frame(coords.aniso(coords = wca1_gages[, c("x", "y")], 
                                                aniso.pars = c(350, 31/30)))
  # new & identical coord column names
  colnames(wca1_gages_anis) <- c("x_aniso", "y_aniso")  
  # add stage values back to gage df
  wca1_gages_anis$stage_cm <- wca1_gages$stage_cm
  
  
  wca2b_gages_anis <- as.data.frame(coords.aniso(coords = wca2b_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca2b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2b_gages_anis$stage_cm <- wca2b_gages$stage_cm
  
  
  wca3b_gages_anis <- as.data.frame(coords.aniso(coords = wca3b_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca3b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3b_gages_anis$stage_cm <- wca3b_gages$stage_cm
  
  
  pw_gages_anis <- as.data.frame(coords.aniso(coords = pw_gages[, c("x", "y")], 
                                              aniso.pars = c(350, 31/30)))
  colnames(pw_gages_anis) <- c("x_aniso", "y_aniso")  
  pw_gages_anis$stage_cm <- pw_gages$stage_cm
  
  
  other_gages_anis <- as.data.frame(coords.aniso(coords = other_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(other_gages_anis) <- c("x_aniso", "y_aniso")  
  other_gages_anis$stage_cm <- other_gages$stage_cm
  
  
  wca2a_gages_anis <- as.data.frame(coords.aniso(coords = wca2a_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca2a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2a_gages_anis$stage_cm <- wca2a_gages$stage_cm
  
  
  l67ext_gages_anis <- as.data.frame(coords.aniso(coords = l67ext_gages[, c("x", "y")], 
                                                  aniso.pars = c(350, 31/30)))
  colnames(l67ext_gages_anis) <- c("x_aniso", "y_aniso")  
  l67ext_gages_anis$stage_cm <- l67ext_gages$stage_cm
  
  
  wca3a_gages_anis <- as.data.frame(coords.aniso(coords = wca3a_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca3a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3a_gages_anis$stage_cm <- wca3a_gages$stage_cm
  
  ######### PERFORM RBF INTERPOLATION ######### -------------------------------
  print("Running RBF spatial interpolation...")
  
  # convert gage data to spatial-points-dataframe for the RBF interpolation
  coordinates(wca1_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca1_gages_anis) <- nad_utm
  # run rbf
  wca1_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca1_gages_anis, func = "M", 
                  eta = 0, rho = 0, n.neigh = 8, newdata = wca1_anis) 
  # add output of predicted stage values to df of original coordinates
  wca1_rbf <- cbind(wca1_coords, round(wca1_rbf$var1.pred))
  colnames(wca1_rbf)[3] <- "stage"
  
  coordinates(wca2b_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca2b_gages_anis) <- nad_utm
  wca2b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2b_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca2b_anis) 
  wca2b_rbf <- cbind(wca2b_coords, round(wca2b_rbf$var1.pred))
  colnames(wca2b_rbf)[3] <- "stage"
  
  coordinates(wca3b_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca3b_gages_anis) <- nad_utm
  wca3b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3b_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca3b_anis) 
  wca3b_rbf <- cbind(wca3b_coords, round(wca3b_rbf$var1.pred))
  colnames(wca3b_rbf)[3] <- "stage"
  
  # NOTE: only 3 neighbors here
  coordinates(pw_gages_anis) <- ~x_aniso + y_aniso
  proj4string(pw_gages_anis) <- nad_utm
  pw_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = pw_gages_anis, func = "M", 
                eta = 0, rho =0, n.neigh = 3, newdata = pw_anis) 
  pw_rbf <- cbind(pw_coords, round(pw_rbf$var1.pred))
  colnames(pw_rbf)[3] <- "stage"
  
  coordinates(other_gages_anis) <- ~x_aniso + y_aniso
  proj4string(other_gages_anis) <- nad_utm
  other_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = other_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = other_anis) 
  other_rbf <- cbind(other_coords, round(other_rbf$var1.pred))
  colnames(other_rbf)[3] <- "stage"
  
  coordinates(wca2a_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca2a_gages_anis) <- nad_utm
  wca2a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2a_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca2a_anis) 
  wca2a_rbf <- cbind(wca2a_coords, round(wca2a_rbf$var1.pred))
  colnames(wca2a_rbf)[3] <- "stage"
  
  coordinates(l67ext_gages_anis) <- ~x_aniso + y_aniso
  proj4string(l67ext_gages_anis) <- nad_utm
  l67ext_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = l67ext_gages_anis, func = "M", 
                    eta = 0, rho = 0, n.neigh = 8, newdata = l67ext_anis) 
  l67ext_rbf <- cbind(l67ext_coords, round(l67ext_rbf$var1.pred))
  colnames(l67ext_rbf)[3] <- "stage"
  
  coordinates(wca3a_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca3a_gages_anis) <- nad_utm
  wca3a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3a_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca3a_anis) 
  wca3a_rbf <- cbind(wca3a_coords, round(wca3a_rbf$var1.pred))
  colnames(wca3a_rbf)[3] <- "stage"
  
  ####### Combine together ###### -------------------------
  alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf,
                    l67ext_rbf, wca3a_rbf)
  
  return(alt_eden)
}  

##### 2 - FUNCTION THAT CREATES DIFFERENCE RASTER W REAL EDEN #### ------------

find_diff_raster <- function(alt_eden_raster, real_eden_raster){
    
  real_eden_raster <- round(real_eden_raster)
  eden_diff <- alt_eden_raster - real_eden_raster # predicted - actual
  return(eden_diff)
}
  
##### 3 - FUNCTION THAT FINDS RMSE BETWEEN ALTEDEN & REAL EDEN #### ------------

find_rmse <- function(alt_eden_raster, real_eden_raster){
 
  real_eden_raster <- round(real_eden_raster)
  diff_raster <- alt_eden_raster - real_eden_raster # predicted - actual 
  diff <- na.omit(getValues(diff_raster))
  rmse <- sqrt(mean(diff^2))
  return(rmse)
}

##### 4 - FUNCTION THAT FINDS THE DEPTH FROM THE EDEN EXTENT #### --------------

find_eden_depth <- function(stage_raster){
  eden_dem <- raster("./data/GIS/eden_em_oc10_cm.tif")
  depth_raster <- stage_raster - eden_dem
  return(depth_raster)
}

##### 5 - FUNCTION THAT FINDS THE DEPTH FROM THE WERP EXTENT #### --------------

find_werp_depth <- function(stage_raster){
  werp_dem <- raster("./data/GIS/werpeden_400m_cm.tif")
  werp_dem <- crop(werp_dem, stage_raster)
  depth_raster <- stage_raster - werp_dem
  return(depth_raster)
}

##### 6 - FUNCTION TO RUN RBF INTEROPLATION OF GAUGES & EXPORT RASTER #### --------
# Output: raster interpolation of water level (stage) in same units as input

interpolate_gauges_raster <- function(gages){
  print("Preparing data...")
  
  station_index <- grep("Station", colnames(gages), ignore.case = TRUE)
  colnames(gages)[station_index] <- "gauge_name"
  
  if("EArea" %in% colnames(gages)) {
    gages <- within(gages, rm("EArea"))
  }
  
  ### MERGE WITH ID COLUMN OF EVER4CAST GAUGE NAMES ###
  # need rows for pseudo & semi gauges; columns for EArea & both versions of gauge names
  gages$X <- round(gages$X, digits = 1)
  gages$Y <- round(gages$Y, digits = 1)
  gages <- merge(id, gages, all.x = TRUE, by.x = c("x", "y"), by.y = c("X", "Y"))
  stage_index <- grep("median", colnames(gages), ignore.case = TRUE) 
  colnames(gages)[stage_index] <- "stage_cm"
  gages <- gages[, c("x", "y", "Station", "EArea", "stage_cm")]
  
  #### ADD PSEUDO- & SEMI- GAUGE STAGE VALUES ####
  # add 4 semi-gauges stage values (2 have the same name)
  gages[gages$Station == "pBCA19+LO1", ]$stage_cm <- gages[gages$Station == "BCA19+", ]$stage_cm
  gages[gages$Station == "pNP202NE1", ]$stage_cm <- gages[gages$Station == "NP202", ]$stage_cm
  gages[gages$Station == "pS12D_DN", ]$stage_cm <- gages[gages$Station == "S12D_DN", ]$stage_cm
  
  # add 5 pseudo-gauge stage values
  # the one on the wca1 - wca2a border reduces error in the northern pt
  gages[gages$Station == "pseudo_canal_1", ]$stage_cm <- gages[gages$Station == "S10D_DN", ]$stage_cm
  # create linear eqns for the four on the wca3a border
  # adding these reduce error on the 3A side
  upper <- gages[gages$Station == "S151+H", ]$stage_cm
  lower <- gages[gages$Station == "S333-H", ]$stage_cm
  slope <- (upper - lower) / 32.57 # 32.57 km between the two gauges (as canal distance, not straight-line)
  # 13.48 km = dist b/t upper gauge and pseudo gauge 2
  gages[gages$Station == "pseudo_canal_2", ]$stage_cm <- round(upper - (slope * 13.48)) #279
  # 15.88 km = dist b/t upper gauge and pseudo gauge 3
  gages[gages$Station == "pseudo_canal_3", ]$stage_cm <- round(upper - (slope * 15.88)) #277
  # 25.28 km = dist b/t upper gauge and pseudo gauge 4
  gages[gages$Station == "pseudo_canal_4", ]$stage_cm <- round(upper - (slope * 25.28)) #268
  # 30.14 km = dist b/t upper gauge and pseudo gauge 5
  gages[gages$Station == "pseudo_canal_5", ]$stage_cm <- round(upper - (slope * 30.14)) #264
  
  #### ADD 'EArea' VALUE to the pseudo-gauges ####
  gages[gages$Station == "pseudo_canal_1", ]$EArea <- "Water Conservation Area 2A"
  gages[gages$Station == "pseudo_canal_2", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_3", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_4", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_5", ]$EArea <- "Water Conservation Area 3A"
  
  ## remove gauges that don't have measurements for that day....
  no_na_values <- sum(is.na(gages$stage_cm))
  print("The number of missing gauges on this day is: ", no_na_values)
  print(no_na_values)
  gages <- na.omit(gages)
  
  # SORT GAGES INTO SUBZONES --------------------------------------------------
  # selection criteria were found in RBFInterpolation.py
  # as well as determined manually by examining output
  
  wca1_gages <- gages[gages$EArea == "Water Conservation Area 1" | gages$EArea == "L39 Canal" | gages$EArea == "L40 Canal", ]
  wca2b_gages <- gages[gages$EArea == "Water Conservation Area 2B" | gages$EArea == "L38E Canal" & gages$Station != "S7-T", ]
  wca3b_gages <- gages[gages$EArea == "Water Conservation Area 3B" & gages$Station != "S9A-T" & gages$Station != "SITE_69E", ] 
  pw_gages <- gages[gages$EArea == "Pennsuco Wetlands" & gages$Station != "S380-H" & gages$Station != "NWWF", ] 
  wca2a_gages <- gages[gages$EArea == "Water Conservation Area 2A" | 
                         gages$Station == "S7-T", ] 
  wca3a_gages <- gages[gages$EArea == "Water Conservation Area 3A" | 
                         gages$EArea == "Miami Canal" |
                         gages$EArea == "L28 Canal" |
                         gages$EArea == "Tamiami Canal" |
                         gages$Station == "EDEN_6" |
                         gages$EArea == "L28 Interceptor Canal",]
  # gages for new subzone east of L67-ext:
  l67ext_gages <- gages[gages$Station == "S334-H" | gages$Station == "S333-T" | 
                          gages$Station == "NESRS1" | gages$Station == "NESRS2" |
                          gages$Station == "G-3578" | gages$Station == "G-3577" |
                          gages$Station == "G-3575" | gages$Station == "G-3576" |
                          gages$Station == "G-3574" | gages$Station == "G-3272" |
                          gages$Station == "G-3273" | gages$Station == "ANGEL.S" |
                          gages$Station == "G-596"| gages$Station == "NESRS4" |
                          gages$Station == "pNP202NE1" | gages$Station == "G-3626" |
                          gages$Station == "G-3628" | gages$Station == "G-3437" |
                          gages$Station == "RG1" | gages$Station == "RG3" |
                          gages$Station == "RG2" | gages$Station == "NP206" |
                          gages$Station == "P33" | gages$Station == "NP202" |
                          gages$Station == "NP203", ]
  
  # 'other' subzone more complicated to sort
  # first: create the 'other' subzone as usual 
  # (the one ifelse command has this part: & gages$Station != "pBCA19+LO1" could try running it that way too)
  other_gages <- gages[gages$EArea != "Water Conservation Area 1" & gages$EArea != "L39 Canal" & 
                         gages$EArea != "L40 Canal"  & gages$EArea != "Water Conservation Area 2B" &
                         gages$EArea != "L38E Canal" & gages$EArea != "Water Conservation Area 3B" & 
                         gages$EArea != "Pennsuco Wetlands" & gages$EArea != "Water Conservation Area 2A" & 
                         gages$Station != "S7-T" & gages$EArea != "L30 Canal" & 
                         gages$Station != "S31M-H" & gages$Station != "S380-H" &
                         gages$Station != "NWWF" | gages$Station == "SITE_69E" |
                         gages$Station == "S9A-T", ]
  # second, remove certain gages east of l67-ext that could influence 'other' surface
  other_gages <- other_gages[other_gages$Station != "NESRS1" & other_gages$Station != "NESRS2" &
                               other_gages$Station != "G-3576" & other_gages$Station != "G-3574" &
                               other_gages$Station != "S334-H" & other_gages$Station != "S334-T" &
                               other_gages$Station != "S333-T", ]
  # third, remove gages that are in WCA3A
  other_gages <- other_gages[other_gages$EArea != "Water Conservation Area 3A" &
                               other_gages$EArea != "Tamiami Canal" & 
                               other_gages$EArea != "Miami Canal" |
                               other_gages$Station == "3ASW+", ]
  
  # don't have to do this with new EDEN files?!
  # fix incorrect location of gauge G-3567
  # this gauge NOT located in EVER4CAST files
  #gages$X[gages$Station == "G-3567"] <- 556509
  #gages$Y[gages$Station == "G-3567"] <- 2864737
  
  ##### STARTING SPATIAL ANALYSIS ########## ----------------------------------
  print("Starting spatial analysis...")
  
  ####### CONVERT both gages and EDEN-centroids to anisotropic coords #########
  
  # conver to anisotropic coords for: gages
  wca1_gages_anis <- as.data.frame(coords.aniso(coords = wca1_gages[, c("x", "y")], 
                                                aniso.pars = c(350, 31/30)))
  # new & identical coord column names
  colnames(wca1_gages_anis) <- c("x_aniso", "y_aniso")  
  # add stage values back to gage df
  wca1_gages_anis$stage_cm <- wca1_gages$stage_cm
  
  
  wca2b_gages_anis <- as.data.frame(coords.aniso(coords = wca2b_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca2b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2b_gages_anis$stage_cm <- wca2b_gages$stage_cm
  
  
  wca3b_gages_anis <- as.data.frame(coords.aniso(coords = wca3b_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca3b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3b_gages_anis$stage_cm <- wca3b_gages$stage_cm
  
  
  pw_gages_anis <- as.data.frame(coords.aniso(coords = pw_gages[, c("x", "y")], 
                                              aniso.pars = c(350, 31/30)))
  colnames(pw_gages_anis) <- c("x_aniso", "y_aniso")  
  pw_gages_anis$stage_cm <- pw_gages$stage_cm
  
  
  other_gages_anis <- as.data.frame(coords.aniso(coords = other_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(other_gages_anis) <- c("x_aniso", "y_aniso")  
  other_gages_anis$stage_cm <- other_gages$stage_cm
  
  
  wca2a_gages_anis <- as.data.frame(coords.aniso(coords = wca2a_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca2a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2a_gages_anis$stage_cm <- wca2a_gages$stage_cm
  
  
  l67ext_gages_anis <- as.data.frame(coords.aniso(coords = l67ext_gages[, c("x", "y")], 
                                                  aniso.pars = c(350, 31/30)))
  colnames(l67ext_gages_anis) <- c("x_aniso", "y_aniso")  
  l67ext_gages_anis$stage_cm <- l67ext_gages$stage_cm
  
  
  wca3a_gages_anis <- as.data.frame(coords.aniso(coords = wca3a_gages[, c("x", "y")], 
                                                 aniso.pars = c(350, 31/30)))
  colnames(wca3a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3a_gages_anis$stage_cm <- wca3a_gages$stage_cm
  
  ######### PERFORM RBF INTERPOLATION ######### -------------------------------
  print("Running RBF spatial interpolation...")
  
  # convert gage data to spatial-points-dataframe for the RBF interpolation
  coordinates(wca1_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca1_gages_anis) <- nad_utm
  # run rbf
  wca1_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca1_gages_anis, func = "M", 
                  eta = 0, rho = 0, n.neigh = 8, newdata = wca1_anis) 
  # add output of predicted stage values to df of original coordinates
  wca1_rbf <- cbind(wca1_coords, round(wca1_rbf$var1.pred))
  colnames(wca1_rbf)[3] <- "stage"
  
  coordinates(wca2b_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca2b_gages_anis) <- nad_utm
  wca2b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2b_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca2b_anis) 
  wca2b_rbf <- cbind(wca2b_coords, round(wca2b_rbf$var1.pred))
  colnames(wca2b_rbf)[3] <- "stage"
  
  coordinates(wca3b_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca3b_gages_anis) <- nad_utm
  wca3b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3b_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca3b_anis) 
  wca3b_rbf <- cbind(wca3b_coords, round(wca3b_rbf$var1.pred))
  colnames(wca3b_rbf)[3] <- "stage"
  
  # NOTE: only 3 neighbors here
  coordinates(pw_gages_anis) <- ~x_aniso + y_aniso
  proj4string(pw_gages_anis) <- nad_utm
  pw_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = pw_gages_anis, func = "M", 
                eta = 0, rho =0, n.neigh = 3, newdata = pw_anis) 
  pw_rbf <- cbind(pw_coords, round(pw_rbf$var1.pred))
  colnames(pw_rbf)[3] <- "stage"
  
  coordinates(other_gages_anis) <- ~x_aniso + y_aniso
  proj4string(other_gages_anis) <- nad_utm
  other_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = other_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = other_anis) 
  other_rbf <- cbind(other_coords, round(other_rbf$var1.pred))
  colnames(other_rbf)[3] <- "stage"
  
  coordinates(wca2a_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca2a_gages_anis) <- nad_utm
  wca2a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2a_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca2a_anis) 
  wca2a_rbf <- cbind(wca2a_coords, round(wca2a_rbf$var1.pred))
  colnames(wca2a_rbf)[3] <- "stage"
  
  coordinates(l67ext_gages_anis) <- ~x_aniso + y_aniso
  proj4string(l67ext_gages_anis) <- nad_utm
  l67ext_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = l67ext_gages_anis, func = "M", 
                    eta = 0, rho = 0, n.neigh = 8, newdata = l67ext_anis) 
  l67ext_rbf <- cbind(l67ext_coords, round(l67ext_rbf$var1.pred))
  colnames(l67ext_rbf)[3] <- "stage"
  
  coordinates(wca3a_gages_anis) <- ~x_aniso + y_aniso
  proj4string(wca3a_gages_anis) <- nad_utm
  wca3a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3a_gages_anis, func = "M", 
                   eta = 0, rho = 0, n.neigh = 8, newdata = wca3a_anis) 
  wca3a_rbf <- cbind(wca3a_coords, round(wca3a_rbf$var1.pred))
  colnames(wca3a_rbf)[3] <- "stage"
  
  ####### Combine together ###### -------------------------
  alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf,
                    l67ext_rbf, wca3a_rbf)
  
  # making the raster
  coordinates(alt_eden) <- ~X_COORD + Y_COORD
  proj4string(alt_eden) <- nad_utm
  gridded(alt_eden) <- TRUE
  alt_eden_raster <- raster(alt_eden)
  
  return(alt_eden_raster)
}  



