#------------------------------------------------------------------------------
# RUNS A WATER SURFACE INTERPOLATION OVER THE EDEN EXTENT 
# (referred to as EDEN version 3)

# INPUT: 
# dataframe with coordinates (named "X" and "Y") in NAD83 UTM 17N
# With median water level (stage) in cm for 1 day
# Column that contains water level must have the word "median" in the name
# Column that contains the gage names must have "station" in the name

# OUTPUT:
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
id <- read.csv("./Output/gauge_name_EArea_id.csv")
  # SHOULD THIS BE INSIDE THE FUNCTION??

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

interpolate_gages <- function(gages, format = "df"){
  print("Preparing data...")

  nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  station_index <- grep("Station", colnames(gages), ignore.case = TRUE)
  colnames(gages)[station_index] <- "gage_name"
  
  if("EArea" %in% colnames(gages)) {
    gages <- within(gages, rm("EArea"))
  }
  
  ### MERGE WITH ID COLUMN OF EVER4CAST GAGE NAMES ###
  # need rows for pseudo & semi gages; columns for EArea & both versions of gage names
  # since the names are always changing, better to join based on location
  gages$X <- round(gages$X, digits = 1)
  gages$Y <- round(gages$Y, digits = 1)
  gages <- merge(id, gages, all.x = TRUE, by.x = c("x", "y"), by.y = c("X", "Y"))
  stage_index <- grep("median", colnames(gages), ignore.case = TRUE) 
  colnames(gages)[stage_index] <- "stage_cm"
  gages <- gages[, c("x", "y", "Station", "EArea", "stage_cm")]
  
  #### ADD PSEUDO- & SEMI- GAGE STAGE VALUES ####
  # add 4 semi-gages stage values (2 have the same name)
  gages[gages$Station == "pBCA19+LO1", ]$stage_cm <- gages[gages$Station == "BCA19+", ]$stage_cm
  gages[gages$Station == "pNP202NE1", ]$stage_cm <- gages[gages$Station == "NP202", ]$stage_cm
  gages[gages$Station == "pS12D_DN", ]$stage_cm <- gages[gages$Station == "S12D_DN", ]$stage_cm
  
  # add 5 pseudo-gage stage values
  # the one on the wca1 - wca2a border reduces error in the northern pt
  gages[gages$Station == "pseudo_canal_1", ]$stage_cm <- gages[gages$Station == "S10D_DN", ]$stage_cm
  # create linear eqns for the four on the wca3a border
  # adding these reduce error on the 3A side
  upper <- gages[gages$Station == "S151+H", ]$stage_cm
  lower <- gages[gages$Station == "S333-H", ]$stage_cm
  slope <- (upper - lower) / 32.57 # 32.57 km between the two gages (as canal distance, not straight-line)
  # 13.48 km = dist b/t upper gage and pseudo gage 2
  gages[gages$Station == "pseudo_canal_2", ]$stage_cm <- round(upper - (slope * 13.48)) #279
  # 15.88 km = dist b/t upper gage and pseudo gage 3
  gages[gages$Station == "pseudo_canal_3", ]$stage_cm <- round(upper - (slope * 15.88)) #277
  # 25.28 km = dist b/t upper gage and pseudo gage 4
  gages[gages$Station == "pseudo_canal_4", ]$stage_cm <- round(upper - (slope * 25.28)) #268
  # 30.14 km = dist b/t upper gage and pseudo gage 5
  gages[gages$Station == "pseudo_canal_5", ]$stage_cm <- round(upper - (slope * 30.14)) #264
  
  # #### ADD 'EArea' VALUE to the pseudo-gages ####
  gages[gages$Station == "pseudo_canal_1", ]$EArea <- "Water Conservation Area 2A"
  gages[gages$Station == "pseudo_canal_2", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_3", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_4", ]$EArea <- "Water Conservation Area 3A"
  gages[gages$Station == "pseudo_canal_5", ]$EArea <- "Water Conservation Area 3A"
  
  ## remove gages that don't have measurements for that day....
  no_na_values <- sum(is.na(gages$stage_cm))
  print(paste0("The number of missing gages on this day is: ", no_na_values))
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
  
  
  ##### STARTING SPATIAL ANALYSIS ########## ----------------------------------
  print("Starting spatial analysis...")
  
  ####### CONVERT both gages and EDEN-centroids to anisotropic coords #########
  
  # conver to anisotropic coords for: gages
  wca1_gages_anis <- as.data.frame(coords.aniso(coords = wca1_gages[, c("x", "y")], 
                                                aniso.pars = c(350*pi/180, 31/30)))
  # new & identical coord column names
  colnames(wca1_gages_anis) <- c("x_aniso", "y_aniso")  
  # add stage values back to gage df
  wca1_gages_anis$stage_cm <- wca1_gages$stage_cm
  
  
  wca2b_gages_anis <- as.data.frame(coords.aniso(coords = wca2b_gages[, c("x", "y")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca2b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2b_gages_anis$stage_cm <- wca2b_gages$stage_cm
  
  
  wca3b_gages_anis <- as.data.frame(coords.aniso(coords = wca3b_gages[, c("x", "y")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca3b_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3b_gages_anis$stage_cm <- wca3b_gages$stage_cm
  
  
  pw_gages_anis <- as.data.frame(coords.aniso(coords = pw_gages[, c("x", "y")], 
                                              aniso.pars = c(350*pi/180, 31/30)))
  colnames(pw_gages_anis) <- c("x_aniso", "y_aniso")  
  pw_gages_anis$stage_cm <- pw_gages$stage_cm
  
  
  other_gages_anis <- as.data.frame(coords.aniso(coords = other_gages[, c("x", "y")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(other_gages_anis) <- c("x_aniso", "y_aniso")  
  other_gages_anis$stage_cm <- other_gages$stage_cm
  
  
  wca2a_gages_anis <- as.data.frame(coords.aniso(coords = wca2a_gages[, c("x", "y")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca2a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca2a_gages_anis$stage_cm <- wca2a_gages$stage_cm
  
  
  l67ext_gages_anis <- as.data.frame(coords.aniso(coords = l67ext_gages[, c("x", "y")], 
                                                  aniso.pars = c(350*pi/180, 31/30)))
  colnames(l67ext_gages_anis) <- c("x_aniso", "y_aniso")  
  l67ext_gages_anis$stage_cm <- l67ext_gages$stage_cm
  
  
  wca3a_gages_anis <- as.data.frame(coords.aniso(coords = wca3a_gages[, c("x", "y")], 
                                                 aniso.pars = c(350*pi/180, 31/30)))
  colnames(wca3a_gages_anis) <- c("x_aniso", "y_aniso")  
  wca3a_gages_anis$stage_cm <- wca3a_gages$stage_cm
  
  ######### PERFORM RBF INTERPOLATION ######### -------------------------------
  print("Running RBF spatial interpolation...")
  
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
