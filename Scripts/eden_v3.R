#------------------------------------------------------------------------------
# REPRODUCTION OF EDEN SURFACE INTERPOLATION
# TRIAL 2 FROM RUN 2: WITH ANISOTROPY ANGLE & RATIO, ETA = 0, RHO = 0
# VERSION 1: REPLICATIN OF RBF PYTHON SCRIPT
# VERSION 2: WITH WCA2A ADDED
# VERSION 3: ADDING 1+ PSEUDO CANAL PTS NORTH OF L67-EXT 
#            ALSO TRYING WITH DIFFERENT DATES
# VERSION 4: WITH SUBZONE EAST OF L67-EXT ADDED
# VERSION 5: WITH SOME PSEUDO-CANAL GAGES ADDED TO WCA2A
# VERSION 6: ADDED WCA3A AS ITS OWN SUBZONE
# VERSION 7: CHANGED SO THAT NOW IMPORT 'MEDIAN' FILE, NOT THE 'EXTENDED' FILE
#              (this means it can be used with EVER4CAST simulations)
#            5 PSEUDO-CANAL LOCATIONS ARE INTERNALLY CALCULATED 
#            4 SEMI-GAUGES ARE NOW INTERNALLY CALCUATED (THE P[SOMETHINGS])
#            [NOTE: DOESN'T WORK WELL WITH EXTENDED FILES - SHOULD SWITCH TO EDEN MEDIAN FILES]
#------------------------------------------------------------------------------

library(geospt)
library(raster)
library(rgdal)
library(geoR)

nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## READ IN STAGE VALUE AT GAUGES ----------------------------------------------

# import data file of gages WITHOUT pseudo canal boundaries and EArea field:
#gage_file <- "../gage_data/20170710_median_flag_v2rt.txt"
#gage_file <- "../gage_data/extended_median_files/20151112_median_Ex.txt"
#gage_file <- "../gage_data/extended_median_files/20160102_median_Ex.txt"
#gage_file <- "../gage_data/extended_median_files/20160304_median_Ex.txt"
#gage_file <- "../gage_data/extended_median_files/20160514_median_Ex.txt"
gages <- read.table(gage_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
colnames(gages)[2] <- "EDEN_station"
head(gages)

## MERGE WITH ID COLUMN, ADD PSEUDO- & SEMI- GAUGE STAGE VALUES ----------------

# import gauge-EArea-ID file
id <- read.csv("../gauge_prep/gauge_name_EArea_id.csv")
# add pseudo & semi gauges, EArea column, and EDEN gauge names
gages <- merge(id, gages, all.x = TRUE, by.x = c("x", "y", "ever4cast_gauge_name"), by.y = c("X", "Y", "EDEN_station"))
# clean up
colnames(gages)[7] <- "stage_cm"
gages <- gages[, c("x", "y", "Station", "EArea", "stage_cm")]

# add 4 semi-gauges stage values (2 have the same name)
gages[gages$Station == "pBCA19+LO1", ]$stage_cm <- gages[gages$Station == "BCA19+", ]$stage_cm
gages[gages$Station == "pNP202NE1", ]$stage_cm <- gages[gages$Station == "NP202", ]$stage_cm
gages[gages$Station == "pS12D_DN", ]$stage_cm <- gages[gages$Station == "S12D_DN", ]$stage_cm

# add 5 pseudo-gauge stage values
# the one on the wca1 - wca2a border
# it reduces error in the northern pt
gages[gages$Station == "pseudo_canal_1", ]$stage_cm <- gages[gages$Station == "S10D_DN", ]$stage_cm

# create linear equation for the four on the wca3a border
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

# add an EArea to the pseudo-gauges
gages[gages$Station == "pseudo_canal_1", ]$EArea <- "Water Conservation Area 2A"
gages[gages$Station == "pseudo_canal_2", ]$EArea <- "Water Conservation Area 3A"
gages[gages$Station == "pseudo_canal_3", ]$EArea <- "Water Conservation Area 3A"
gages[gages$Station == "pseudo_canal_4", ]$EArea <- "Water Conservation Area 3A"
gages[gages$Station == "pseudo_canal_5", ]$EArea <- "Water Conservation Area 3A"

## remove gauges that don't have measurements for that day....
gages <- na.omit(gages)

# SORT GAGES INTO SUBZONES --------------------------------------------------
# selection criteria were found in RBFInterpolation.py
# as well as determined manually by examining output

unique(gages$EArea)

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

# don't have to do this with new files?!
# fix incorrect location of gauge G-3567
# this gauge NOT located in EVER4CAST files
#gages$X[gages$Station == "G-3567"] <- 556509
#gages$Y[gages$Station == "G-3567"] <- 2864737

## read in polygon shapefiles of the 5 subzones -------------------------------
wca1 <- readOGR(dsn = path.expand("../GIS/EDEN_zones_GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA1")
wca2b <- readOGR(dsn = path.expand("../GIS/EDEN_zones_GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA2B")
wca3b <- readOGR(dsn = path.expand("../GIS/EDEN_zones_GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA3B")
pw <- readOGR(dsn = path.expand("../GIS/EDEN_zones_GIS"),
              layer = "EDEN_grid_poly_Jan_10_PW")

other <- readOGR(dsn = path.expand("../GIS"),
                 layer = "EDEN_grid_poly_OTHER_noWCA2A_noL67ext_noWCA3A")
wca2a <- readOGR(dsn = path.expand("../GIS"),
                 layer = "EDEN_grid_poly_WCA2A")
l67ext <- readOGR(dsn = path.expand("../GIS"),
                  layer = "EDEN_grid_poly_east_L67ext")
wca3a <- readOGR(dsn = path.expand("../GIS"),
                 layer = "EDEN_grid_poly_WCA3A")
head(wca2a)
#plot(wca2a)

## Create dataframes of the polygon centroids ---------------------------------

wca1_coords <- wca1@data[, c("X_COORD", "Y_COORD")]
wca2b_coords <- wca2b@data[, c("X_COORD", "Y_COORD")]
wca3b_coords <- wca3b@data[, c("X_COORD", "Y_COORD")]
pw_coords <- pw@data[, c("X_COORD", "Y_COORD")]
other_coords <- other@data[, c("X_COORD", "Y_COORD")]
wca2a_coords <- wca2a@data[, c("X_COORD", "Y_COORD")]
l67ext_coords <- l67ext@data[, c("X_COORD", "Y_COORD")]
wca3a_coords <- wca3a@data[, c("X_COORD", "Y_COORD")]

## Convert both gages and EDEN-centroids to anisotropic coords -------------

# conver to anisotropic coords for: 1:gages, 2:EDEN-centroids
wca1_gages_anis <- as.data.frame(coords.aniso(coords = wca1_gages[, c("x", "y")], 
                                aniso.pars = c(350, 31/30)))
wca1_anis <- as.data.frame(coords.aniso(coords = wca1_coords, 
                                        aniso.pars = c(350, 31/30)))
# new & identical coord column names
colnames(wca1_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca1_anis) <- c("x_aniso", "y_aniso") 
# add stage values back to gage df
wca1_gages_anis$stage_cm <- wca1_gages$stage_cm


wca2b_gages_anis <- as.data.frame(coords.aniso(coords = wca2b_gages[, c("x", "y")], 
                                               aniso.pars = c(350, 31/30)))
wca2b_anis <- as.data.frame(coords.aniso(coords = wca2b_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca2b_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca2b_anis) <- c("x_aniso", "y_aniso")
wca2b_gages_anis$stage_cm <- wca2b_gages$stage_cm


wca3b_gages_anis <- as.data.frame(coords.aniso(coords = wca3b_gages[, c("x", "y")], 
                                              aniso.pars = c(350, 31/30)))
wca3b_anis <- as.data.frame(coords.aniso(coords = wca3b_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(wca3b_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca3b_anis) <- c("x_aniso", "y_aniso")
wca3b_gages_anis$stage_cm <- wca3b_gages$stage_cm


pw_gages_anis <- as.data.frame(coords.aniso(coords = pw_gages[, c("x", "y")], 
                                              aniso.pars = c(350, 31/30)))
pw_anis <- as.data.frame(coords.aniso(coords = pw_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(pw_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(pw_anis) <- c("x_aniso", "y_aniso")
pw_gages_anis$stage_cm <- pw_gages$stage_cm


other_gages_anis <- as.data.frame(coords.aniso(coords = other_gages[, c("x", "y")], 
                                              aniso.pars = c(350, 31/30)))
other_anis <- as.data.frame(coords.aniso(coords = other_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(other_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(other_anis) <- c("x_aniso", "y_aniso")
other_gages_anis$stage_cm <- other_gages$stage_cm


wca2a_gages_anis <- as.data.frame(coords.aniso(coords = wca2a_gages[, c("x", "y")], 
                                               aniso.pars = c(350, 31/30)))
wca2a_anis <- as.data.frame(coords.aniso(coords = wca2a_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca2a_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca2a_anis) <- c("x_aniso", "y_aniso")
wca2a_gages_anis$stage_cm <- wca2a_gages$stage_cm


l67ext_gages_anis <- as.data.frame(coords.aniso(coords = l67ext_gages[, c("x", "y")], 
                                               aniso.pars = c(350, 31/30)))
l67ext_anis <- as.data.frame(coords.aniso(coords = l67ext_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(l67ext_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(l67ext_anis) <- c("x_aniso", "y_aniso")
l67ext_gages_anis$stage_cm <- l67ext_gages$stage_cm


wca3a_gages_anis <- as.data.frame(coords.aniso(coords = wca3a_gages[, c("x", "y")], 
                                               aniso.pars = c(350, 31/30)))
wca3a_anis <- as.data.frame(coords.aniso(coords = wca3a_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca3a_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca3a_anis) <- c("x_aniso", "y_aniso")
wca3a_gages_anis$stage_cm <- wca3a_gages$stage_cm

##  Perform RBF interpolation -------------------------------------------------

# convert gage data to spatial-points-dataframe for the RBF interpolation
coordinates(wca1_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca1_gages_anis) <- nad_utm
# run rbf
wca1_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca1_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca1_anis) 
# add output of predicted stage values to df of original coordinates
wca1_rbf <- cbind(wca1_coords, round(wca1_rbf$var1.pred))
colnames(wca1_rbf)[3] <- "alt_stage_cm"
head(wca1_rbf)


coordinates(wca2b_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca2b_gages_anis) <- nad_utm
wca2b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2b_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca2b_anis) 
wca2b_rbf <- cbind(wca2b_coords, round(wca2b_rbf$var1.pred))
colnames(wca2b_rbf)[3] <- "alt_stage_cm"
head(wca2b_rbf)


coordinates(wca3b_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca3b_gages_anis) <- nad_utm
wca3b_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3b_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca3b_anis) 
wca3b_rbf <- cbind(wca3b_coords, round(wca3b_rbf$var1.pred))
colnames(wca3b_rbf)[3] <- "alt_stage_cm"
head(wca3b_rbf)

# NOTE: only 3 neighbors here
coordinates(pw_gages_anis) <- ~x_aniso + y_aniso
proj4string(pw_gages_anis) <- nad_utm
pw_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = pw_gages_anis, func = "M", 
                eta = 0, rho =0, n.neigh = 3, newdata = pw_anis) 
pw_rbf <- cbind(pw_coords, round(pw_rbf$var1.pred))
colnames(pw_rbf)[3] <- "alt_stage_cm"
head(pw_rbf)


coordinates(other_gages_anis) <- ~x_aniso + y_aniso
proj4string(other_gages_anis) <- nad_utm
other_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = other_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = other_anis) 
other_rbf <- cbind(other_coords, round(other_rbf$var1.pred))
colnames(other_rbf)[3] <- "alt_stage_cm"
head(other_rbf)


coordinates(wca2a_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca2a_gages_anis) <- nad_utm
wca2a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca2a_gages_anis, func = "M", 
                 eta = 0, rho = 0, n.neigh = 8, newdata = wca2a_anis) 
wca2a_rbf <- cbind(wca2a_coords, round(wca2a_rbf$var1.pred))
colnames(wca2a_rbf)[3] <- "alt_stage_cm"
head(wca2a_rbf)

coordinates(l67ext_gages_anis) <- ~x_aniso + y_aniso
proj4string(l67ext_gages_anis) <- nad_utm
l67ext_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = l67ext_gages_anis, func = "M", 
                 eta = 0, rho = 0, n.neigh = 8, newdata = l67ext_anis) 
l67ext_rbf <- cbind(l67ext_coords, round(l67ext_rbf$var1.pred))
colnames(l67ext_rbf)[3] <- "alt_stage_cm"
head(l67ext_rbf)


coordinates(wca3a_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca3a_gages_anis) <- nad_utm
wca3a_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = wca3a_gages_anis, func = "M", 
                 eta = 0, rho = 0, n.neigh = 8, newdata = wca3a_anis) 
wca3a_rbf <- cbind(wca3a_coords, round(wca3a_rbf$var1.pred))
colnames(wca3a_rbf)[3] <- "alt_stage_cm"
head(wca3a_rbf)

## Combine together & Convert to raster ---------------------------------------

alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf,
                  l67ext_rbf, wca3a_rbf)

coordinates(alt_eden) <- ~X_COORD + Y_COORD
proj4string(alt_eden) <- nad_utm
gridded(alt_eden) <- TRUE
alt_eden <- raster(alt_eden)
plot(alt_eden, main = "altEDEN")

## compare with official EDEN surface  ----------------------------------------

# the different eden surface files
#eden_file <- "../EDEN_surfaces/2017_q3_v2rt_geotif/20170710_geotif_v2rt/s_20170710_v2rt.tif"
eden_file <- "../EDEN_surfaces/2015_q4_tiff_v2prov/s_20151112.tif"
#eden_file <- "../EDEN_surfaces/2016_q1_tiff_v2prov/s_20160102.tif"
#eden_file <- "../EDEN_surfaces/2016_q1_tiff_v2prov/s_20160304.tif"
#eden_file <- "../EDEN_surfaces/2016_q2_tiff_v2prov/s_20160514.tif"

eden <- raster(eden_file)
eden <- round(eden_file)
plot(eden, main = "real EDEN")

# compare extent, grid
alt_eden
eden

eden_diff <- alt_eden - eden # predicted - actual
plot(eden_diff, main = "altEDEN - real EDEN (cm)")

# summary stats - useless but to compare against older output
cellStats(eden_diff, 'mean')
cellStats(eden_diff, 'sd')

# find RMSE - the relevant stat
diff <- na.omit(getValues(eden_diff))
sqrt(mean(diff^2))

## export ---------------------------------------------------------------------
#writeRaster(alt_eden, "../R_run3/output/altEden_Trial2_20170710_v6.tif")
#writeRaster(eden_diff, "../R_run3/output/edenDiff_Trial2_20170710_v6.tif")
