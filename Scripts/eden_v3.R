#------------------------------------------------------------------------------
# REPRODUCTION OF EDEN SURFACE INTERPOLATION
# TRIAL 2: WITH ANISOTROPY ANGLE & RATIO, ETA = 0, RHO = 0
# VERSION 2: WITH WCA2A ADDED
# VERSION 3: ADDING TO V2, 1+ PSEUDO CANAL PTS NORTH OF L67-EXT 
#            ALSO TRYING WITH DIFFERENT DATES
# VERSION 4: WITH SUBZONE EAST OF L67-EXT ADDED
#------------------------------------------------------------------------------

library(geospt)
library(raster)
library(rgdal)
library(geoR)

nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## read in stage values at gages ----------------------------------------------

# import data file of gages with pseudo canal boundaries and EArea field:
# there are a few different options
#gage_file <- "E:/altEden/fromHeather/20170710_median_Ex.txt"
#gage_file <- "E:/altEden/gage_data/extended_median_files/20151112_median_Ex.txt"
#gage_file <- "E:/altEden/gage_data/extended_median_files/20160102_median_Ex.txt"
gage_file <- "E:/altEden/gage_data/extended_median_files/20160304_median_Ex.txt"
#gage_file <- "E:/altEden/gage_data/extended_median_files/20160514_median_Ex.txt"
gages <- read.table(gage_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
head(gages)

# additional pseudo-canal gage to keep & add to 'other':
(canal <- gages[gages$FID == "856" | gages$FID == "830"| gages$FID == "772"| gages$FID == "783", ])

# remove pseudo canal boundaries
gages <- gages[gages$EArea != "", ]
  # have to use EArea and not Station because there is a problem with 3 stations that are in here twice
  # but with different names, and the incorrect ones don't have anything in the EArea field

# round median values to integers??
# no - because it appears that EDEN team only does this for water depth, not water level

# SUBDIVIDE GAGES BY SUBZONE --------------------------------------------------
# selection criteria were found in RBFInterpolation.py

unique(gages$EArea)

wca1_gages <- gages[gages$EArea == "Water Conservation Area 1" | gages$EArea == "L39 Canal" | gages$EArea == "L40 Canal", ]
wca2b_gages <- gages[gages$EArea == "Water Conservation Area 2B" | gages$EArea == "L38E Canal" & gages$Station != "S7-T", ]
wca3b_gages <- gages[gages$EArea == "Water Conservation Area 3B" & gages$Station != "S9A-T" & gages$Station != "SITE_69E", ] 
pw_gages <- gages[gages$EArea == "Pennsuco Wetlands" & gages$Station != "S380-H" & gages$Station != "NWWF", ] 
wca2a_gages <- gages[gages$EArea == "Water Conservation Area 2A" | gages$Station == "S7-T", ] 

# new subzone east of L67-ext:
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

# 'other' subzone now needs to exclude certain pts from l67ext subzone that could influence it negatively
# first: create the 'other' subzone as usual
# the one ifelse command has this part: & gages$Station != "pBCA19+LO1" could try running it that way too
other_gages <- gages[gages$EArea != "Water Conservation Area 1" & gages$EArea != "L39 Canal" & 
                     gages$EArea != "L40 Canal"  & gages$EArea != "Water Conservation Area 2B" &
                     gages$EArea != "L38E Canal" & gages$EArea != "Water Conservation Area 3B" & 
                     gages$EArea != "Pennsuco Wetlands" & gages$EArea != "Water Conservation Area 2A" & 
                     gages$Station != "S7-T" & gages$EArea != "L30 Canal" & 
                     gages$Station != "S31M-H" & gages$Station != "S380-H" &
                     gages$Station != "NWWF" | gages$Station == "SITE_69E" |
                     gages$Station == "S9A-T", ]
# second, remove certain gages east of l67-ext
other_gages <- other_gages[other_gages$Station != "NESRS1" & other_gages$Station != "NESRS2" &
                           other_gages$Station != "G-3576" & other_gages$Station != "G-3574" &
                           other_gages$Station != "S334-H" & other_gages$Station != "S334-T" &
                           other_gages$Station != "S333-T", ]
# add pseudo canal gages:
other_gages <- rbind(other_gages, canal)


# don't have to do this with new files?!
# fix incorrect location of gauge G-3567
#gages$X[gages$Station == "G-3567"] <- 556509
#gages$Y[gages$Station == "G-3567"] <- 2864737

## read in polygon shapefiles of the 5 subzones -------------------------------
wca1 <- readOGR(dsn = path.expand("E:/altEden/GIS/EDEN_zones_GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA1")
wca2b <- readOGR(dsn = path.expand("E:/altEden/GIS/EDEN_zones_GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA2B")
wca3b <- readOGR(dsn = path.expand("E:/altEden/GIS/EDEN_zones_GIS"),
                 layer = "EDEN_grid_poly_Jan_10_WCA3B")
pw <- readOGR(dsn = path.expand("E:/altEden/GIS/EDEN_zones_GIS"),
              layer = "EDEN_grid_poly_Jan_10_PW")
other <- readOGR(dsn = path.expand("E:/altEden/GIS"),
                 layer = "EDEN_grid_poly_OTHER_noWCA2A_noL67ext")
wca2a <- readOGR(dsn = path.expand("E:/altEden/GIS"),
                 layer = "EDEN_grid_poly_WCA2A")
l67ext <- readOGR(dsn = path.expand("E:/altEden/GIS"),
                  layer = "EDEN_grid_poly_east_L67ext")
head(wca2a)
#plot(wca2a)

## create dataframes of the polygon centroids ---------------------------------

wca1_coords <- wca1@data[, c("X_COORD", "Y_COORD")]
wca2b_coords <- wca2b@data[, c("X_COORD", "Y_COORD")]
wca3b_coords <- wca3b@data[, c("X_COORD", "Y_COORD")]
pw_coords <- pw@data[, c("X_COORD", "Y_COORD")]
other_coords <- other@data[, c("X_COORD", "Y_COORD")]
wca2a_coords <- wca2a@data[, c("X_COORD", "Y_COORD")]
l67ext_coords <- l67ext@data[, c("X_COORD", "Y_COORD")]

## convert both gages and polygon centroids to anisotropic coords -------------

# convert both the gage & centroids coordinates
# rename coordinates column names
# add stage values back to gage df
wca1_gages_anis <- as.data.frame(coords.aniso(coords = wca1_gages[, 3:4], 
                                aniso.pars = c(350, 31/30)))
wca1_anis <- as.data.frame(coords.aniso(coords = wca1_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(wca1_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca1_anis) <- c("x_aniso", "y_aniso") 
wca1_gages_anis$MEDIAN <- wca1_gages$MEDIAN


wca2b_gages_anis <- as.data.frame(coords.aniso(coords = wca2b_gages[, 3:4], 
                                               aniso.pars = c(350, 31/30)))
wca2b_anis <- as.data.frame(coords.aniso(coords = wca2b_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca2b_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca2b_anis) <- c("x_aniso", "y_aniso")
wca2b_gages_anis$MEDIAN <- wca2b_gages$MEDIAN


wca3b_gages_anis <- as.data.frame(coords.aniso(coords = wca3b_gages[, 3:4], 
                                              aniso.pars = c(350, 31/30)))
wca3b_anis <- as.data.frame(coords.aniso(coords = wca3b_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(wca3b_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca3b_anis) <- c("x_aniso", "y_aniso")
wca3b_gages_anis$MEDIAN <- wca3b_gages$MEDIAN


pw_gages_anis <- as.data.frame(coords.aniso(coords = pw_gages[, 3:4], 
                                              aniso.pars = c(350, 31/30)))
pw_anis <- as.data.frame(coords.aniso(coords = pw_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(pw_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(pw_anis) <- c("x_aniso", "y_aniso")
pw_gages_anis$MEDIAN <- pw_gages$MEDIAN


other_gages_anis <- as.data.frame(coords.aniso(coords = other_gages[, 3:4], 
                                              aniso.pars = c(350, 31/30)))
other_anis <- as.data.frame(coords.aniso(coords = other_coords, 
                                        aniso.pars = c(350, 31/30)))
colnames(other_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(other_anis) <- c("x_aniso", "y_aniso")
other_gages_anis$MEDIAN <- other_gages$MEDIAN

wca2a_gages_anis <- as.data.frame(coords.aniso(coords = wca2a_gages[, 3:4], 
                                               aniso.pars = c(350, 31/30)))
wca2a_anis <- as.data.frame(coords.aniso(coords = wca2a_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(wca2a_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(wca2a_anis) <- c("x_aniso", "y_aniso")
wca2a_gages_anis$MEDIAN <- wca2a_gages$MEDIAN

l67ext_gages_anis <- as.data.frame(coords.aniso(coords = l67ext_gages[, 3:4], 
                                               aniso.pars = c(350, 31/30)))
l67ext_anis <- as.data.frame(coords.aniso(coords = l67ext_coords, 
                                         aniso.pars = c(350, 31/30)))
colnames(l67ext_gages_anis) <- c("x_aniso", "y_aniso")  
colnames(l67ext_anis) <- c("x_aniso", "y_aniso")
l67ext_gages_anis$MEDIAN <- l67ext_gages$MEDIAN


##  perform rbf interpolation -------------------------------------------------

# convert gage data to spatialpointsdf for rbf
# run rbf
# add output of predicted stage values to df of original coordinates
coordinates(wca1_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca1_gages_anis) <- nad_utm
wca1_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = wca1_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca1_anis) 
wca1_rbf <- cbind(wca1_coords, wca1_rbf$var1.pred)
colnames(wca1_rbf)[3] <- "alt_stage_cm"
head(wca1_rbf)


coordinates(wca2b_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca2b_gages_anis) <- nad_utm
wca2b_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = wca2b_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca2b_anis) 
wca2b_rbf <- cbind(wca2b_coords, wca2b_rbf$var1.pred)
colnames(wca2b_rbf)[3] <- "alt_stage_cm"
head(wca2b_rbf)


coordinates(wca3b_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca3b_gages_anis) <- nad_utm
wca3b_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = wca3b_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca3b_anis) 
wca3b_rbf <- cbind(wca3b_coords, wca3b_rbf$var1.pred)
colnames(wca3b_rbf)[3] <- "alt_stage_cm"
head(wca3b_rbf)

# note the number of neighbors here
coordinates(pw_gages_anis) <- ~x_aniso + y_aniso
proj4string(pw_gages_anis) <- nad_utm
pw_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = pw_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 6, newdata = pw_anis) 
pw_rbf <- cbind(pw_coords, pw_rbf$var1.pred)
colnames(pw_rbf)[3] <- "alt_stage_cm"
head(pw_rbf)


coordinates(other_gages_anis) <- ~x_aniso + y_aniso
proj4string(other_gages_anis) <- nad_utm
other_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = other_gages_anis, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = other_anis) 
other_rbf <- cbind(other_coords, other_rbf$var1.pred)
colnames(other_rbf)[3] <- "alt_stage_cm"
head(other_rbf)


coordinates(wca2a_gages_anis) <- ~x_aniso + y_aniso
proj4string(wca2a_gages_anis) <- nad_utm
wca2a_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = wca2a_gages_anis, func = "M", 
                 eta = 0, rho = 0, n.neigh = 8, newdata = wca2a_anis) 
wca2a_rbf <- cbind(wca2a_coords, wca2a_rbf$var1.pred)
colnames(wca2a_rbf)[3] <- "alt_stage_cm"
head(wca2a_rbf)

coordinates(l67ext_gages_anis) <- ~x_aniso + y_aniso
proj4string(l67ext_gages_anis) <- nad_utm
l67ext_rbf <- rbf(MEDIAN ~ x_aniso + y_aniso, data = l67ext_gages_anis, func = "M", 
                 eta = 0, rho = 0, n.neigh = 8, newdata = l67ext_anis) 
l67ext_rbf <- cbind(l67ext_coords, l67ext_rbf$var1.pred)
colnames(l67ext_rbf)[3] <- "alt_stage_cm"
head(l67ext_rbf)

## combine together & convert to raster ---------------------------------------

alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf,
                  l67ext_rbf)

coordinates(alt_eden) <- ~X_COORD + Y_COORD
proj4string(alt_eden) <- nad_utm
gridded(alt_eden) <- TRUE
alt_eden <- raster(alt_eden)
plot(alt_eden, main = "altEDEN")

## compare with official EDEN surface  ----------------------------------------

# the different eden surface files
#eden_file <- "E:/altEden/EDEN_surfaces/2017_q3_v2rt_geotif/20170710_geotif_v2rt/s_20170710_v2rt.tif"
#eden_file <- "E:/altEden/EDEN_surfaces/2015_q4_tiff_v2prov/s_20151112.tif"
#eden_file <- "E:/altEden/EDEN_surfaces/2016_q1_tiff_v2prov/s_20160102.tif"
eden_file <- "E:/altEden/EDEN_surfaces/2016_q1_tiff_v2prov/s_20160304.tif"
#eden_file <- "E:/altEden/EDEN_surfaces/2016_q2_tiff_v2prov/s_20160514.tif"

eden <- raster(eden_file)
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
#writeRaster(alt_eden, "E:/altEden/R_run3/output/altEden_Trial2_20170710_v4.tif")
#writeRaster(eden_diff, "E:/altEden/R_run3/output/edenDiff_Trial2_20170710_v4.tif")
