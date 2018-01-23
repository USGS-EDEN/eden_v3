#-------------------------------------------------------
# REPRODUCTION OF EDEN SURFACE INTERPOLATION
# TRIAL 1: WITH ANISOTROPY ANGLE & RATIO, ETA = 16.77, RHO = 0
#-------------------------------------------------------

library(geospt)
#library(dplyr)
library(raster)
library(rgdal)
library(geoR)

options(digits = 15)

nad_utm <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

## read in stage values at gages ----

# unaltered text file of daily means downloaded from EDEN website
gages <- read.table("E:/altEden/gage_data/2008_q4_DM_v2r1/20081001_median.txt", 
                    header = TRUE, sep = "\t", stringsAsFactors = FALSE)
head(gages)
gages <- gages[, c("Station", "X", "Y", "Daily.median.water.level..in.cm.")]
colnames(gages)[4] <- "stage_cm"

# fix incorrect location of gauge G-3567
gages$X[gages$Station == "G-3567"] <- 556509
gages$Y[gages$Station == "G-3567"] <- 2864737


## add in points to pseudo-replicate the L67-Extension canal ----
# which is located in 'other'

pt1 <- gages$stage_cm[gages$Station == "S333+H"] - 23.0509
pt2 <- gages$stage_cm[gages$Station == "S333+H"] - 43.92989
pt3 <- gages$stage_cm[gages$Station == "S333+H"] - 63.20402

pt4 <- gages$stage_cm[gages$Station == "S333+T"] - 1.2068
pt5 <- gages$stage_cm[gages$Station == "S333+T"] + 1.0407
pt6 <- gages$stage_cm[gages$Station == "S333+T"] + 3.098

gages[nrow(gages) + 1, ] <- list("l67ext_pt1", 532760.759, 2846703.198, pt1)
gages[nrow(gages) + 1, ] <- list("l67ext_pt2", 532851.2435, 2844104.773, pt2)
gages[nrow(gages) + 1, ] <- list("l67ext_pt3", 532934.7678, 2841706.227, pt3)

gages[nrow(gages) + 1, ] <- list("l67ext_pt4", 532810.759, 2846703.198, pt4)
gages[nrow(gages) + 1, ] <- list("l67ext_pt5", 532901.2435, 2844104.773, pt5)
gages[nrow(gages) + 1, ] <- list("l67ext_pt6", 532984.7678, 2841706.227, pt6)


## convert to sp* ojbect ----

# convert to sp points df
coordinates(gages) <- ~ X + Y
proj4string(gages) <- nad_utm

## read in polygon shapefiles of the 5 subzones ----
wca1 <- readOGR(dsn = path.expand("E:/altEden/GIS_run2/EDEN_zones_GIS"),
                layer = "EDEN_grid_poly_Jan_10_WCA1")
wca2b <- readOGR(dsn = path.expand("E:/altEden/GIS_run2/EDEN_zones_GIS"),
                layer = "EDEN_grid_poly_Jan_10_WCA2B")
wca3b <- readOGR(dsn = path.expand("E:/altEden/GIS_run2/EDEN_zones_GIS"),
                layer = "EDEN_grid_poly_Jan_10_WCA3B")
pw <- readOGR(dsn = path.expand("E:/altEden/GIS_run2/EDEN_zones_GIS"),
                layer = "EDEN_grid_poly_Jan_10_PW")
other <- readOGR(dsn = path.expand("E:/altEden/GIS_run2/EDEN_zones_GIS"),
                          layer = "EDENGRID_Jan_10_NO_WCA1_2B_3B_PW")
head(wca2b)
plot(pw)

## find which gages are in which of the 5 subzones &  make subdivide the data ----

subgages <- over(gages, wca1) # find which pts are within the subzone
subgages <- na.omit(subgages) # remove the pts which are not within the subzone
wca1_gages <- gages[row.names(subgages), ] # subset original df by row index
wca1_gages@data # check

subgages <- over(gages, wca2b)
subgages <- na.omit(subgages) 
wca2b_gages <- gages[row.names(subgages), ]
wca2b_gages@data 

subgages <- over(gages, wca3b)
subgages <- na.omit(subgages) 
wca3b_gages <- gages[row.names(subgages), ]
wca3b_gages@data 

subgages <- over(gages, pw)
subgages <- na.omit(subgages) 
pw_gages <- gages[row.names(subgages), ]
pw_gages@data 

subgages <- over(gages, other)
subgages <- na.omit(subgages) 
other_gages <- gages[row.names(subgages), ]
other_gages@data 

plot(other_gages, pch = 1, cex = other_gages$stage_cm/200)

## create dataframes of the polygon centroids ----
# and change column names to match the gage spdf
# which is required for rbf

wca1_coords <- wca1@data[, c("X_COORD", "Y_COORD")]
colnames(wca1_coords) <- c("X", "Y")
wca2b_coords <- wca2b@data[, c("X_COORD", "Y_COORD")]
colnames(wca2b_coords) <- c("X", "Y")
wca3b_coords <- wca3b@data[, c("X_COORD", "Y_COORD")]
colnames(wca3b_coords) <- c("X", "Y")
pw_coords <- pw@data[, c("X_COORD", "Y_COORD")]
colnames(pw_coords) <- c("X", "Y")
other_coords <- other@data[, c("X_COORD", "Y_COORD")]
colnames(other_coords) <- c("X", "Y")

##  perform rbf interpolation ----

# run rbf
# add output of predicted stage values to df of original coordinates
wca1_rbf <- rbf(stage_cm ~ X + Y, data = wca1_gages, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca1_coords) 
wca1_rbf <- subset(wca1_rbf, select = -var1.var)
colnames(wca1_rbf)[3] <- "alt_stage_cm"
head(wca1_rbf)


wca2b_rbf <- rbf(stage_cm ~ X + Y, data = wca2b_gages, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca2b_coords) 
wca2b_rbf <- subset(wca2b_rbf, select = -var1.var)
colnames(wca2b_rbf)[3] <- "alt_stage_cm"
head(wca2b_rbf)


wca3b_rbf <- rbf(stage_cm ~ X + Y, data = wca3b_gages, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = wca3b_coords) 
wca3b_rbf <- subset(wca3b_rbf, select = -var1.var)
colnames(wca3b_rbf)[3] <- "alt_stage_cm"
head(wca3b_rbf)

# note the number of neighbors here
pw_rbf <- rbf(stage_cm ~ X + Y, data = pw_gages, func = "M", 
                eta = 0, rho = 0, n.neigh = 5, newdata = pw_coords) 
pw_rbf <- subset(pw_rbf, select = -var1.var)
colnames(pw_rbf)[3] <- "alt_stage_cm"
head(pw_rbf)


other_rbf <- rbf(stage_cm ~ X + Y, data = other_gages, func = "M", 
                eta = 0, rho = 0, n.neigh = 8, newdata = other_coords) 
other_rbf <- subset(other_rbf, select = -var1.var)
colnames(other_rbf)[3] <- "alt_stage_cm"
head(other_rbf)

## combine together & convert to raster ----

alt_eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf)

coordinates(alt_eden) <- ~ x + y
proj4string(alt_eden) <- nad_utm
gridded(alt_eden) <- TRUE
alt_eden <- raster(alt_eden)
plot(alt_eden)

## compare with official EDEN surface  ----

eden <- raster("E:/altEden/EDEN_surfaces/2008_q4_tiff_v2r1/s_20081001.tif")
plot(eden)

# compare extent, grid
alt_eden
eden

eden_diff <- alt_eden - eden # predicted - actual
plot(eden_diff)

# summary stats - useless but to compare against older output
cellStats(eden_diff, 'mean')
cellStats(eden_diff, 'sd')

# find RMSE
diff <- na.omit(getValues(eden_diff))
sqrt(mean(diff^2))

## export ----
writeRaster(alt_eden, "E:/altEden/R_run2/output/altEden_Trial5_l67ext_20081001.tif")
writeRaster(eden_diff, "E:/altEden/R_run2/output/edenDiff_Trial5_l67ext_20081001.tif")
