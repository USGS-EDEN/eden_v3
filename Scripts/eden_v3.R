#-------------------------------------------------------
# NOTES AND QUESTIONS TO BE ADDRESSED: !!!!!!!!
# - try more experimenting with eta and rho
# - leonard's note about the levee extension
# fix so that defns and file locations is more proper?
# what about that section to the east of WCA3B 
# - can that be made into its own polygon?

#-------------------------------------------------------
# REPRODUCTION OF EDEN SURFACE INTERPOLATION USING RBF
#-------------------------------------------------------

library(sp)
library(geospt)
library(dplyr)
library(raster)

#-------------------------------------------------------
# DEFINTIONS AND FILES

# working directory
home_folder <- "E:/altEden/R_run1"

# input files:
# unaltered text file of daily means downloaded from EDEN website
gauge_df <- read.table("E:/altEden/gage_data/2008_q4_DM_v2r1/20081001_median.txt", 
                        header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# USGS EDEN interpolation file
eden_tiff <- "E:/altEden/EDEN_surfaces/2008_q4_tiff_v2r1/s_20081001.tif"

# csv of eden grid, includes region location for each grid cell
edenFile <- "E:/altEden/GIS_run1/EDEN_grid_pts.csv"

# csv that relates the gauge station to region location
station_id <- read.csv("E:/altEden/R_run1/station_region_id.csv", 
                       header = TRUE, stringsAsFactors = FALSE)

# output file
# format: rbf_output_#nearestneighbors_yearmonthday.csv
rbf_output <- "altEDEN_8n_20081001"

# RBF Parameters
p_neigh <- 8
p_rho <- 0
p_eta <- 0

#-------------------------------------------------------
# PREPARE GAUGE DAILY MEANS DATA

# fix water stage column name
# 5th column is the stage value from downloaded csvs
colnames(gauge_df)[5] <- "stage_cm"
head(gauge_df)

# fix incorrect location of gauge G-3567
gauge_df$X[gauge_df$Station == "G-3567"] <- 556509
gauge_df$Y[gauge_df$Station == "G-3567"] <- 2864737

# add column for the regional location of each gauge
gaugeMeans <- left_join(gauge_df, station_id, by = ("Station" = "Station"))
head(gaugeMeans)

# str(gaugeMeans)
#options(digits = 10)
#head(gaugeMeans)

coordinates(gaugeMeans) <- ~ X + Y

# break into independent regions
gauge_wca1a <- gaugeMeans[gaugeMeans$Region == "WCA1A", ]
gauge_wca2a <- gaugeMeans[gaugeMeans$Region == "WCA2A", ]
gauge_wca2b <- gaugeMeans[gaugeMeans$Region == "WCA2B", ]
gauge_wca3a <- gaugeMeans[gaugeMeans$Region == "WCA3A", ]
gauge_wca3b <- gaugeMeans[gaugeMeans$Region == "WCA3B", ]
gauge_np <- gaugeMeans[gaugeMeans$Region == "NP", ]


head(gauge_wca2a)
gauge_wca1a@data
#-------------------------------------------------------
# PREPARE EDEN GRID DATA (FOR USE AS 'NEWDATA')

edenGrid <- read.csv(edenFile, header = TRUE)
head(edenGrid)
names(edenGrid)[2:3] <- c("X", "Y") # must have same names!

coordinates(edenGrid) <- ~ X + Y

# break into independent regions
eden_wca1a <- edenGrid[edenGrid$Zone == "WCA1A", ]
eden_wca2a <- edenGrid[edenGrid$Zone == "WCA2A", ]
eden_wca2b <- edenGrid[edenGrid$Zone == "WCA2B", ]
eden_wca3a <- edenGrid[edenGrid$Zone == "WCA3A", ]
eden_wca3b <- edenGrid[edenGrid$Zone == "WCA3B", ]
eden_np <- edenGrid[edenGrid$Zone == "NP", ]
head(eden_np)

#-------------------------------------------------------
# first attempt at radial basis function
# FIRST NEED:
# 1) eta = optimal smoothing parameter
# 2) rho = optimal robustness parameter
# tried ranges 0-5 for eta and rho for wca1a, didn't seem to change anything
# Arc's geostatistical wizard calculates "0" as the best optimization parameter
# 3) n.neigh = no. of nearest observations used for predictions
# seems like the more observations the smoother the plot
# tried using 5, 8, and 10, 8 seemed to work best

# with each region
wca1a.rbf <- rbf(stage_cm ~ X + Y, data = gauge_wca1a, func = "M", eta = p_eta, rho = p_rho, 
                 n.neigh = p_neigh, newdata = eden_wca1a) 
# show prediction map
#coordinates(wca1a.rbf) <- ~ x + y
#gridded(wca1a.rbf) <- TRUE
#spplot(wca1a.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
#       main = "wca1a Stage", key.space=list(space="right", cex=0.8))


wca2a.rbf <- rbf(stage_cm ~ X + Y, data = gauge_wca2a, func = "M", eta = p_eta, rho = p_rho, 
                 n.neigh = p_neigh, newdata = eden_wca2a) 
# show prediction map
#coordinates(wca2a.rbf) <- ~ x + y
#gridded(wca2a.rbf) <- TRUE
#spplot(wca2a.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
#       main = "wca2a Stage", key.space=list(space="right", cex=0.8))

wca2b.rbf <- rbf(stage_cm ~ X + Y, data = gauge_wca2b, func = "M", eta = p_eta, rho = p_rho, 
                 n.neigh = p_neigh, newdata = eden_wca2b) 
# show prediction map
#coordinates(wca2b.rbf) <- ~ x + y
#gridded(wca2b.rbf) <- TRUE
#spplot(wca2b.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
#       main = "wca2b Stage", key.space=list(space="right", cex=0.8))

wca3a.rbf <- rbf(stage_cm ~ X + Y, data = gauge_wca3a, func = "M", eta = p_eta, rho = p_rho, 
                 n.neigh = p_neigh, newdata = eden_wca3a) 
# show prediction map
#coordinates(wca3a.rbf) <- ~ x + y
#gridded(wca3a.rbf) <- TRUE
#spplot(wca3a.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
#       main = "wca3a Stage", key.space=list(space="right", cex=0.8))

wca3b.rbf <- rbf(stage_cm ~ X + Y, data = gauge_wca3b, func = "M", eta = p_eta, rho = p_rho, 
                 n.neigh = p_neigh, newdata = eden_wca3b) 
# show prediction map
#coordinates(wca3b.rbf) <- ~ x + y
#gridded(wca3b.rbf) <- TRUE
#spplot(wca3b.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
#       main = "wca3b Stage", key.space=list(space="right", cex=0.8))

np.rbf <- rbf(stage_cm ~ X + Y, data = gauge_np, func = "M", eta = p_eta, rho = p_rho, 
              n.neigh = p_neigh, newdata = eden_np) 
# show prediction map
#coordinates(np.rbf) <- ~ x + y
#gridded(np.rbf) <- TRUE
#spplot(np.rbf["var1.pred"], cuts=40, col.regions=bpy.colors(100),
#       main = "np Stage", key.space=list(space="right", cex=0.8))

#------------------------------------------------------------------------------
# MERGE TOGETHER AND EXPORT

eden_rbf <- rbind(np.rbf, wca1a.rbf, wca2a.rbf, wca2b.rbf, wca3a.rbf, wca3b.rbf)

# add cell id column
eden_df <- read.csv(edenFile, header = TRUE)
eden_rbf <- left_join(eden_rbf, eden_df, by = c("x" = "x_centroid", "y" = "y_centroid"))
head(eden_rbf)
nrow(eden_rbf)
# can export eden_rbf here as data frame with EDEN cell id, if desired
setwd(home_folder)
file_name <- paste(rbf_output, "csv", sep = ".")
write.csv(eden_rbf, file_name, row.names = FALSE)

#------------------------------------------------------------------------------
# CONVERT TO RASTER FORMAT

altEden <- eden_rbf
coordinates(altEden) <- ~ x + y
proj4string(altEden) <- CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
gridded(altEden) <- TRUE
altEden <- raster(altEden)
plot(altEden)
altEden

# export if desired
setwd(home_folder)
raster_name <- paste(rbf_output, "tif", sep = ".")
writeRaster(altEden, raster_name)

#------------------------------------------------------------------------------
# READ IN USGS EDEN SURFACE INTERPOLATION & CREATE DIFFERENCES RASTER
# AND ASSOCIATED SUMMARY DATA

eden <- raster(eden_tiff)
plot(eden)

edens_diff <- altEden - eden
plot(edens_diff)
edens_diff

# summary stats
cellStats(edens_diff, 'mean')
cellStats(edens_diff, 'sd')

# export
raster_name <- paste(rbf_output, "_diff", ".tif", sep = "")
writeRaster(edens_diff, raster_name)



