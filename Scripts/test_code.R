source("./Scripts/eden_v3b.R")

## For testing eden_v3b::eden_nc()

# Desired date range
# realtime
#dt <- seq(Sys.Date() - 4, Sys.Date() - 1, "days")
# arbitrary
dt <- seq(as.Date("2018-07-01"), as.Date("2018-07-02"), "days")

netcdf_output <- "./Output/Stage/eden_2018q3.nc"

eden_nc(dt, netcdf_output, "database")

stage.nc<-nc_open(netcdf_output)
v3<-ncvar_get(stage.nc, "stage")
nc_close(stage.nc)
stage.nc<-nc_open("./Validation/2018_q3_v2rt.nc")
v2<-ncvar_get(stage.nc, "stage")
x<-ncvar_get(stage.nc, "x")
y<-ncvar_get(stage.nc, "y")
nc_close(stage.nc)

v3 <- v3[, 405:1, ]

canal <- readOGR("./GIS/eden_grid_canals/")
canal <- canal@data[, c("X_COORD", "Y_COORD")]
radius <- 0
for (i in 1:dim(canal)[1]) {
  xpos <- which(x == canal$X_COORD[i])
  ypos <- which(y == canal$Y_COORD[i])
  v2[(xpos - radius):(xpos + radius), (ypos - radius):(ypos + radius), ] <- v3[(xpos - radius):(xpos + radius), (ypos - radius):(ypos + radius), ] <- NA
}

diff <- v3 - v2
print(range(diff, na.rm=T))
bin <- 10
bigdiff <- diff
bigdiff[bigdiff < bin] <- NA
for (i in 1:length(dt)) {
  png(filename = paste0("./Validation/", dt[i], ".png"), width = 1200, height = 1700, type = "quartz")
  par(mfrow = c(2, 2))
  image(v3[, , i], main = "EDEN V3", xaxt = "n", yaxt = "n")
  image(v2[, , i], main = "EDEN V2", xaxt = "n", yaxt = "n")
  image(diff[, , i], main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")
  image(bigdiff[, , i], main = paste0("Difference >", bin, " cm"), xaxt = "n", yaxt = "n")
  dev.off()
}

for (i in 1:length(dt)) {
  s <- raster(netcdf_output, i)
  t <- raster("./Validation/2018_q3_v2rt.nc", i)
  dif <- s - t
  bdif <- dif
  bdif[bdif < bin] <- NA
  png(filename = paste0("./Validation/", dt[i], "_raster.png"), width = 1200, height = 1700, type = "quartz")
  par(mfrow = c(2, 2))
  plot(s, main = "EDEN V3", xaxt = "n", yaxt = "n")
  plot(t, main = "EDEN V2", xaxt = "n", yaxt = "n")
  plot(dif, main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")
  plot(bdif, main = paste0("Difference >", bin, " cm"), xaxt = "n", yaxt = "n")
  dev.off()
}
