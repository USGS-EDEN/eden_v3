## For testing eden_v3b::eden_nc()
source("./Scripts/eden_v3b.R")

# Desired date range
# realtime
#date_range <- seq(Sys.Date() - 4, Sys.Date() - 1, "days")
# arbitrary
date_range <- seq(as.Date("2018-07-01"), as.Date("2018-07-05"), "days")

canal <- readOGR("./GIS/eden_grid_canals/")
canal <- canal@data[, c("X_COORD", "Y_COORD")]

for (j in c(0.01, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 3, 5, 10, 30, 50))
  for (k in c(0.01, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 3, 5, 10, 30, 50))
  {    
output_file <- paste0("./Output/Stage/eden_2018q3_eta",j,"_rho",k,".nc")

eden_nc(date_range, output_file, "database", j, k)

stage.nc<-nc_open(output_file)
v3<-ncvar_get(stage.nc, "stage")
nc_close(stage.nc)
stage.nc<-nc_open("./Validation/2018_q3_v2rt.nc")
v2<-ncvar_get(stage.nc, "stage")
x<-ncvar_get(stage.nc, "x")
y<-ncvar_get(stage.nc, "y")
nc_close(stage.nc)

v3 <- v3[, 405:1, ]

radius <- 0
for (i in 1:dim(canal)[1]) {
  xpos <- which(x == canal$X_COORD[i])
  ypos <- which(y == canal$Y_COORD[i])
#  v2[(xpos - radius):(xpos + radius), (ypos - radius):(ypos + radius), ] <- v3[(xpos - radius):(xpos + radius), (ypos - radius):(ypos + radius), ] <- NA
}

diff <- v3 - v2
print(range(diff, na.rm=T))
bin <- 5
bigdiff <- diff
bigdiff[abs(bigdiff) < bin] <- 0
for (i in 1:length(date_range)) {
  png(filename = paste0("./Validation/", date_range[i], "_eta",j,"_rho",k,".png"), width = 2400, height = 3400, type = "quartz")
  par(mfrow = c(2, 2))
  image(v3[, , i], main = paste0("EDEN V3 eta ",j," rho ",k), xaxt = "n", yaxt = "n")
  image(v2[, , i], main = "EDEN V2", xaxt = "n", yaxt = "n")
  image(diff[, , i], main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")
  image(x, y, bigdiff[, , i], main = paste0("Difference >", bin, " cm"), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = c(heat.colors(12)[1:6], "#DDDDDDFF", heat.colors(12)[7:12]), breaks = c(-500, -30, -25, -20, -15, -10, -bin, bin, 10, 15, 20, 25, 30, 500))
  points(canal$X_COORD, canal$Y_COORD, pch = 16, col = "green", cex = 0.5)
  points(gages$utm_easting, gages$utm_northing, pch = 16, cex = 0.5)
  pgages<-gages[gages$station_name == "BCA19+" | gages$station_name == "NP202" | gages$station_name == "S12D_DN" | gages$station_name == "S10D_DN" | gages$station_name == "S151+H" | gages$station_name == "S333-H" | gages$station_name_web == "MO-214" | gages$station_name_web == "NESRS1", ]
  text(pgages$utm_easting, pgages$utm_northing, pgages$station_name_web, cex = 0.75, pos = 1)
  points(id$x_nad83_utm17n[1:9], id$y_nad83_utm17n[1:9], pch = 16, col = "blue", cex = 0.5)
  text(id$x_nad83_utm17n[1:9], id$y_nad83_utm17n[1:9], id$Station[1:9], cex = 0.75, pos = 4)
  dev.off()
}

for (i in 1:length(date_range)) {
  png(filename = paste0("./Validation/", date_range[i], "_sw_eta",j,"_rho",k,".png"), width = 2400, height = 3400, type = "quartz")
  par(mfrow = c(2, 2))
  image(v3[5:133, 25:202, i], main = "EDEN V3", xaxt = "n", yaxt = "n")
  image(v2[5:133, 25:202, i], main = "EDEN V2", xaxt = "n", yaxt = "n")
  image(diff[5:133, 25:202, i], main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")
  image(x[5:133], y[25:202], bigdiff[5:133, 25:202, i], main = paste0("Difference >", bin, " cm"), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = c(heat.colors(12)[1:6], "#DDDDDDFF", heat.colors(12)[7:12]), breaks = c(-500, -30, -25, -20, -15, -10, -bin, bin, 10, 15, 20, 25, 30, 500))
  points(canal$X_COORD, canal$Y_COORD, pch = 16, col = "green", cex = 0.5)
  points(gages$utm_easting, gages$utm_northing, pch = 16, cex = 0.5)
  points(id$x_nad83_utm17n[1:9], id$y_nad83_utm17n[1:9], pch = 16, col = "blue", cex = 0.5)
  dev.off()
}}

for (i in 1:length(date_range)) {
  s <- raster(output_file, i)
  t <- raster("./Validation/2018_q3_v2rt.nc", i)
  dif <- s - t
  bdif <- dif
  bdif[bdif < bin] <- NA
  png(filename = paste0("./Validation/", date_range[i], "_raster.png"), width = 1200, height = 1700, type = "quartz")
  par(mfrow = c(2, 2))
  plot(s, main = "EDEN V3", xaxt = "n", yaxt = "n")
  plot(t, main = "EDEN V2", xaxt = "n", yaxt = "n")
  plot(dif, main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")
  plot(bdif, main = paste0("Difference >", bin, " cm"), xaxt = "n", yaxt = "n")
  dev.off()
}
