## For testing eden_v3b::eden_nc()
source("./Scripts/eden_v3b.R")
library (RCurl)

canal <- readOGR("./GIS/eden_grid_canals/")
canal <- canal@data[, c("X_COORD", "Y_COORD")]
radius <- 0
for (i in 1:dim(canal)[1]) {
  xpos <- which(x == canal$X_COORD[i])
  ypos <- which(y == canal$Y_COORD[i])
  #  v2[(xpos - radius):(xpos + radius), (ypos - radius):(ypos + radius), ] <- v3[(xpos - radius):(xpos + radius), (ypos - radius):(ypos + radius), ] <- NA
}


# Desired date range
# realtime
#date_range <- seq(Sys.Date() - 4, Sys.Date() - 1, "days")
# arbitrary
for (i in 7:11)
  for (j in 1:4) {
    if (i == 7 & j == 1) next
    if (i == 11 & j == 4) break
    yr <- 2000 + i
    foq <- switch(j, "1" = "01", "2" = "04", "3" = "07", "4" = "10")
    loq <- switch(j, "1" = "03-31", "2" = "06-30", "3" = "09-30", "4" = "12-31")
    date_range <- seq(as.Date(paste(yr, foq, "01", sep = "-")), as.Date(paste(yr, loq, sep = "-")), "days")

    output_file <- paste0("./Output/Stage/edenV3_", yr, "_q", j, ".nc")

    eden_nc(date_range, output_file, "database")

    ftpUpload(output_file, paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/BryanSaira_testing/edenV3_bryan/edenV3_", yr, "_q", j, ".nc"))
    download.file(paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/BryanSaira_testing/edenV2_netcdfs/", yr, "_q", j, ".nc"), paste0("./Validation/", yr, "_q", j, ".nc"))

    stage.nc <- nc_open(output_file)
    v3 <- ncvar_get(stage.nc, "stage")
    nc_close(stage.nc)
    stage.nc<-nc_open(paste0("./Validation/", yr, "_q", j, ".nc"))
    v2 <- ncvar_get(stage.nc, "stage")
    x <- ncvar_get(stage.nc, "x")
    y <- ncvar_get(stage.nc, "y")
    nc_close(stage.nc)
    gages <- read.csv("./Output/edenmaster.csv", colClasses = c("character", "NULL", "numeric", "numeric", rep("NULL", 8)))
    pgages <- gages[gages$Station == "BCA19+" | gages$Station == "NP202" | gages$Station == "S12D_DN" | gages$Station == "S10D_DN" | gages$Station == "S151+H" | gages$Station == "S333-H" | gages$Station == "MO-214" | gages$Station == "NESRS1", ]
    ps <- gages[1:9, ]
    gages <- gages[-(1:9), ]

    v3 <- v3[, 405:1, ]

    diff <- v3 - v2
    print(paste(yr, j, range(diff, na.rm=T)))
    bin <- 5
    bigdiff <- diff
    bigdiff[abs(bigdiff) < bin] <- 0
    for (k in 1:length(date_range)) {
      png(paste0("./Validation/", date_range[k], ".png"), width = 2400, height = 3400, type = "quartz")
      par(mfrow = c(2, 2))
      image(v3[, , k], main = "EDEN V3", xaxt = "n", yaxt = "n")
      image(v2[, , k], main = "EDEN V2", xaxt = "n", yaxt = "n")
      image(diff[, , k], main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")
      image(x, y, bigdiff[, , k], main = paste0("Difference >", bin, " cm"), xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = c(heat.colors(12)[1:6], "#DDDDDDFF", heat.colors(12)[7:12]), breaks = c(-500, -30, -25, -20, -15, -10, -bin, bin, 10, 15, 20, 25, 30, 500))
      points(canal$X_COORD, canal$Y_COORD, pch = 16, col = "green", cex = 0.5)
      points(gages$x_nad83_utm17n, gages$y_nad83_utm17n, pch = 16, cex = 0.5)
      text(pgages$x_nad83_utm17n, pgages$y_nad83_utm17n, pgages$Station, cex = 0.75, pos = 1)
      points(ps$x_nad83_utm17n, ps$y_nad83_utm17n, pch = 16, col = "blue", cex = 0.5)
      text(ps$x_nad83_utm17n, ps$y_nad83_utm17n, ps$Station, cex = 0.75, pos = 4)
      dev.off()
      ftpUpload(paste0("./Validation/", date_range[k], ".png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/BryanSaira_testing/edenV3_bryan/diff_images/", date_range[k], ".png"))
    }
  }


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
