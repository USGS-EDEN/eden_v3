### Set to containing directory of script!
setwd("~/Desktop/eden_V3")
dir <- getwd()

source("./Scripts/eden_v3b.R")

## For testing eden_v3::eden_nc()

# Desired date range
# realtime
#dt <- seq(Sys.Date() - 4, Sys.Date() - 1, "days")
# arbitrary
dt <- seq(as.Date("2018-07-01"), as.Date("2018-07-01"), "days")

netcdf_output <- "./Output/Stage/eden_2018q3.nc"


eden_nc(dt, netcdf_output, "database")


stage.nc<-nc_open(netcdf_output)
x<-ncvar_get(stage.nc,"x")
y<-ncvar_get(stage.nc,"y")
new<-ncvar_get(stage.nc,"stage")
nc_close(stage.nc)
stage.nc<-nc_open("~/Downloads/2018_q3_v2rt.nc")
old<-ncvar_get(stage.nc,"stage")
nc_close(stage.nc)

new <- new[,405:1]
par(mfrow = c(2, 2))
image(new, main = "EDEN V3", xaxt = "n", yaxt = "n")
image(old, main = "EDEN V2", xaxt = "n", yaxt = "n")
diff<-new-old
range(diff, na.rm=T)
image(diff, main = "EDEN V3 - V2", xaxt = "n", yaxt = "n")

s <- raster(netcdf_output)
plot(s)
t <- raster("~/Downloads/2018_q3_v2rt.nc")
plot(t)
plot(s-t)
