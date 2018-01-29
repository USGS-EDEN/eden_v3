## This code runs along ever4cast_sims.R so as to be able to run just one 
# simulation and a few days at a time (for testing & experimenting)

library(data.table)

output_folder <- "./Output/testing_"


sims <- sims[1]
sim_names <- sim_names[1]


sim_list[[1]] <- sim_list[[1]][Date <= 20180103, ]

### For running eden_v3.R
source("./Scripts/eden_v3.R")
test_gages <- fread("../EDEN/gage_data/2007_q3_DM_v2r1/20070701_median.txt")
v3 <- interpolate_gauges(test_gages)
v3
v3 <- interpolate_gauges(test_gages, format = "df")
v3
v3 <- interpolate_gauges(test_gages, format = "raster")
v3
plot(v3)
