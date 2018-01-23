# -----------------------------------------------------------------------------
# (1) Checks that the median values for the EDEN gauges are the same in two files
#     file 1: the median file of stage values from the sofia website
#     file 2: the median file of stage values from Brian/Heather, that includes the pseudo-canals
#             this is the file that the EDEN python script uses to interpolate
#             so we want the median file from sofia to match this


# (2) Creates and ID with gauge coordinates, gauge name from EX file, & EArea
#     Uses list of EVER4CAST gauges to determine which gauges to include
#     Necessary to create this ID which can be merged with any 'sofia' medians 
#        file or any EVER4CAST simulation, based on x, y coordinates
#     Because interpolation subzone script is based on 'EX' file gauge names & 
#       EArea designation, not sofia gauge names or gauge coordinates


# (3) Keeps rows for the locations of the 4 gauges that aren't actually gauges
        # the p[something] that have the exact same values as other gauges


# (4) Adds rows for the locations of the 5 pseudo-gauges

# OUTPUT: csv with... 
#                  COLUMNS: 
#                  x utm coordinate 
#                  y utm coordinate 
#                  gauge name from EX file (EDEN names used in py script)
#                  EArea 
#                  gauge name used in EVER4CAST simulations

#                  NEW ROWS:
#                  4 gauges that aren't pseudo gauges or real gauges (the p[x])
#                  5 pseudo-gauges that are used in the interpolations


# Saira Haider 
# Romanach Lab @ Wetland and Aquatic Research Center 
# US Geological Survey

# Last update: November 3rd 2017
# -----------------------------------------------------------------------------

library(data.table)
options(digits = 10)

# -----------------------------------------------------------------------------

#### SAVE PSEUDO-CANAL LOCATIONS AS SEPARATE DATATABLE ####---------------------

# gauges data from 'extended' file (only possible to get this through EDEN team)
(ex <- fread("../fromHeather/20170710_median_Ex.txt"))

# before removing non-gauges, grab the locations of the pseudo-gauges for the
# interpolation script
(pseudos <- ex[FID == "856" | FID == "830" | FID == "772" | FID == "783" | FID == "783" | FID == "564", 
               c("X", "Y", "Station")])
pseudos$Station <- paste0("pseudo_canal_", seq(1, 5, by = 1))
colnames(pseudos)[1:2] <- c("x", "y")
pseudos

# clean up datatable
ex <- ex[Flg == 1]
ex$X <- round(ex$X, digits = 1)
ex$Y <- round(ex$Y, digits = 1)
# have to correct 1 gauge by hand because R isnt rounding it the same as EDEN/EVER4CAST rounds coords of this gauge
ex[Station == "S333-T"]$Y <- 2849335.3 

ex$MEDIAN <- round(ex$MEDIAN, digits = 0)
ex

#### CHECK THAT MEDIAN VALUES ARE THE SAME IN BOTH FILES #### -----------------

# gauge data available online from sofia website
sofia <- fread("../gage_data/20170710_median_flag_v2rt.txt")
sofia

# merge
dt <- sofia[ex, on = c("X", "Y")]
dt[is.na(Station),]  # these are the 4 p[x-name] gauges
dt


# look at differences in stage value
dt$diff <- dt$MEDIAN - dt$`Daily Median Water Level (cm, NAVD88)`
summary(dt$diff)
rm(dt, sofia)
## CLOSE ENOUGH ##

#### MAKE ID DATATABLE FOR EVER4CAST GAUGES #####------------------------------

# list of ever4cast gauges
ever <- fread("../gauge_prep/variability_metrics.csv")
ever <- ever[, c("gage", "x", "y")]
ever <- unique(ever)
colnames(ever)[1] <- "ever4cast_gauge_name"
ever

# merge ever4cast dt with 'EX' dt
ex <- ex[, c("X", "Y", "Station", "EArea")]
dt <- ever[ex, on = c("x" = "X", "y" = "Y")]
setorder(dt, EArea)
dt[is.na(ever4cast_gauge_name),]  # gauges in EDEN but not in EVER4CAST
dt

#### grab the 4 gauges that aren't exactly gauges
p_gauge_names <- grep("p", dt[is.na(ever4cast_gauge_name), Station], value = TRUE)
p_gauges <- dt[Station %in% p_gauge_names]
p_gauges

# check to see if we have all 207 EVER4CAST gauges
dt <- na.omit(dt)  # only 206 - this means there's a gauge in 'ever' that's not in 'ex'
# find that gauge
check <- ex[ever, on = c("X" = "x", "Y" = "y")]
check[is.na(Station),]
gname <- check[is.na(Station),]$ever4cast_gauge_name

# get the info for missing gauge and add it in
ex2 <- fread("../gage_data/extended_median_files/20160224_median_Ex.txt")
ex2 <- ex2[, c("X", "Y", "Station", "EArea")]
gname <- grep(gname, ex2$Station, value = TRUE)
wca2f1 <- ex2[Station == gname]
wca2f1$X <- round(wca2f1$X, digits = 1)
wca2f1$Y <- round(wca2f1$Y, digits = 1)
wca2f1
# merge with ever to get full set of columns
wca2f1 <- ever[wca2f1, on = c("x" = "X", "y" = "Y")]
colnames(dt)
# add onto ID datatable
dt <- rbind(dt, wca2f1)
dt


#### ADD IN 5 PSEUDO GAUGES ALONG CANALS & 4 SEMI-GAUGES -------------------

dt <- rbind(dt, pseudos, p_gauges, fill = TRUE)

###### EXPORT THE DATATABLE AS AN ID TABLE FOR USE IN INTERPOLATION SCRIPT -----
fwrite(dt, "../gauge_prep/gauge_name_EArea_id.csv")

