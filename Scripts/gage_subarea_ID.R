# -----------------------------------------------------------------------------
# Create an ID file that matches each gage in the interpolation to a subarea

# Using: gage data downloaded from https://sofia.usgs.gov/eden/data_download.php
# And the subarea selection method developed in gauge_prep.R and eden_v3.R

# Saira Haider
# shaider@usgs.gov
# -----------------------------------------------------------------------------

library(data.table)

# -----------------------------------------------------------------------------

gages <- fread("./Data/Gages/all_gages_from_eden_website_28Feb2018/1519851774station_data.txt")

# Select on gages that are used in EDEN surfacing inteprolation
colnames(gages)
gages <- gages[`Station Used in Surfacing Program?` == "Yes", ]

# Join to current file used to see differences
earea_gages <- fread("./Output/gauge_name_EArea_id.csv")

compare_gages <- earea_gages[gages, on = c("Station" = "EDEN Station Name")]
