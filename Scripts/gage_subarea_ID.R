# -----------------------------------------------------------------------------
# Create an ID file that matches each gage in the interpolation to a subarea

# Using: gage data downloaded from https://sofia.usgs.gov/eden/data_download.php
# And the subarea selection method developed in gauge_prep.R and eden_v3.R

# Saira Haider
# shaider@usgs.gov
# -----------------------------------------------------------------------------

library(data.table)
options(digits = 10)

# -----------------------------------------------------------------------------
# Import list of gages from EDEN website

gages <- fread("./Data/Gages/all_gages_from_eden_website_28Feb2018/1519851774station_data.txt")
# Using the downloaded gage file from the website as the base
# So if changes are made in the future to names/coordinates,
#     simply download a new updated one

# Select gages that are used in EDEN surfacing inteprolation
colnames(gages)
gages <- gages[`Station Used in Surfacing Program?` == "Yes", ]

# -----------------------------------------------------------------------------
# Look at differences between previous ID to EArea file and EDEN list

earea_gages <- fread("./Output/gauge_name_EArea_id.csv")
merge_gages <- merge(earea_gages, gages, 
                     by.x = "ever4cast_gauge_name", 
                     by.y = "EDEN Station Name", 
                     all = TRUE)
# 231 gages breakdown:
# 220 gages used in EDEN_v2 interpolation (includes 15 not used in Ever4Cast)
#   5 pseudo-gages hand-created for EDEN_v3
#          because can't create canal-boundaries, as is done in v2
#          locations of these were drawn from v2 pseudo-boundaries9
#   2 gages that have been discontinued from the surface: WCA2E4 & WCA2F1
#          keeping them because these are still used in Ever4Cast
#   ADD PARAMETER IN FUNCTION TO SPECIFY THIS USE?? ------------------------------------------------------
#   4 gages that are found in the _Ex files:
#          pS12D_DN, pNP202NE1 (in two different locations), pBCA19+LO1
#          as are found in: 20170710_median_Ex

# -----------------------------------------------------------------------------
# Merge coordinates & make sure they're the same for common gages
# - This is for peace of mind to check against what's been used in the past

gages$`UTM E Zone 17N (m NAD83)` <- round(gages$`UTM E Zone 17N (m NAD83)`, 
                                          digits = 1)
gages$`UTM N Zone 17N (m NAD83)` <- round(gages$`UTM N Zone 17N (m NAD83)`,
                                          digits = 1)
merge_gages <- merge(earea_gages, gages, 
                     by.x = "ever4cast_gauge_name", 
                     by.y = "EDEN Station Name", 
                     all = TRUE)

merge_gages$`UTM E Zone 17N (m NAD83)` - merge_gages$x
merge_gages$`UTM N Zone 17N (m NAD83)` - merge_gages$y
# They match for a list of gages downloaded on 28Feb2018

# -----------------------------------------------------------------------------
# Add 'fake gages' to the downloaded gage dataset from the EDEN website

# There should be 9 'fake gages' not in the downloaded EDEN file 
#    (5 pseudos and 4 from _Ex files)
# These gages locations are used in the surface interpolation 


# Gages that are not in the downloaded EDEN file
merge_gages[is.na(`UTM E Zone 17N (m NAD83)`), ]

# Add coordinates from 9 'fake gages' to the set from EDEN website
merge_gages[is.na(`UTM E Zone 17N (m NAD83)`), `UTM E Zone 17N (m NAD83)` := x]
merge_gages[is.na(`UTM N Zone 17N (m NAD83)`), `UTM N Zone 17N (m NAD83)` := y]

# Remove now redundant coordinates that come from the old gage_ID file
merge_gages[, c("x", "y") := NULL]

# -----------------------------------------------------------------------------
# Add EArea classification for 15 gages used in EDEN interpolation but not Ever4Cast

# Check that EArea and Location Area are the same
cbind(merge_gages$EArea, merge_gages$`Location Area`)

# Look at where they are not the same
same_labels <- which(merge_gages$EArea == merge_gages$`Location Area`)
merge_gages[!same_labels]
  # Only not the same for the 9 'fake gages' and 15 additional gages

# Add the Location Area classification to EArea for the 15 additional gages
merge_gages[is.na(EArea), ]
merge_gages[is.na(EArea), EArea := `Location Area`]

# Remove now redundant Location Area column
merge_gages[, `Location Area` := NULL]

# -----------------------------------------------------------------------------
# Add EArea classification for pseudo-gages
# This classification was determined in development for EDEN_v3

merge_gages[EArea == "", ]

merge_gages[Station == "pseudo_canal_1", EArea := "Water Conservation Area 2A"]

merge_gages[Station == "pseudo_canal_2" | 
            Station == "pseudo_canal_3" |
            Station == "pseudo_canal_4" |
            Station == "pseudo_canal_5", 
            EArea := "Water Conservation Area 3A"]

# -----------------------------------------------------------------------------
# Add Station names for 15 additional gauges

merge_gages[is.na(Station), ]

merge_gages[is.na(Station), Station := ever4cast_gauge_name]
merge_gages[, ever4cast_gauge_name := NULL]

# -----------------------------------------------------------------------------
# Do a little cleaning

merge_gages[, `Real-Time Daily Data Available` := NULL]
merge_gages[, `Station Used in Surfacing Program?` := NULL]

colnames(merge_gages)
colnames(merge_gages)[3:4] <- c("x_nad83_utm17n", "y_nad83_utm17n")

# -----------------------------------------------------------------------------
# Add column for each of the subarea & classify
# Selection criteria were found in RBFInterpolation.py
# As well as determined manually by examining output in development for EDEN_v3

# Note: Some gages are classified into more than one subarea

merge_gages[EArea == "Water Conservation Area 1" | 
            EArea == "L39 Canal" | 
            EArea == "L40 Canal", 
            wca1 := 1]

merge_gages[EArea == "Water Conservation Area 2A" | 
            Station == "S7-T", 
            wca2a := 1] 

merge_gages[EArea == "Water Conservation Area 2B" | 
            EArea == "L38E Canal" & 
            Station != "S7-T", 
            wca2b := 1]

merge_gages[EArea == "Water Conservation Area 3A" | 
            EArea == "Miami Canal" |
            EArea == "L28 Canal" |
            EArea == "Tamiami Canal" |
            EArea == "L28 Interceptor Canal" |
            Station == "EDEN_6",
            wca3a := 1]

merge_gages[EArea == "Water Conservation Area 3B" & 
            Station != "S9A-T", 
            wca3b := 1] 

merge_gages[Station == "NESRS1" | 
            Station == "NESRS2" |
            Station == "G-3578" | 
            Station == "G-3577" |
            Station == "G-3575" | 
            Station == "G-3576" |
            Station == "G-3574" | 
            Station == "G-3272" |
            Station == "G-3273" | 
            Station == "ANGEL.S" |
            Station == "G-596"| 
            Station == "NESRS4" |
            Station == "pNP202NE1" | 
            Station == "G-3626" |
            Station == "G-3628" | 
            Station == "G-3437" |
            Station == "RG1" | 
            Station == "RG3" |
            Station == "RG2" | 
            Station == "NP206" |
            Station == "P33" | 
            Station == "NP202" |
            Station == "S334-H" | 
            Station == "S333-T" | 
            Station == "NP203", 
            l67ext := 1]

merge_gages[EArea == "Pennsuco Wetlands", 
            pw := 1] 

# STOPPED HERE
merge_gages[Station == "EDEN_6" |
            Station == "3ASW+" |
            Station == "L28S1+" |
            Station == "L28S2+",
            other := 1]

# Add something here that sums up the subzone columns, if == 0, then other = 1
# Still need to double check that sorting is done correctly
# And need to go through notebook and catch ones that are in more than one subzone


# first: create the 'other' subzone as usual 
# (the one ifelse command has this part: & gages$Station != "pBCA19+LO1" could try running it that way)
merge_gages[EArea != "Water Conservation Area 1" &
            EArea != "L39 Canal" &
            EArea != "L40 Canal"&
            EArea != "Water Conservation Area 2B" &
            EArea != "L38E Canal" &
            EArea != "Water Conservation Area 3B" &
            EArea != "Pennsuco Wetlands" &
            EArea != "Water Conservation Area 2A" &
            Station != "S7-T" &
            EArea != "L30 Canal" &
            Station != "S31M-H" |
            Station == "S9A-T", ]
# second, remove certain gages east of l67-ext that could influence 'other' surface
other_gages <- other_gages[other_gages$Station != "NESRS1" & other_gages$Station != "NESRS2" &
                             other_gages$Station != "G-3576" & other_gages$Station != "G-3574" &
                             other_gages$Station != "S334-H" & other_gages$Station != "S334-T" &
                             other_gages$Station != "S333-T", ]
#third, remove gages that are in WCA3A
other_gages <- other_gages[other_gages$EArea != "Water Conservation Area 3A" &
                             other_gages$EArea != "Tamiami Canal" & 
                             other_gages$EArea != "Miami Canal" |
                             other_gages$Station == "3ASW+", ]
