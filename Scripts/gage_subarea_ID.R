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

merge_gages$x_nad83_utm17n <- round(merge_gages$x_nad83_utm17n, digits = 1)
merge_gages$y_nad83_utm17n <- round(merge_gages$y_nad83_utm17n, digits = 1)

# -----------------------------------------------------------------------------
# Add column for each of the subarea & classify
# Selection criteria were found in RBFInterpolation.py
# As well as determined manually by examining output in development for EDEN_v3

# Note: Some gages are classified into more than one subarea

merge_gages[, c("wca1", "wca2a", "wca2b", "wca3a", "wca3b","l67ext", "pw", "other") := 0]

merge_gages[EArea == "Water Conservation Area 1" | 
            EArea == "L39 Canal" | 
            EArea == "L40 Canal", 
            wca1 := 1]

merge_gages[EArea == "Water Conservation Area 2A" | 
            EArea == "L6 Canal" |
            Station == "S7-T", 
            wca2a := 1] 

merge_gages[EArea == "Water Conservation Area 2B" | 
            EArea == "L38E Canal" & 
            Station != "S7-T", 
            wca2b := 1]

merge_gages[EArea == "Water Conservation Area 3A" | 
            EArea == "Miami Canal" |
            EArea == "Tamiami Canal" |
            Station == "L28S1+" |
            Station == "L28S2+" |
            Station == "S140M-H" |
            Station == "EDEN_6",
            wca3a := 1]

merge_gages[EArea == "Water Conservation Area 3B" & 
            Station != "S9A-T", 
            wca3b := 1] 

merge_gages[EArea == "Pennsuco Wetlands", 
            pw := 1] 


# Other has to be done before L67-extension
merge_gages[rowSums(merge_gages[, 5:11], na.rm = TRUE) < 1, ]$other <- 1

# Add gages to 'other' that are also in WCA3A
merge_gages[Station == "EDEN_6" |
            Station == "3ASW+" |
            Station == "L28S1+" |
            Station == "L28S2+",
            other := 1]

# Remove gages from 'other' gages that will be in L67-ext
# Remove S9A-T because EDEN_V2 doesn't use it
merge_gages[Station == "NESRS1" |
            Station == "NESRS2" |
            Station == "G-3576" |
            Station == "G-3574" |
            Station == "S9A-T",
            other := 0]


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

# -----------------------------------------------------------------------------
# Take a look at the file & export

colSums(merge_gages[, 5:12])
nrow(merge_gages)

write.csv(merge_gages, "./Output/gage_subareaID_21June2018.csv", row.names = FALSE)
  # Output from 6 June 2018 uses EDEN data downloaded from the website on 28 Feb 2018
  # Output from 21 June 2018 uses EDEN data downloaded from the website on 28 Feb 2018
