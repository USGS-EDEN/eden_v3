# EDEN v3 - EDEN in R

EDEN v3 is an attempt to reproduce the EDEN interpolation, currently written in ArcPy for use with ESRI's ArcGIS 9, in the R environment.

## Description of ./Scripts

- **eden\_v3:**  Contains a function for running the EDEN water surface interpolation for one day
- **ever4cast\_sims:** Uses eden\_v3.R to create surfaces for the Ever4Cast project
- **ever4cast\_sims\_testcode:** Used only when developing scripts
- **gage\_subarea\_ID**: Creates an ID file that matches each EDEN gage to a subarea used for the interpolation. Created ./Output/gage\_subareaID\_6June2018.csv"
- **gauge\_prep:** Older script for creating a different type of EDEN gauge ID file. Produced ./Output/gauge\_name\_EArea.id.csv, which is used in gage\_subarea\_ID.R
- **historical\_mean\_sd:** Creates a raster of the mean adnd standard deviation of the historical EDEN v3 dataset.  Used for validation.
- **historical\_surfaces:** Runs eden\_v3.R over the time period Jan 1 2007 - Sept 30 2017 and produces both raster and csv output. Used for validation.
- **historical\_validation:** Finds the differences between EDEN v3 and EDEN v2 for a set historical preiod. Used for validation.
- **validation:** Validation of EDEN surface using benchmarks (field measurements)
- **validation2:** Validation of EDEN surface using 2nd set of benchmarks (field measurements)
- **werp\_depth:** Find depth using the new DEM from WERP

## Description of ./Output

- **gage\_subareaID\_6June2018:** Created by ./Scripts/gage\_subarea\_ID.R.  Used in eden\_v3.R to assign each gage to an interpolation subarea
- **gauge\_name\_EArea\_id:** Created by ./Scripts.gauge\_prep.R.  Formerly used to assign each gage to an interpolation subarea.  Now used to create the new subareaID csv. 

## Description of ./GIS/V3\_subzones
- Shapefiles of the subareas where the independent interpolations are run. 