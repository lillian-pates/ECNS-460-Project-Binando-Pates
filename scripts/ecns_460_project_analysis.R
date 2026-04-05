############################################################
# Montana State University 
# Project: ECNS 460 Data Analytics Spring 2026
# Author: Bailey Binando & Lillian Pates
# Created: 2026-04-01
# Last Updated: 2026-04-

# Research Question: 
# Output: 
############################################################
# Preliminary: Libraries 

library(sf)
library(tidyverse)
library(tmap)
library(terra)

############################################################
# Preliminary: Data  

## Area 
cazones_basin = rast("data/raw/cazones_basin_dem.tif")
entire_dem = rast("data/raw/dem_model_whole.tif")

## Analysis
population = rast("data/raw/mex_pop_2026_CN_100m_R2025A_v1.tiff")
buildings = st_read("data/raw/microsoft_buildings_pozarica.gpkg")
key_infrastructure = read_csv()

## Rivers, Water specific data 
rivers = st_read("data/raw/HydroRIVERS_v10_na.gdb")
flow_direction = rast("data/raw/floodplains_HAND/flow_dir.tif")
streams = rast("data/raw/floodplains_HAND/streams.tif")
  
## Flood Plains 
flood_plain2yr = st_read("data/raw/floodplains_HAND/floodplain_HAND_2yr.shp")
flood_plain5yr = st_read("data/raw/floodplains_HAND/floodplain_HAND_5yr.shp")
flood_plain10yr = st_read("data/raw/floodplains_HAND/floodplain_HAND_10yr.shp")
flood_plain25yr = st_read("data/raw/floodplains_HAND/floodplain_HAND_25yr.shp")
flood_plain50yr = st_read("data/raw/floodplains_HAND/floodplain_HAND_50yr.shp")
flood_plain100yr = st_read("data/raw/floodplains_HAND/floodplain_HAND_100yr.shp")

floodplains_all = st_read("data/raw/floodplains_HAND/floodplains_HAND_all.geojson")
  
## Poza Rica Extent 
ext_pr = ext(-98.0, -97.1, 20.2, 20.9)


############################################################
# Trimming to files for just Poza Rica analysis 

dem_pr = crop(entire_dem, ext_pr)

############################################################
# Deriving Slope, Aspect, Hill from DEM Model 

slope = terrain(dem_pr, v = "slope", unit = "degrees")

aspect = terrain(dem_pr, v = "aspect")

hill = shade(
  terrain(dem_pr, "slope"),
  terrain(dem_pr, "aspect")
)

############################################################
# Preliminary Visuals 





############################################################
# Data Behavior 




############################################################
# Regressions 



############################################################
# Exploratory Analysis  



############################################################
