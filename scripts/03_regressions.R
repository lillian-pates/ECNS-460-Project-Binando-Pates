############################################################
# Montana State University 
# Project: ECNS 460 Data Analytics Spring 2026
# Author: Bailey Binando & Lillian Pates
# Created: 2026-04-01
# Last Updated: 2026-04-05

# Script 3 
# Output: Basic Visuals & Regression Analysis 
############################################################
# Preliminary: Libraries 

library(sf)
library(tidyverse)
library(tmap)
library(terra)

############################################################
# Preliminary: Data  

## Main 
dem_pz = rast("data/processed/dem_pozarica.tif")
rivers = st_read("data/processed/river_clipped.gdb")
flooding = read_csv("data/processed/flood_timeseries_clean.csv")


## For Analysis
population = st_read("data/processed/poza_rica.gpkg")
buildings = st_read("data/processed/buildings_clipped.gpkg")
key_infrastructure = read_csv("data/processed/poza_rica_key_infrastructure.csv")

## Flood Plains 
flood_plain2yr = st_read("data/processed/floodplain_2yr.gpkg")
flood_plain5yr =st_read("data/processed/floodplain_5yr.gpkg")
flood_plain10yr = st_read("data/processed/floodplain_10yr.gpkg")
flood_plain25yr = st_read("data/processed/floodplain_25yr.gpkg")
flood_plain50yr = st_read("data/processed/floodplain_50yr.gpkg")
flood_plain100yr = st_read("data/processed/floodplain_100yr.gpkg")


############################################################
# Deriving Slope, Aspect, Hill from DEM Model 

slope = terrain(dem_pz, v = "slope", unit = "degrees")

aspect = terrain(dem_pz, v = "aspect")

hill = shade(
  terrain(dem_pz, "slope"),
  terrain(dem_pz, "aspect")
)

############################################################
# Preliminary Visuals 




############################################################
# Data Behavior 




############################################################
# Regressions 



############################################################

