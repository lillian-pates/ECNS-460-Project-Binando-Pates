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
rivers = st_read("data/processed/river_clipped.gpkg")
flooding = read_csv("data/processed/flood_timeseries_clean.csv")


## For Analysis
population = st_read("data/processed/poza_rica.gpkg")
buildings = st_read("data/processed/buildings_clipped.gpkg")
key_infrastructure = read_csv("data/processed/poza_rica_key_infrastructure.csv")
expos = st_read("data/processed/building_exposure_vulnerability.gpkg")


## Flood Plains 
flood_plain2yr = st_read("data/processed/floodplain_2yr.gpkg")
flood_plain5yr =st_read("data/processed/floodplain_5yr.gpkg")
flood_plain10yr = st_read("data/processed/floodplain_10yr.gpkg")
flood_plain25yr = st_read("data/processed/floodplain_25yr.gpkg")
flood_plain50yr = st_read("data/processed/floodplain_50yr.gpkg")
flood_plain100yr = st_read("data/processed/floodplain_100yr.gpkg")


## Flood events 
floods = read_csv("data/processed/flood_timeseries_clean.csv")
  

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


## Flood Plain 100 yr flood extent 
ggplot() +
  geom_sf(data = flood_plain100yr, fill = "#0A7398", color = NA, alpha = 0.6) +
  geom_sf(data = rivers, color = "#08306b", linewidth = 0.5) +
  theme_void() +
  labs(title = "Cazones River Flood Extent")

## Predicted river flow vs flood indications 
floods |>
  mutate(flow_bin = cut(pred_flow, breaks = 20)) |>
  group_by(flow_bin) |>
  summarise(prop_flood = mean(flood_flag),
            mid = mean(pred_flow)) |>
  ggplot(aes(x = mid, y = prop_flood)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  labs(y = "Proportion flooded", x = "Predicted flow")

## Actual river flow vs flood indications
floods |>
  mutate(flow_bin = cut(river_flow, breaks = 20)) |>
  group_by(flow_bin) |>
  summarise(prop_flood = mean(flood_flag),
            mid = mean(river_flow)) |>
  ggplot(aes(x = mid, y = prop_flood)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  labs(y = "Proportion flooded", x = "Predicted flow")

## Predicted River Flow vs Flood cause 
ggplot(floods, aes(x = pred_flow , y = cause)) +
  geom_jitter(height = 0.05, alpha = 0.3)




############################################################
# Data Behavior 




############################################################
# Regressions 



############################################################



protected_intersect = st_filter(population, flood_plain2yr) # default: st_intersects
protected_within = st_filter(population, flood_plain2yr, .predicate = st_within)
protected_intersection = st_intersection(population, flood_plain2yr)

tm_shape(flood_plain2yr) + tm_polygons() +
  tm_shape(protected_intersect) + tm_polygons(col = "green", alpha = 0.5) +
  tm_shape(protected_intersection) + tm_polygons(col = "red", alpha = 0.5)

schools_nad83 = st_transform(population, st_crs(flood_plain2yr))
schools_tract = st_join(schools_nad83, flood_plain2yr)

tmap_mode("view")
tm_shape(flood_plain2yr) + tm_polygons(col = 'white', border.col = 'black') +
  tm_shape(schools_tract) + tm_dots(col = 'GEOID')
